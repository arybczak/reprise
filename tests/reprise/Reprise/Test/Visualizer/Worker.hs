module Reprise.Test.Visualizer.Worker (visualizerWorkerTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.IORef.Strict qualified as S
import Data.Int
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import Effectful
import Effectful.Dispatch.Dynamic
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix.Files
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Config
import Reprise.Effect.Clock
import Reprise.Effect.Fifo
import Reprise.Event
import Reprise.Test.Visualizer.Helpers
import Reprise.Visualizer.Samples
import Reprise.Visualizer.Spectrum
import Reprise.Visualizer.Wave
import Reprise.Visualizer.Worker

visualizerWorkerTests :: TestTree
visualizerWorkerTests =
  testGroup
    "worker"
    [ testCase "every frame shows the samples of its time" test_playout
    , testCase "MPD stops writing" test_playoutStops
    , testCase "a short write" test_playoutShortWrite
    , testCase "the lag is bounded" test_playoutLag
    , testCase "the worker sends the samples of the fifo" test_worker
    , testCase "each frame sends the samples of its time" test_workerFrames
    , testCase "a late frame doesn't make the next ones late" test_workerLate
    , testCase "a dropped frame counts in its own second" test_workerDropped
    , testCase "the worker sends the spectrum" test_workerSpectrum
    , testCase "the spectrum stays until its window is silent" test_workerSilence
    , testCase "a switch shows the samples since it" test_workerSwitch
    , testCase "the worker sends the wave" test_workerWave
    , testCase "the worker reports a data source that it can't read" test_workerFails
    , testCase "a data source that isn't a fifo" test_workerRegularFile
    , testCase "the worker sends what happened to its frames" test_workerStats
    , testCase "the worker waits through a pause" test_workerPause
    , testCase "the worker waits for the first samples" test_workerFirstSamples
    , testCase "the worker stops waiting when the visualizer hides" test_workerHidden
    , testCase "the worker reads MPD's fifo again after MPD restarts" test_workerRestart
    , testCase "a wait on the fifo after its writer went" test_fifoWriterGone
    ]

-- | MPD writes 503 samples at a time, as the author's does, which is longer
-- than a frame at 120 frames a second. Once the buffer holds a frame and
-- two writes, every frame shows the samples of its time.
test_playout :: Assertion
test_playout = do
  let frames = simulate 1 Nothing Nothing
      steady = dropWhile (\f -> f.frame == 0) frames
  case steady of
    first : _ ->
      assertBool
        ("the first samples at " <> show first.time)
        (first.time <= 3 * writeSeconds + 1 / 120)
    [] -> assertFailure "no samples"
  assertEqual "every frame" [] [f | f <- steady, f.frame /= f.shown]
  assertEqual "no stop" [] [f | f <- steady, f.stopped]

-- | After MPD's last write, the frames show what the buffer holds, and MPD
-- stopped once two writes didn't come.
test_playoutStops :: Assertion
test_playoutStops = do
  let frames = simulate 1 (Just 1) Nothing
      later = dropWhile (\f -> f.time <= 1) frames
      stoppedAt = [f.time | f <- later, f.stopped]
  case stoppedAt of
    t : _ -> assertBool ("stopped at " <> show t) (t - 1 <= 2 * writeSeconds + 1 / 120)
    [] -> assertFailure "never stopped"
  assertEqual
    "nothing to show once the buffer ran out"
    []
    [f | f <- dropWhile (\f -> f.frame /= 0) later, f.frame /= 0]

-- | MPD's last write before a pause can be short, and come alone in a read.
-- After three writes the short one doesn't count, and the frames start as
-- at the start, once the buffer holds a frame and two writes. Then every
-- frame shows the samples of its time again, and MPD didn't stop.
test_playoutShortWrite :: Assertion
test_playoutShortWrite = do
  let resumed = 0.5 + 0.2
      recovered =
        dropWhile
          (\f -> f.time <= resumed + 6 * writeSeconds + 1 / 120)
          (simulate 1 Nothing (Just 0.5))
  assertEqual "every frame" [] [f | f <- recovered, f.frame /= f.shown]
  assertEqual "no stop" [] [f | f <- recovered, f.stopped]

-- | MPD's clock is a little faster than reprise's, so the buffer would
-- grow. It holds a frame and three writes at most.
test_playoutLag :: Assertion
test_playoutLag = do
  let frames = simulate 1.01 Nothing Nothing
  assertEqual
    "the lag"
    []
    [f | f <- frames, f.buffered > f.shown + 3 * writeBytes + frameBytes]

data Simulated = Simulated
  { time :: Double
  , shown :: Int
  , frame :: Int
  , stopped :: Bool
  , buffered :: Int
  }
  deriving stock (Eq, Show)

-- | Two seconds of frames at 120 a second, with MPD's writes of 503
-- samples, at a speed relative to the sound's, until a time, with a short
-- write of 10 samples at a time, before a pause of 0.2 s.
simulate :: Double -> Maybe Double -> Maybe Double -> [Simulated]
simulate speed stop short = go (newPlayout 0) 1
  where
    go :: Playout -> Int -> [Simulated]
    go p n
      | n > 2 * fps = []
      | otherwise =
          let t = frameTime n
              new = BS.replicate (sum (writesBetween (frameTime (n - 1)) t)) 1
              shown = frameBytes * (samplesUntil n - samplesUntil (n - 1))
              (frame, stopped, p') = playout shown t new p
          in Simulated
               { time = t
               , shown = shown
               , frame = BS.length frame
               , stopped = stopped
               , buffered = BS.length p'.buffered
               }
               : go p' (n + 1)

    -- The bytes of each write that comes between two times.
    writesBetween :: Double -> Double -> [Int]
    writesBetween t0 t1 =
      [b | (w, b) <- takeWhile ((<= t1) . fst) writes, w > t0, maybe True (w <=) stop]

    -- Each write with its time and its bytes. A short write is the last
    -- before a pause, and the writes go on after it.
    writes :: [(Double, Int)]
    writes = case short of
      Nothing -> from 0
      Just s -> takeWhile ((< s) . fst) (from 0) <> [(s, 10 * frameBytes)] <> from (s + pause)

    from :: Double -> [(Double, Int)]
    from t0 = [(t0 + fromIntegral i * writeSeconds / speed, writeBytes) | i <- [1 :: Int ..]]

    pause :: Double
    pause = 0.2

    frameTime :: Int -> Double
    frameTime n = fromIntegral n / fromIntegral fps

    samplesUntil :: Int -> Int
    samplesUntil n = n * 44100 `div` fps

    fps :: Int
    fps = 120

writeBytes :: Int
writeBytes = 503 * frameBytes

writeSeconds :: Double
writeSeconds = 503 / 44100

test_worker :: Assertion
test_worker = withSystemTempDirectory "visualizer" $ \dir -> do
  let path = dir </> "fifo"
  createNamedPipe path (unionFileModes ownerReadMode ownerWriteMode)
  events <- newTQueueIO
  reading <- newTVarIO (Just Ellipse)
  let pcm = samples (replicate 100 (1000, -1000))
  bracket (forkIO . realWorker path $ source reading events) killThread $ \_ -> do
    frame <- withWriting path pcm . expectWithin $ firstSamples events
    assertBool
      ("whole samples: " <> show (BS.length frame))
      (BS.length frame `mod` frameBytes == 0)
    assertBool "the samples" (frame `BS.isPrefixOf` BS.concat (replicate 100 pcm))
    atomically $ writeTVar reading Nothing
    threadDelay frameInterval
    void . atomically $ flushTQueue events
    threadDelay (2 * frameInterval)
    assertEqual "stopped" [] =<< atomically (flushTQueue events)
  where
    -- The frames are empty until the samples come.
    firstSamples :: TQueue AppEvent -> IO BS.ByteString
    firstSamples events =
      atomically (readTQueue events) >>= \case
        VisualizerSamples bytes | not (BS.null bytes) -> pure bytes
        _ -> firstSamples events

    frameInterval :: Int
    frameInterval = 1000000 `div` 60

-- | The buffer holds a frame and the writes ahead before the frames show
-- samples. The frames before show nothing, so they aren't sent.
test_workerFrames :: Assertion
test_workerFrames = do
  events <- runScene (scene Ellipse steadyWrites) {seconds = 6 / 60}
  assertEqual
    "the frames"
    (map VisualizerSamples [write 0, write 1, write 2, write 3])
    (map snd events)

-- | The worker wakes for the third frame when the fifth is due. The sixth
-- frame then shows the samples of the frames it skipped.
test_workerLate :: Assertion
test_workerLate = do
  events <-
    runScene
      (scene Ellipse steadyWrites)
        { lateness = \t -> if nearFrame 3 t then 2.25 / 60 else 0
        , debug = True
        , seconds = 1
        }
  assertEqual
    "the frames"
    (map VisualizerSamples [write 0, BS.concat (map write [1 .. 3]), write 4])
    (take 3 [e | (_, e@(VisualizerSamples _)) <- events])
  assertEqual
    "what happened in the second"
    [ FrameStats
        { frames = 57
        , late = 2
        , empty = 2
        , dropped = 0
        , bytes = 59 * BS.length (write 0)
        }
    ]
    [st | (_, VisualizerStats st) <- events]

-- | The first two frames show nothing, so they aren't sent, and nothing
-- drops them.
test_workerDropped :: Assertion
test_workerDropped = do
  events <- runScene (scene Ellipse steadyWrites) {taken = False, debug = True, seconds = 1}
  assertEqual
    "frames and dropped"
    [(59, 57)]
    [(st.frames, st.dropped) | (_, VisualizerStats st) <- events]

-- | Samples in the left channel only, so that the channels differ.
test_workerSpectrum :: Assertion
test_workerSpectrum = do
  let leftOnly k = samples (replicate (sampleRate `div` 60) (fromIntegral k + 1, 0))
  events <- runScene (scene Spectrum [(time k, leftOnly k) | k <- [0 ..]]) {seconds = 0.5}
  case [(l, r) | (_, VisualizerSpectrum l r) <- events, any ((> 0) . VS.maximum) [l, r]] of
    (left, right) : _ -> do
      assertEqual
        "the bins"
        [32768 `div` 2 + 1, 32768 `div` 2 + 1]
        (map VS.length [left, right])
      assertBool "the left channel" (VS.maximum left > 0)
      assertEqual "the right channel" 0 (VS.maximum right)
    [] -> assertFailure "no spectrum of the samples"

-- | Loud samples show on the spectrum, then silence on the wave for longer
-- than the spectrum's window. Back on the spectrum, it shows the silence,
-- not the samples from before the switch.
test_workerSwitch :: Assertion
test_workerSwitch = do
  let silent = samples (replicate (sampleRate `div` 60) (0, 0))
      writes = [(time k, if k < 20 then write k else silent) | k <- [0 ..]]
  events <-
    runScene
      (scene Spectrum writes) {seconds = 1.5, switches = [(0.3, Wave), (1, Spectrum)]}
  case [VS.maximum l + VS.maximum r | (t, VisualizerSpectrum l r) <- events, t > 1] of
    back : _ -> assertEqual "silent" 0 back
    [] -> assertFailure "no spectrum after the switch back"
  assertBool
    "loud before the switch"
    (any (> 0) [VS.maximum l | (t, VisualizerSpectrum l _) <- events, t < 0.3])

-- | MPD writes for half a second. The frames show the last write 2 frames
-- later, after which the samples fall out of the window, a frame's worth
-- at a time.
test_workerSilence :: Assertion
test_workerSilence = do
  events <- runScene (scene Spectrum (take 30 steadyWrites)) {seconds = 2}
  let spectra = [(t, VS.maximum l + VS.maximum r) | (t, VisualizerSpectrum l r) <- events]
      lastSamples = 32 / 60
  assertEqual
    "the frames of silence"
    ((windowSamples * 60 + sampleRate - 1) `div` sampleRate)
    (length [() | (t, _) <- spectra, t > lastSamples])
  case reverse spectra of
    (_, final) : (_, before) : _ -> do
      assertEqual "silent at last" 0 final
      assertBool "not silent before" (before > 0)
    _ -> assertFailure "too few spectra"

-- | The mix of the channels is silent, so the wave ends with the last
-- samples.
test_workerWave :: Assertion
test_workerWave = do
  let sample = samples [(1000, -1000)]
      writes = [(time k, BS.concat (replicate 735 sample)) | k <- [0 ..]]
  events <- runScene (scene Wave writes) {seconds = 0.5}
  case [pcm | (_, VisualizerWave pcm) <- events, BS.any (/= 0) pcm] of
    pcm : _ -> do
      assertEqual "the length" (waveSamples * frameBytes) (BS.length pcm)
      assertEqual "the last sample" sample (BS.takeEnd frameBytes pcm)
    [] -> assertFailure "no wave of the samples"

-- | Write the samples to the fifo again and again while an action runs. The
-- writer can open the fifo once the worker opened it, which also drops
-- what the fifo held before, so the samples go on until they come.
withWriting :: FilePath -> BS.ByteString -> IO a -> IO a
withWriting path pcm act = do
  writer <- forkIO . withWriter $ \h -> forever $ do
    BS.hPut h pcm
    threadDelay writeInterval
  act `finally` killThread writer
  where
    -- Opening a fifo without a reader fails, so it opens again until the
    -- worker reads it. Closing flushes into the fifo, which fails once the
    -- worker stopped reading it.
    withWriter :: (Handle -> IO ()) -> IO ()
    withWriter k =
      try @IOException (openBinaryFile path WriteMode) >>= \case
        Left _ -> threadDelay writeInterval >> withWriter k
        Right h -> k h `finally` void (try @IOException (hClose h))

    -- A tenth of a frame at 60 frames a second.
    writeInterval :: Int
    writeInterval = 1000000 `div` 600

-- | It tries again for another visualization, or when the visualizer shows
-- again. The failure comes also when the UI has no room for frames.
test_workerFails :: Assertion
test_workerFails = withSystemTempDirectory "visualizer" $ \dir -> do
  events <- newTQueueIO
  reading <- newTVarIO (Just Ellipse)
  let failed :: String -> Assertion
      failed what =
        expectWithin (atomically (readTQueue events)) >>= \case
          VisualizerFailed _ -> pure ()
          e -> assertFailure $ what <> ": " <> show e
      -- The UI takes one event at a time, so a worker that tried again at
      -- once fails the test instead of filling the memory.
      oneAtATime e = atomically $ do
        empty <- isEmptyTQueue events
        check empty
        writeTQueue events e
      src = (source reading events) {emit = const (pure False), report = oneAtATime}
  -- A worker that tries again at once never takes the kill, which would
  -- hang the test instead of failing it.
  bracket (forkIO . realWorker (dir </> "missing") $ src) (void . forkIO . killThread) $ \_ -> do
    failed "the first try"
    timeout 100000 (atomically (readTQueue events)) >>= \case
      Nothing -> pure ()
      Just e -> assertFailure $ "a try with nothing changed: " <> show e
    atomically $ writeTVar reading (Just Spectrum)
    failed "another visualization"
    -- The key that shows the visualizer again comes long after the worker
    -- saw it leave.
    atomically $ writeTVar reading Nothing
    threadDelay 100000
    atomically $ writeTVar reading (Just Spectrum)
    failed "shown again"

-- | A regular file, e.g. an audio file named by mistake, would be read whole
-- on every frame.
test_workerRegularFile :: Assertion
test_workerRegularFile = withSystemTempDirectory "visualizer" $ \dir -> do
  events <- newTQueueIO
  reading <- newTVarIO (Just Ellipse)
  let file = dir </> "song.flac"
  BS.writeFile file (BS.replicate 1000 0)
  bracket (forkIO . realWorker file $ source reading events) killThread $ \_ ->
    expectWithin (atomically (readTQueue events)) >>= \case
      VisualizerFailed reason ->
        assertBool ("the reason: " <> T.unpack reason) ("not a fifo" `T.isInfixOf` reason)
      e -> assertFailure $ "not a failure: " <> show e

-- | A second has the frames that came in it. The 60th frame comes a second
-- after the start, so it starts the next one.
test_workerStats :: Assertion
test_workerStats = do
  events <- runScene (scene Ellipse steadyWrites) {debug = True, seconds = 1}
  assertEqual
    "what happened in the second"
    [ FrameStats
        { frames = 59
        , late = 0
        , empty = 2
        , dropped = 0
        , bytes = 59 * BS.length (write 0)
        }
    ]
    [st | (_, VisualizerStats st) <- events]

-- | MPD writes for half a second, pauses for a second and plays again. The
-- trail of the ellipse falls through silence, the worker sleeps through
-- the rest of the pause, and the frames show the new samples once the
-- buffer holds a frame and the writes ahead.
test_workerPause :: Assertion
test_workerPause = do
  let resumed = 1.5
      writes =
        take 30 steadyWrites
          <> [(resumed + time k, write (30 + k)) | k <- [0 .. 29]]
  (events, wakes) <- runSceneWakes (scene Ellipse writes) {seconds = 2.2}
  let frames = [(t, pcm) | (t, VisualizerSamples pcm) <- events]
      (paused, played) = span ((< resumed) . fst) frames
  assertEqual
    "the trail falls through silence"
    (replicate sceneTrail "")
    (reverse . takeWhile BS.null . reverse $ map snd paused)
  assertEqual "no wakes during the pause" [] [w | w <- wakes, w > 0.7, w < resumed]
  case played of
    (t, pcm) : _ -> do
      assertEqual "the new samples" (write 30) pcm
      assertBool ("the first new frame at " <> show t) (t <= resumed + 4 / 60)
    [] -> assertFailure "no frames after the pause"

-- | The worker doesn't wake before the first samples, e.g. while MPD is
-- paused when the visualizer shows.
test_workerFirstSamples :: Assertion
test_workerFirstSamples = do
  let writes = [(1 + time k, write k) | k <- [0 ..]]
  (events, wakes) <- runSceneWakes (scene Ellipse writes) {seconds = 1.2}
  assertEqual "no wakes before the samples" [] (filter (< 1) wakes)
  assertBool
    "the samples"
    (not (null [() | (_, VisualizerSamples pcm) <- events, not (BS.null pcm)]))

-- | MPD keeps the fifo open, and writes nothing while it is paused. The
-- worker, which waits for the samples, closes the fifo once the visualizer
-- hides. A write would wake it, so the test counts the descriptors open on
-- the fifo instead.
test_workerHidden :: Assertion
test_workerHidden = withSystemTempDirectory "visualizer" $ \dir -> do
  let path = dir </> "fifo"
  createNamedPipe path (unionFileModes ownerReadMode ownerWriteMode)
  events <- newTQueueIO
  reading <- newTVarIO (Just Ellipse)
  bracket (forkIO . realWorker path $ source reading events) killThread $ \_ ->
    bracket (openWriter path) hClose $ \_ -> do
      threadDelay 100000
      assertEqual "the worker's and MPD's" 2 =<< openOn path
      atomically $ writeTVar reading Nothing
      expectWithin $ untilOpen path 1
  where
    untilOpen :: FilePath -> Int -> IO ()
    untilOpen path n = do
      open <- openOn path
      unless (open == n) $ threadDelay 10000 >> untilOpen path n

-- | MPD closes the fifo when it quits, and opens it when it starts again.
test_workerRestart :: Assertion
test_workerRestart = withSystemTempDirectory "visualizer" $ \dir -> do
  let path = dir </> "fifo"
  createNamedPipe path (unionFileModes ownerReadMode ownerWriteMode)
  events <- newTQueueIO
  reading <- newTVarIO (Just Ellipse)
  let pcm s = samples (replicate 100 s)
  bracket (forkIO . realWorker path $ source reading events) killThread $ \_ -> do
    void . withWriting path (pcm (1000, -1000)) . expectWithin $ someSamples events
    -- The worker sends frames until it waits for the fifo, which finds the
    -- writer gone. A writer that came sooner would write into the same fifo.
    expectWithin $ untilQuiet events
    withWriting path (pcm (2000, -2000)) . expectWithin $ samplesOf events (2000, -2000)
  where
    -- Six frames at 60 frames a second.
    untilQuiet :: TQueue AppEvent -> IO ()
    untilQuiet events =
      timeout 100000 (atomically (readTQueue events)) >>= mapM_ (\_ -> untilQuiet events)

    -- The frames that the worker still has of the first writer come first.
    samplesOf :: TQueue AppEvent -> (Int16, Int16) -> IO ()
    samplesOf events s = do
      bytes <- someSamples events
      unless (samples [s] `BS.isPrefixOf` bytes) $ samplesOf events s

-- | Once the writer went, a wait on the fifo says so. On the fifo opened
-- again, it waits for the next writer instead of finding it gone at once.
test_fifoWriterGone :: Assertion
test_fifoWriterGone = withSystemTempDirectory "visualizer" $ \dir -> do
  let path = dir </> "fifo"
  createNamedPipe path (unionFileModes ownerReadMode ownerWriteMode)
  runEff . runFifo path $ do
    openFifo
    liftIO $ writeOnce path "first"
    assertWaited "the samples" SamplesCame =<< waitFifo retry
    liftIO . assertEqual "the first samples" "first" =<< readFifo
    assertWaited "the writer went" WriterGone =<< waitFifo retry
    openFifo
    soon <- liftIO $ registerDelay 100000
    assertWaited "a wait for the next writer" Cancelled
      =<< waitFifo (readTVar soon >>= check)
    liftIO $ writeOnce path "second"
    assertWaited "the next samples" SamplesCame =<< waitFifo retry
    liftIO . assertEqual "the next samples" "second" =<< readFifo
  where
    assertWaited :: IOE :> es => String -> Waited -> Waited -> Eff es ()
    assertWaited what expected = liftIO . assertEqual what expected

    writeOnce :: FilePath -> BS.ByteString -> IO ()
    writeOnce path bytes = bracket (openBinaryFile path WriteMode) hClose (`BS.hPut` bytes)

----------------------------------------
-- Helpers

-- | The first frame with samples.
someSamples :: TQueue AppEvent -> IO BS.ByteString
someSamples events =
  atomically (readTQueue events) >>= \case
    VisualizerSamples bytes | not (BS.null bytes) -> pure bytes
    _ -> someSamples events

-- | The descriptors of the process that are open on a file.
openOn :: FilePath -> IO Int
openOn path = do
  fds <- listDirectory "/proc/self/fd"
  links <- mapM (try @IOException . readSymbolicLink . ("/proc/self/fd" </>)) fds
  pure $ length [() | Right link <- links, link == path]

-- | Open the fifo for writing, once the worker opened it for reading.
openWriter :: FilePath -> IO Handle
openWriter path =
  try @IOException (openBinaryFile path WriteMode) >>= \case
    Left _ -> threadDelay 10000 >> openWriter path
    Right h -> pure h

source :: TVar (Maybe Visualization) -> TQueue AppEvent -> VisualizerSource
source reading events =
  VisualizerSource
    { fps = 60
    , trail = sceneTrail
    , reading = reading
    , emit = \e -> True <$ atomically (writeTQueue events e)
    , report = atomically . writeTQueue events
    , debug = False
    }

-- | The worker with the real clock and the fifo at a path.
realWorker :: FilePath -> VisualizerSource -> IO ()
realWorker path = runEff . runClock . runFifo path . visualizerWorker

-- | What happens around the worker, from when it opens the fifo.
data Scene = Scene
  { visualization :: Visualization
  , writes :: [(Double, BS.ByteString)]
  -- ^ MPD's writes, by their times.
  , lateness :: Double -> Double
  -- ^ How late the worker wakes up for a time.
  , taken :: Bool
  -- ^ Whether the UI has room for the events.
  , debug :: Bool
  , seconds :: Double
  -- ^ How long the scene lasts.
  , switches :: [(Double, Visualization)]
  -- ^ The visualizations that the user switches to, by their times.
  }

scene :: Visualization -> [(Double, BS.ByteString)] -> Scene
scene v ws =
  Scene
    { visualization = v
    , writes = ws
    , lateness = const 0
    , taken = True
    , debug = False
    , seconds = 1
    , switches = []
    }

-- | The worker's events in a scene, with their times. Time passes only when
-- the worker sleeps or waits for the samples, so the scene runs at once.
runScene :: Scene -> IO [(Double, AppEvent)]
runScene = fmap fst . runSceneWakes

-- | The worker's events in a scene, and the times that it slept until. The
-- scene ends when the worker waits for samples that don't come.
runSceneWakes :: Scene -> IO ([(Double, AppEvent)], [Double])
runSceneWakes sc = do
  timeRef <- S.newIORef 0
  writesRef <- S.newIORef sc.writes
  eventsRef <- S.newIORef []
  wakesRef <- S.newIORef []
  reading <- newTVarIO (Just sc.visualization)
  let src =
        VisualizerSource
          { fps = 60
          , trail = sceneTrail
          , reading = reading
          , emit = \e -> do
              t <- S.readIORef timeRef
              S.modifyIORef eventsRef ((t, e) :)
              pure sc.taken
          , report = \e -> do
              t <- S.readIORef timeRef
              S.modifyIORef eventsRef ((t, e) :)
          , debug = sc.debug
          }
  void
    . try @SceneEnded
    . runEff
    . runScriptedClock timeRef wakesRef reading
    . runScriptedFifo timeRef writesRef
    $ visualizerWorker src
  (,) <$> (reverse <$> S.readIORef eventsRef) <*> (reverse <$> S.readIORef wakesRef)
  where
    -- A sleep moves the time to its end, or ends the scene. The user
    -- switches to the visualization of the last switch until then.
    runScriptedClock
      :: IOE :> es
      => S.IORef Double
      -> S.IORef [Double]
      -> TVar (Maybe Visualization)
      -> Eff (Clock : es) a
      -> Eff es a
    runScriptedClock timeRef wakesRef reading = interpret_ $ \case
      MonotonicTime -> liftIO $ S.readIORef timeRef
      SleepUntil t
        | t > sc.seconds -> liftIO $ throwIO SceneEnded
        | otherwise -> liftIO $ do
            S.modifyIORef wakesRef (t :)
            S.modifyIORef timeRef $ \now -> max now t + sc.lateness t
            now <- S.readIORef timeRef
            case reverse [v | (at, v) <- sc.switches, at <= now] of
              v : _ -> atomically . writeTVar reading $ Just v
              [] -> pure ()

    -- A read takes the writes that came until the time, and a wait moves
    -- the time to the next write.
    runScriptedFifo
      :: IOE :> es
      => S.IORef Double
      -> S.IORef [(Double, BS.ByteString)]
      -> Eff (Fifo : es) a
      -> Eff es a
    runScriptedFifo timeRef writesRef = interpret_ $ \case
      OpenFifo -> pure ()
      ReadFifo -> liftIO $ do
        now <- S.readIORef timeRef
        (came, later) <- span ((<= now) . fst) <$> S.readIORef writesRef
        S.writeIORef writesRef later
        pure . BS.concat $ map snd came
      WaitFifo _ ->
        liftIO $
          S.readIORef writesRef >>= \case
            (t, _) : _ | t <= sc.seconds -> SamplesCame <$ S.modifyIORef timeRef (max t)
            _ -> throwIO SceneEnded
      CloseFifo -> pure ()

data SceneEnded = SceneEnded
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The frames whose samples the ellipse of a scene shows.
sceneTrail :: Int
sceneTrail = 3

-- | MPD writes a frame's samples each frame, half a frame after it.
steadyWrites :: [(Double, BS.ByteString)]
steadyWrites = [(time k, write k) | k <- [0 ..]]

-- | The time of a frame's write.
time :: Int -> Double
time k = (fromIntegral k + 0.5) / 60

-- | A frame's samples, which tell the writes apart.
write :: Int -> BS.ByteString
write k = samples (replicate (sampleRate `div` 60) (v, -v))
  where
    v :: Int16
    v = fromIntegral k + 1

-- | Whether a time is of a frame.
nearFrame :: Int -> Double -> Bool
nearFrame n t = abs (t - fromIntegral n / 60) < 1 / 120

-- | Wait for what comes soon.
expectWithin :: IO a -> IO a
expectWithin act = timeout (5 * 1000000) act >>= maybe (assertFailure "nothing came") pure
