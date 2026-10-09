module VisualizerTests (visualizerTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.IORef.Strict qualified as S
import Data.Int
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector.Storable qualified as VS
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.ByteOrder
import Graphics.Vty qualified as V
import Optics.Core
import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix.Files
import System.Timeout
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Reprise.Config
import Reprise.Effect.Clock
import Reprise.Effect.Fifo
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Style
import Reprise.UI.Layout
import Reprise.Visualizer.Samples
import Reprise.Visualizer.Spectrum
import Reprise.Visualizer.Wave
import Reprise.Visualizer.Worker
import Reprise.Width
import Utils

visualizerTests :: TestTree
visualizerTests =
  testGroup
    "Visualizer"
    [ testCase "showing the visualizer reads the samples, leaving it stops" test_reading
    , testCase "space switches the visualization" test_switch
    , testCase "the bars of the spectrum" test_bars
    , testCase "the colors of a bar blend from its foot to its top" test_barColors
    , testCase "a row is as few texts as it can be" test_runs
    , testCase "without a data source, the screen says how to set one" test_noSource
    , testCase "samples after the visualizer stopped are dropped" test_staleSamples
    , testCase "the frames of the trail stay" test_trail
    , testCase "silence after silence keeps the screen" test_silence
    , testCase "no room for the visualizer" test_noRoom
    , testCase "mono is a diagonal" test_mono
    , testCase "the left channel alone is a horizontal line" test_oneChannel
    , testCase "louder samples have later colors" test_loudness
    , goldenVsString
        "a circle"
        ("tests" </> "reprise" </> "golden" </> "visualizer.txt")
        (BL.fromStrict . T.encodeUtf8 . T.unlines <$> mainLines Ellipse visualizing [circle])
    , testCase "silence is a line in the middle of each channel" test_waveSilence
    , testCase "a wave goes across the screen" test_waveRamp
    , testCase "a jump of the wave is a line" test_waveJump
    , testCase "louder samples of the wave have later colors" test_waveLoudness
    , testCase "a steady sound stands still in the wave" test_triggerStill
    , testCase "the wave follows the bass" test_triggerBass
    , testCase "without a rise of the bass, the wave is of the last samples" test_triggerNone
    , testCase "the wave of silence" test_triggerSilence
    , goldenVsString
        "a period of a sine and a cosine"
        ("tests" </> "reprise" </> "golden" </> "visualizer-wave.txt")
        (BL.fromStrict . T.encodeUtf8 . T.unlines <$> mainLines Wave visualizing [circle])
    , testCase "the spectrum of a sine" test_sineSpectrum
    , testCase "the window of the spectrum" test_window
    , testCase "every frame shows the samples of its time" test_playout
    , testCase "MPD stops writing" test_playoutStops
    , testCase "a short write" test_playoutShortWrite
    , testCase "the lag is bounded" test_playoutLag
    , testCase "the worker sends the samples of the fifo" test_worker
    , testCase "each frame sends the samples of its time" test_workerFrames
    , testCase "a late frame doesn't make the next ones late" test_workerLate
    , testCase "a dropped frame counts in its own second" test_workerDropped
    , testCase "the worker sends the spectrum" test_workerSpectrum
    , testCase "the spectrum stays until its window is silent" test_workerSilence
    , testCase "the worker sends the wave" test_workerWave
    , testCase "the worker reports a data source that it can't read" test_workerFails
    , testCase "a data source that isn't a fifo" test_workerRegularFile
    , testCase "the screen says why it can't read the data source" test_failureShows
    , testCase "the debug line shows what happened to the frames" test_debugLine
    , testCase "the worker sends what happened to its frames" test_workerStats
    ]

test_reading :: Assertion
test_reading = do
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  shown <- runEventsWith visualizing 0 [key "8"] s
  assertEqual "started" [Visualize (Just Spectrum)] [c | c@(Visualize _) <- shown.commands]
  left <- runEventsWith visualizing 0 [key "1"] shown.state
  assertEqual "stopped" [Visualize Nothing] [c | c@(Visualize _) <- left.commands]

test_switch :: Assertion
test_switch = do
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  r <- runEventsWith visualizing 0 [key "8", key "space", key "space"] s
  assertEqual
    "the samples"
    [Visualize (Just Spectrum), Visualize (Just Ellipse), Visualize (Just Wave)]
    [c | c@(Visualize _) <- r.commands]
  assertEqual "the message" (Just "Visualization: wave") ((.text) <$> r.state.message)
  back <- runEventsWith visualizing 0 [key "space"] r.state
  assertEqual "back" Spectrum back.state.toggles.visualization

-- | A full bar around 1 kHz, in column 22 of 40: from 20 Hz times 1000 to
-- the power of 22 / 40, 893 Hz, to 1000 to the power of 23 / 40, 1062 Hz.
-- A column shows the mean of its bins, so they are all at full scale.
test_bars :: Assertion
test_bars = do
  let bins = 32768 `div` 2 + 1
      silent = VS.replicate bins 0
      edge x = 20 * 1000 ** (x / 40)
      full = VS.generate bins $ \k ->
        if binFrequency k >= edge 22 && binFrequency k < edge 23 then 1 else 0
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  stereo <-
    (.state) <$> runEventsWith visualizing 0 [key "8", VisualizerSpectrum full silent] s
  assertEqual
    "rising in the top half"
    [(r, 22) | r <- [0 .. 3]]
    (filled (mainLinesOf stereo))
  hanging <-
    (.state) <$> runEventsWith visualizing 0 [key "8", VisualizerSpectrum silent full] s
  assertEqual
    "hanging in the bottom half"
    [(r, 22) | r <- [4 .. 7]]
    (filled (mainLinesOf hanging))
  -- -46 dB is 0.6 of the way from -100 dB to -10 dB, so 2.4 of the 4 rows.
  let partly = VS.map (* (10 ** (-46 / 20))) full
  ending <-
    (.state) <$> runEventsWith visualizing 0 [key "8", VisualizerSpectrum silent partly] s
  assertEqual
    "the end of a hanging bar"
    ["█", "█", "🮃"]
    [T.take 1 (T.drop 22 l) | l <- take 3 (drop 4 (mainLinesOf ending))]
  assertBool
    "no reverse video"
    ( and
        [ not (V.hasStyle st V.reverseVideo)
        | (a, _) <- imageSpans (renderScreen visualizing ending)
        , V.SetTo st <- [V.attrStyle a]
        ]
    )
  assertEqual
    "the upper blocks are one column wide"
    [1]
    (L.nub (map (textWidth . T.singleton) "▔🮂🮃▀🮄🮅🮆"))
  where
    filled :: [T.Text] -> [(Int, Int)]
    filled ls = [(r, c) | (r, l) <- zip [0 ..] ls, (c, ch) <- zip [0 ..] (T.unpack l), ch /= ' ']

    mainLinesOf :: AppState -> [T.Text]
    mainLinesOf s = take (mainHeight s.terminalSize) . drop 2 $ imageLines (renderScreen visualizing s)

-- | A full bar from black to white has a color of its own in each of its 4
-- rows, from black at its foot to white at its top.
test_barColors :: Assertion
test_barColors = do
  let env =
        visualizing
          & #config % #visualizer % #colors .~ (style "#000000" NE.:| [style "#ffffff"])
      bins = 32768 `div` 2 + 1
      edge x = 20 * 1000 ** (x / 40)
      full = VS.generate bins $ \k ->
        if binFrequency k >= edge 22 && binFrequency k < edge 23 then 1 else 0
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  r <- runEventsWith env 0 [key "8", VisualizerSpectrum full (VS.replicate bins 0)] s
  let colors = [V.attrForeColor a | (a, t) <- imageSpans (renderScreen env r.state), T.any (== '█') t]
  case colors of
    [V.SetTo top, V.SetTo second, V.SetTo third, V.SetTo foot] -> do
      assertEqual "the top" (V.RGBColor 255 255 255) top
      assertEqual "the foot" (V.RGBColor 0 0 0) foot
      assertEqual "4 colors" 4 (length (L.nub [top, second, third, foot]))
    _ -> assertFailure $ "colors: " <> show colors

-- | A bar in the middle of a row and the blanks around it are one text,
-- unless the color has a background, which would show on the blanks.
test_runs :: Assertion
test_runs = do
  let bins = 32768 `div` 2 + 1
      edge x = 20 * 1000 ** (x / 40)
      bar = VS.generate bins $ \k ->
        if binFrequency k >= edge 22 && binFrequency k < edge 23 then 1 else 0
      spansIn env = do
        s <- testState (40, 12) (statusOf Stopped Nothing 0) []
        r <- runEventsWith env 0 [key "8", VisualizerSpectrum bar bar] s
        pure . length $ imageSpans (renderScreen env r.state)
      withBackground =
        visualizing & #config % #visualizer % #colors .~ (style "red on blue" NE.:| [])
  joined <- spansIn visualizing
  apart <- spansIn withBackground
  assertEqual
    "the blanks on both sides of the bar in each of the 8 rows"
    (2 * 8)
    (apart - joined)

test_noSource :: Assertion
test_noSource = do
  r <- runEvents 0 [key "8"] =<< testState (80, 12) (statusOf Stopped Nothing 0) []
  assertEqual "no samples" [] [c | c@(Visualize _) <- r.commands]
  case imageLines (renderScreen testAppEnv r.state) of
    _ : _ : first : _ ->
      assertBool ("the hint: " <> T.unpack first) ("visualizer.data_source" `T.isInfixOf` first)
    ls -> assertFailure $ "too few lines: " <> show ls

test_staleSamples :: Assertion
test_staleSamples = do
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  r <- runEventsWith visualizing 0 [VisualizerSamples circle] s
  assertEqual "the screen stays" [KeepScreen] r.commands
  assertEqual "no frames" Seq.empty (ellipseFrames r.state)

-- | 50 ms at 60 frames a second are 3 frames.
test_trail :: Assertion
test_trail = do
  let env = visualizing & #config % #visualizer % #trail .~ 0.05
  r <- shownWith Ellipse env (replicate 5 circle)
  assertEqual "frames" 3 (Seq.length (ellipseFrames r))

test_silence :: Assertion
test_silence = do
  s <- shownWith Ellipse visualizing []
  r <- runEventsWith visualizing 0 [VisualizerSamples BS.empty] s
  assertEqual "the screen stays" [KeepScreen] r.commands
  stayed <- shownWith Ellipse visualizing [circle, BS.empty]
  assertEqual
    "the screen changes while samples are on it"
    2
    (Seq.length (ellipseFrames stayed))

-- | The header and the bars take the 4 rows.
test_noRoom :: Assertion
test_noRoom = do
  s <- testState (40, 4) (statusOf Stopped Nothing 0) []
  r <- runEventsWith visualizing 0 [key "8", key "space", VisualizerSamples circle] s
  assertEqual "the lines" 4 (length (imageLines (renderScreen visualizing r.state)))

test_mono :: Assertion
test_mono = do
  dots <- dotsOf Ellipse [(v, v) | v <- [minBound, minBound + 1000 .. maxBound]]
  assertBool "dots on several rows" (length (L.nub (map fst dots)) > 1)
  case (dots, reverse dots) of
    ((_, top) : _, (_, bottom) : _) -> assertBool "up to the right" (top > bottom)
    _ -> assertFailure "no dots"

test_oneChannel :: Assertion
test_oneChannel = do
  dots <- dotsOf Ellipse [(v, 0) | v <- [minBound, minBound + 1000 .. maxBound]]
  assertBool "dots in several columns" (length (L.nub (map snd dots)) > 1)
  assertEqual "one row" 1 (length (L.nub (map fst dots)))

-- | With two colors, a quiet sample in the center has the first, and a
-- loud one in the top right corner the second.
test_loudness :: Assertion
test_loudness = do
  let env = visualizing & #config % #visualizer % #colors .~ (style "red" NE.:| [style "blue"])
  s <- shownWith Ellipse env [samples [(1000, 1000), (maxBound, maxBound)]]
  let colorsOf = [V.attrForeColor a | (a, t) <- imageSpans (renderScreen env s), T.any isBraille t]
  assertEqual "colors" [V.SetTo (V.ISOColor 4), V.SetTo (V.ISOColor 1)] colorsOf

-- | The left channel in the top 4 rows and the right one in the bottom 4.
test_waveSilence :: Assertion
test_waveSilence = do
  dots <- dotsOf Wave (replicate 100 (0, 0))
  let rows = L.nub (map fst dots)
  assertBool ("the rows " <> show rows) (rows `elem` [[r, r + 4] | r <- [1, 2]])
  assertEqual "a dot in each column of both" (2 * 40) (length dots)

-- | 10 samples across the 80 columns of dots, which interpolate between
-- them.
test_waveRamp :: Assertion
test_waveRamp = do
  dots <-
    dotsOf
      Wave
      [ (round @Double (fromIntegral (maxBound @Int16) * (2 * fromIntegral i / 9 - 1)), 0)
      | i <- [0 .. 9 :: Int]
      ]
  let left = [d | d@(r, _) <- dots, r < 4]
  assertEqual "every column" [0 .. 39] (L.nub (L.sort (map snd left)))
  assertEqual "every row" [0 .. 3] (L.nub (L.sort (map fst left)))
  assertEqual "from the bottom" [3] [r | (r, 0) <- left]
  assertEqual "to the top" [0] [r | (r, 39) <- left]

-- | 300 samples across 80 columns of dots jump from the bottom to the top
-- between the columns of dots 39 and 40, which are in the cells 19 and 20.
-- The dots between join them.
test_waveJump :: Assertion
test_waveJump = do
  dots <- dotsOf Wave [(if i < 150 then minBound else maxBound, 0) | i <- [0 .. 299 :: Int]]
  assertEqual
    "the columns of the middle rows of the left channel"
    [19, 20]
    (L.nub (L.sort [c | (r, c) <- dots, r `elem` [1, 2]]))

-- | With two colors, the silent right channel has the first, and the left
-- one at full scale the second.
test_waveLoudness :: Assertion
test_waveLoudness = do
  let env = visualizing & #config % #visualizer % #colors .~ (style "red" NE.:| [style "blue"])
  s <- shownWith Wave env [samples (replicate 100 (maxBound, 0))]
  let colorsOf = [V.attrForeColor a | (a, t) <- imageSpans (renderScreen env s), T.any isBraille t]
  assertEqual "colors" [V.SetTo (V.ISOColor 4), V.SetTo (V.ISOColor 1)] colorsOf

-- | A tone of 100 Hz, with a period of 441 samples. Once more samples come,
-- the wave is the same.
test_triggerStill :: Assertion
test_triggerStill = do
  w <- newWaveWindow
  let tone i = 20000 * sin (2 * pi * 100 * fromIntegral (i `mod` 441) / 44100)
  pushWave w (sound tone 0 44100)
  first <- waveOf w
  assertEqual "the length" (waveSamples * frameBytes) (BS.length first)
  pushWave w (sound tone 44100 44200)
  assertEqual "the wave" first =<< waveOf w

-- | A bass of 100 Hz and a treble of 2953 Hz as loud. The treble isn't a
-- harmonic of the bass, so it rises through zero at other points of the
-- bass's period in each one. As more samples come, in 20 chunks of more
-- than 2 periods, the waves differ from the first by the swing of the
-- treble and the bass's change in 10 samples at most: the treble that the
-- filter passes moves the rise by a few samples. Without the filter, the
-- bass is at any point of its period, and they differ by up to the swing
-- of both.
test_triggerBass :: Assertion
test_triggerBass = do
  w <- newWaveWindow
  let tone i =
        let t = fromIntegral i / 44100
        in bass * sin (2 * pi * 100 * t) + treble * sin (2 * pi * 2953 * t)
  pushWave w (sound tone 0 44100)
  first <- waveOf w
  differences <- forM [1 .. 20] $ \k -> do
    pushWave w (sound tone (44100 + (k - 1) * 1000) (44100 + k * 1000))
    later <- waveOf w
    pure . maximum $ zipWith (\a b -> abs (a - b)) (left first) (left later)
  assertBool
    ("the differences: " <> show differences)
    (all (<= 2 * treble + bass * 2 * pi * 100 * 10 / 44100) differences)
  where
    bass, treble :: Double
    bass = 10000
    treble = 10000

    left :: BS.ByteString -> [Double]
    left pcm =
      [ sampleAt pcm (i * channels) * fromIntegral (maxBound @Int16)
      | i <- [0 .. BS.length pcm `div` frameBytes - 1]
      ]

test_triggerNone :: Assertion
test_triggerNone = do
  w <- newWaveWindow
  let rising = samples [(fromIntegral (i `mod` 30000) + 1, 1000) | i <- [0 .. 4000 :: Int]]
  pushWave w rising
  assertEqual "the last samples" (BS.takeEnd (waveSamples * frameBytes) rising) =<< waveOf w

test_triggerSilence :: Assertion
test_triggerSilence = do
  w <- newWaveWindow
  let silence = BS.replicate (waveSamples * frameBytes) 0
  assertEqual "at first" silence =<< waveOf w
  pushWave w circle
  pushWaveSilence w (historySamples * frameBytes)
  assertEqual "after the samples" silence =<< waveOf w

-- | The Blackman window passes 0.42 of a sine, and a real sine is half in
-- the bin of its frequency and half in the bin of the negative one.
test_sineSpectrum :: Assertion
test_sineSpectrum = do
  transform <- newTransform
  let amplitude = 0.9
      frequency = 1000
      sine =
        samples
          [ (round (amplitude * fromIntegral (maxBound @Int16) * sin (2 * pi * frequency * t)), 0)
          | i <- [0 .. windowSamples - 1]
          , let t = fromIntegral i / 44100
          ]
  window <- newSampleWindow
  pushSamples window sine
  left <- spectrumOf transform window 0
  right <- spectrumOf transform window 1
  let peak = VS.maxIndex left
  assertBool
    ("the peak at " <> show (binFrequency peak) <> " Hz")
    (abs (binFrequency peak - frequency) <= binFrequency 1)
  assertBool
    ("its magnitude " <> show (left VS.! peak))
    (abs (left VS.! peak - amplitude * 0.42 / 2) < 0.01)
  assertEqual "the silent channel" 0 (VS.maximum right)

-- | The window holds the last samples, whether they came at once or in
-- pieces, and silence pushes them out.
test_window :: Assertion
test_window = do
  transform <- newTransform
  let count = 2 * windowSamples
      noise =
        samples
          [ (fromIntegral (i * 7919), fromIntegral (i * 104729))
          | i <- [0 .. count - 1]
          ]
      bytesOf n = 4 * n
      half = windowSamples `div` 2
      spectra w = forM [0, 1] $ spectrumOf transform w
      windowOf pcm = do
        w <- newSampleWindow
        pushSamples w pcm
        pure w
  whole <- spectra =<< windowOf (BS.takeEnd (bytesOf windowSamples) noise)
  pieces <- newSampleWindow
  let (first, rest) = BS.splitAt (bytesOf 1000) noise
      (middle, end) = BS.splitAt (BS.length rest - bytesOf 3) rest
  mapM_ (pushSamples pieces) [first, middle, end]
  assertEqual "in pieces" whole =<< spectra pieces
  pushSilence pieces (bytesOf half)
  silenced <-
    spectra
      =<< windowOf (BS.takeEnd (bytesOf half) noise <> BS.replicate (bytesOf half) 0)
  assertEqual "half silent" silenced =<< spectra pieces
  pushSilence pieces (bytesOf count)
  assertEqual "silent" [0, 0] . map VS.maximum =<< spectra pieces

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
          in Simulated t shown (BS.length frame) stopped (BS.length p'.buffered)
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
  let pattern = samples (replicate 100 (1000, -1000))
  bracket (forkIO . realWorker path $ source reading events) killThread $ \_ -> do
    frame <- withWriting path pattern . expectWithin $ firstSamples events
    assertBool
      ("whole samples: " <> show (BS.length frame))
      (BS.length frame `mod` frameBytes == 0)
    assertBool "the samples" (frame `BS.isPrefixOf` BS.concat (replicate 100 pattern))
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
-- samples.
test_workerFrames :: Assertion
test_workerFrames = do
  events <- runScene (scene Ellipse steadyWrites) {seconds = 6 / 60}
  assertEqual
    "the frames"
    (map VisualizerSamples ["", "", write 0, write 1, write 2, write 3])
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
    (map VisualizerSamples ["", "", write 0, BS.concat (map write [1 .. 3]), write 4])
    (take 5 [e | (_, e@(VisualizerSamples _)) <- events])
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

test_workerDropped :: Assertion
test_workerDropped = do
  events <- runScene (scene Ellipse steadyWrites) {taken = False, debug = True, seconds = 1}
  assertEqual
    "frames and dropped"
    [(59, 59)]
    [(st.frames, st.dropped) | (_, VisualizerStats st) <- events]

test_workerSpectrum :: Assertion
test_workerSpectrum = do
  events <- runScene (scene Spectrum steadyWrites) {seconds = 0.5}
  case [(l, r) | (_, VisualizerSpectrum l r) <- events, any ((> 0) . VS.maximum) [l, r]] of
    (left, right) : _ -> do
      assertEqual
        "the bins"
        [32768 `div` 2 + 1, 32768 `div` 2 + 1]
        (map VS.length [left, right])
      assertBool "both channels" (all ((> 0) . VS.maximum) [left, right])
    [] -> assertFailure "no spectrum of the samples"

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
-- again.
test_workerFails :: Assertion
test_workerFails = withSystemTempDirectory "visualizer" $ \dir -> do
  events <- newTQueueIO
  reading <- newTVarIO (Just Ellipse)
  let failed :: String -> Assertion
      failed what =
        expectWithin (atomically (readTQueue events)) >>= \case
          VisualizerFailed _ -> pure ()
          e -> assertFailure $ what <> ": " <> show e
  bracket (forkIO . realWorker (dir </> "missing") $ source reading events) killThread $ \_ -> do
    failed "the first try"
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

-- | The screen says why, until the next try.
test_failureShows :: Assertion
test_failureShows = do
  s <- testState (80, 12) (statusOf Stopped Nothing 0) []
  let reason = "The visualizer can't read its data source: no such file"
  r <- runEventsWith visualizing 0 [key "8", VisualizerFailed reason] s
  case imageLines (renderScreen visualizing r.state) of
    _ : _ : first : second : _ -> do
      assertEqual "the reason" reason first
      assertBool ("how to try again: " <> T.unpack second) ("try again" `T.isInfixOf` second)
    ls -> assertFailure $ "too few lines: " <> show ls
  switched <- runEventsWith visualizing 0 [key "space"] r.state
  assertEqual "gone with the next try" Nothing switched.state.visualizer.failure
  left <- runEventsWith visualizing 0 [key "1", VisualizerFailed reason] s
  assertEqual "not after leaving" Nothing left.state.visualizer.failure

-- | The line takes the top row, and the picture the rest.
test_debugLine :: Assertion
test_debugLine = do
  let env = visualizing & #config % #visualizer % #debug .~ True
      stats = FrameStats {frames = 60, late = 1, empty = 2, dropped = 3, bytes = 176400}
  s <- testState (80, 12) (statusOf Stopped Nothing 0) []
  r <-
    runEventsWith
      env
      0
      [ key "8"
      , key "space"
      , VisualizerSamples circle
      , VisualizerSamples circle
      , VisualizerStats stats
      ]
      s
  let ls = imageLines (renderScreen env r.state)
  assertEqual "the lines" 12 (length ls)
  case drop 2 ls of
    first : _ ->
      assertEqual
        "the line"
        "2 fps drawn; worker: 60 frames, 1 late, 2 empty, 3 dropped, 176400 B read"
        first
    [] -> assertFailure "no main area"

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

----------------------------------------
-- Helpers

-- | The samples of the ellipse on the screen.
ellipseFrames :: AppState -> Seq.Seq BS.ByteString
ellipseFrames s = case s.visualizer.reading of
  Just (EllipseFrames frames) -> frames
  _ -> Seq.empty

-- | The default config with a data source.
visualizing :: AppEnv
visualizing = testAppEnv & #config % #visualizer % #dataSource ?~ "fifo"

-- | The state after a visualization showed the frames, the oldest first.
shownWith :: Visualization -> AppEnv -> [BS.ByteString] -> IO AppState
shownWith v env frames = do
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  (.state)
    <$> runEventsWith
      env
      0
      (key "8" : replicate (fromEnum v) (key "space") <> map frame frames)
      s
  where
    frame :: BS.ByteString -> AppEvent
    frame = case v of
      Wave -> VisualizerWave
      _ -> VisualizerSamples

-- | The lines of the main area, after the header's two, once a
-- visualization showed the frames.
mainLines :: Visualization -> AppEnv -> [BS.ByteString] -> IO [T.Text]
mainLines v env frames = do
  s <- shownWith v env frames
  pure . take (mainHeight s.terminalSize) . drop 2 $ imageLines (renderScreen env s)

-- | The rows and the columns of the cells with dots, from the top, after a
-- visualization showed a frame of the samples.
dotsOf :: Visualization -> [(Int16, Int16)] -> IO [(Int, Int)]
dotsOf v frame = do
  ls <- mainLines v visualizing [samples frame]
  pure
    [ (row, column)
    | (row, l) <- zip [0 ..] ls
    , (column, c) <- zip [0 ..] (T.unpack l)
    , c /= ' '
    ]

isBraille :: Char -> Bool
isBraille c = c >= '\x2801' && c <= '\x28ff'

style :: T.Text -> Style
style = either (error . T.unpack) id . parseStyle

-- | A frame of samples of the left and the right channel, as MPD writes
-- them.
samples :: [(Int16, Int16)] -> BS.ByteString
samples = BL.toStrict . BB.toLazyByteString . foldMap (\(l, r) -> sample l <> sample r)
  where
    sample :: Int16 -> BB.Builder
    sample = case targetByteOrder of
      LittleEndian -> BB.int16LE
      BigEndian -> BB.int16BE

-- | The samples of a sound from an index until an end, the same in both
-- channels.
sound :: (Int -> Double) -> Int -> Int -> BS.ByteString
sound f from end = samples [(s, s) | i <- [from .. end - 1], let s = round (f i)]

-- | A frame with a period of a sine on the left and a cosine on the right,
-- which is a circle.
circle :: BS.ByteString
circle =
  samples
    [ (scaled (sin a), scaled (cos a))
    | i <- [0 .. n - 1]
    , let a = 2 * pi * fromIntegral i / fromIntegral n
    ]
  where
    n :: Int
    n = 44100 `div` 60

    scaled :: Double -> Int16
    scaled = round . (* 30000)

source :: TVar (Maybe Visualization) -> TQueue AppEvent -> VisualizerSource
source reading events =
  VisualizerSource
    { fps = 60
    , reading = reading
    , emit = \e -> True <$ atomically (writeTQueue events e)
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
    }

-- | The worker's events in a scene, with their times. Time passes only when
-- the worker sleeps, so the scene runs at once.
runScene :: Scene -> IO [(Double, AppEvent)]
runScene sc = do
  timeRef <- S.newIORef 0
  writesRef <- S.newIORef sc.writes
  eventsRef <- S.newIORef []
  reading <- newTVarIO (Just sc.visualization)
  let src =
        VisualizerSource
          { fps = 60
          , reading = reading
          , emit = \e -> do
              t <- S.readIORef timeRef
              S.modifyIORef eventsRef ((t, e) :)
              pure sc.taken
          , debug = sc.debug
          }
  void
    . try @SceneEnded
    . runEff
    . runScriptedClock timeRef
    . runScriptedFifo timeRef writesRef
    $ visualizerWorker src
  reverse <$> S.readIORef eventsRef
  where
    -- A sleep moves the time to its end, or ends the scene.
    runScriptedClock :: IOE :> es => S.IORef Double -> Eff (Clock : es) a -> Eff es a
    runScriptedClock timeRef = interpret_ $ \case
      MonotonicTime -> liftIO $ S.readIORef timeRef
      SleepUntil t
        | t > sc.seconds -> liftIO $ throwIO SceneEnded
        | otherwise -> liftIO . S.modifyIORef timeRef $ \now -> max now t + sc.lateness t

    -- A read takes the writes that came until the time.
    runScriptedFifo
      :: IOE :> es
      => S.IORef Double -> S.IORef [(Double, BS.ByteString)] -> Eff (Fifo : es) a -> Eff es a
    runScriptedFifo timeRef writesRef = interpret_ $ \case
      OpenFifo -> pure ()
      ReadFifo -> liftIO $ do
        now <- S.readIORef timeRef
        (came, later) <- span ((<= now) . fst) <$> S.readIORef writesRef
        S.writeIORef writesRef later
        pure . BS.concat $ map snd came
      CloseFifo -> pure ()

data SceneEnded = SceneEnded
  deriving stock (Show)
  deriving anyclass (Exception)

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

key :: T.Text -> AppEvent
key = KeyPressed . either (error . T.unpack) id . parseKeySpec
