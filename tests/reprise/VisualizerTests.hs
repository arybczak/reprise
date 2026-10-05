module VisualizerTests (visualizerTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Style
import Reprise.UI.Layout
import Reprise.Visualizer.Worker
import Utils

visualizerTests :: TestTree
visualizerTests =
  testGroup
    "Visualizer"
    [ testCase "showing the visualizer reads the samples, leaving it stops" test_reading
    , testCase "without a data source, the screen says how to set one" test_noSource
    , testCase "samples after the visualizer stopped are dropped" test_staleSamples
    , testCase "the frames of the trail stay" test_trail
    , testCase "silence after silence keeps the screen" test_silence
    , testCase "no room for the visualizer" test_noRoom
    , testCase "mono is a vertical line" test_mono
    , testCase "one channel alone is a diagonal" test_oneChannel
    , testCase "older frames fade through the colors" test_fading
    , goldenVsString
        "a circle"
        ("tests" </> "reprise" </> "golden" </> "visualizer.txt")
        (BL.fromStrict . T.encodeUtf8 . T.unlines <$> mainLines visualizing [circle])
    , testCase "the worker sends the samples of the fifo" test_worker
    , testCase "the worker reports a data source that it can't read" test_workerFails
    ]

test_reading :: Assertion
test_reading = do
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  shown <- runEventsWith visualizing 0 [key "8"] s
  assertEqual "started" [Visualize True] [c | c@(Visualize _) <- shown.commands]
  left <- runEventsWith visualizing 0 [key "1"] shown.state
  assertEqual "stopped" [Visualize False] [c | c@(Visualize _) <- left.commands]

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
  assertEqual "no frames" Seq.empty r.state.visualizer.frames

-- | 50 ms at 60 frames a second are 3 frames.
test_trail :: Assertion
test_trail = do
  let env = visualizing & #config % #visualizer % #trail .~ 0.05
  r <- shownWith env (replicate 5 circle)
  assertEqual "frames" 3 (Seq.length r.visualizer.frames)

test_silence :: Assertion
test_silence = do
  s <- shownWith visualizing []
  r <- runEventsWith visualizing 0 [VisualizerSamples BS.empty] s
  assertEqual "the screen stays" [KeepScreen] r.commands
  faded <- shownWith visualizing [circle, BS.empty]
  assertEqual
    "the screen changes while the samples fade"
    2
    (Seq.length faded.visualizer.frames)

-- | The header and the bars take the 4 rows.
test_noRoom :: Assertion
test_noRoom = do
  s <- testState (40, 4) (statusOf Stopped Nothing 0) []
  r <- runEventsWith visualizing 0 [key "8", VisualizerSamples circle] s
  assertEqual "the lines" 4 (length (imageLines (renderScreen visualizing r.state)))

test_mono :: Assertion
test_mono = do
  ls <-
    mainLines visualizing [samples [(v, v) | v <- [minBound, minBound + 1000 .. maxBound]]]
  let columns = [c | l <- ls, (c, ch) <- zip [0 :: Int ..] (T.unpack l), ch /= ' ']
  assertBool "dots on several rows" (length columns > 1)
  case columns of
    c : _ -> assertEqual "one column" [c] (L.nub columns)
    [] -> assertFailure "no dots"

test_oneChannel :: Assertion
test_oneChannel = do
  ls <-
    mainLines visualizing [samples [(v, 0) | v <- [minBound, minBound + 1000 .. maxBound]]]
  let dots =
        [ (row, c)
        | (row, l) <- zip [0 :: Int ..] ls
        , (c, ch) <- zip [0 :: Int ..] (T.unpack l)
        , ch /= ' '
        ]
  case (dots, reverse dots) of
    ((_, top) : _, (_, bottom) : _) -> assertBool "up to the right" (top > bottom)
    _ -> assertFailure "no dots"

-- | With two colors and two frames on the screen, the newer frame has the
-- first color and the older one the second.
test_fading :: Assertion
test_fading = do
  let env =
        visualizing
          & #config % #visualizer % #trail .~ 2 / 60
          & #config % #visualizer % #colors .~ (style "red" NE.:| [style "blue"])
      older = samples [(maxBound, maxBound)]
      newer = samples [(minBound, minBound)]
  s <- shownWith env [older, newer]
  let colorsOf = [V.attrForeColor a | (a, t) <- imageSpans (renderScreen env s), T.any isBraille t]
  assertEqual "colors" [V.SetTo (V.ISOColor 4), V.SetTo (V.ISOColor 1)] colorsOf
  where
    isBraille :: Char -> Bool
    isBraille c = c >= '\x2801' && c <= '\x28ff'

    style :: T.Text -> Style
    style = either (error . T.unpack) id . parseStyle

test_worker :: Assertion
test_worker = withSystemTempDirectory "visualizer" $ \dir -> do
  let path = dir </> "fifo"
  createNamedPipe path (unionFileModes ownerReadMode ownerWriteMode)
  events <- newTQueueIO
  reading <- newTVarIO True
  let frameBytes = 4
      samplesBytes = frameBytes * (44100 `div` 60)
      pattern = samples (replicate 100 (1000, -1000))
  bracket (forkIO . visualizerWorker $ source path reading events) killThread $ \_ -> do
    -- The writer can open the fifo once the worker opened it, which also
    -- drops what the fifo held before, so the samples go on until they come.
    writer <- forkIO . withWriter path $ \h -> forever $ do
      BS.hPut h pattern
      threadDelay writeInterval
    frame <- (`finally` killThread writer) . expectWithin $ firstSamples events
    assertBool
      ("whole samples: " <> show (BS.length frame))
      (BS.length frame `mod` frameBytes == 0)
    assertBool "a frame long at most" (BS.length frame <= samplesBytes)
    assertBool "the samples" (frame `BS.isPrefixOf` BS.concat (replicate 100 pattern))
    atomically $ writeTVar reading False
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

    -- Opening a fifo without a reader fails, so it opens again until the
    -- worker reads it.
    withWriter :: FilePath -> (Handle -> IO ()) -> IO ()
    withWriter path k =
      try @IOException (openBinaryFile path WriteMode) >>= \case
        Left _ -> threadDelay writeInterval >> withWriter path k
        Right h -> k h `finally` hClose h

    -- A tenth of a frame at 60 frames a second.
    writeInterval :: Int
    writeInterval = 1000000 `div` 600

    frameInterval :: Int
    frameInterval = 1000000 `div` 60

test_workerFails :: Assertion
test_workerFails = withSystemTempDirectory "visualizer" $ \dir -> do
  events <- newTQueueIO
  reading <- newTVarIO True
  bracket (forkIO . visualizerWorker $ source (dir </> "missing") reading events) killThread $ \_ ->
    expectWithin (atomically (readTQueue events)) >>= \case
      VisualizerFailed _ -> pure ()
      e -> assertFailure $ "event: " <> show e

----------------------------------------
-- Helpers

-- | The default config with a data source.
visualizing :: AppEnv
visualizing = testAppEnv & #config % #visualizer % #dataSource ?~ "fifo"

-- | The state after the visualizer showed the frames, the oldest first.
shownWith :: AppEnv -> [BS.ByteString] -> IO AppState
shownWith env frames = do
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  (.state) <$> runEventsWith env 0 (key "8" : map VisualizerSamples frames) s

-- | The lines of the main area, after the header's two, once the visualizer
-- showed the frames.
mainLines :: AppEnv -> [BS.ByteString] -> IO [T.Text]
mainLines env frames = do
  s <- shownWith env frames
  pure . take (mainHeight s.terminalSize) . drop 2 $ imageLines (renderScreen env s)

-- | A frame of samples of the left and the right channel, as MPD writes
-- them.
samples :: [(Int16, Int16)] -> BS.ByteString
samples = BL.toStrict . BB.toLazyByteString . foldMap (\(l, r) -> sample l <> sample r)
  where
    sample :: Int16 -> BB.Builder
    sample = case targetByteOrder of
      LittleEndian -> BB.int16LE
      BigEndian -> BB.int16BE

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

source :: FilePath -> TVar Bool -> TQueue AppEvent -> VisualizerSource
source path reading events =
  VisualizerSource
    { path = path
    , channels = 2
    , fps = 60
    , reading = reading
    , emit = atomically . writeTQueue events
    }

-- | Wait for what comes soon.
expectWithin :: IO a -> IO a
expectWithin act = timeout (5 * 1000000) act >>= maybe (assertFailure "nothing came") pure

key :: T.Text -> AppEvent
key = KeyPressed . either (error . T.unpack) id . parseKeySpec
