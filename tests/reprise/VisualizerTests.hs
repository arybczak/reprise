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
import Data.Vector.Storable qualified as VS
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
import Reprise.Visualizer.Spectrum
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
        (BL.fromStrict . T.encodeUtf8 . T.unlines <$> mainLines visualizing [circle])
    , testCase "the spectrum of a sine" test_sineSpectrum
    , testCase "the worker sends the samples of the fifo" test_worker
    , testCase "the worker sends the spectrum" test_workerSpectrum
    , testCase "the worker reports a data source that it can't read" test_workerFails
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
  r <- runEventsWith visualizing 0 [key "8", key "space"] s
  assertEqual
    "the samples"
    [Visualize (Just Spectrum), Visualize (Just Ellipse)]
    [c | c@(Visualize _) <- r.commands]
  assertEqual "the message" (Just "Visualization: ellipse") ((.text) <$> r.state.message)
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
  mono <- (.state) <$> runEventsWith visualizing 0 [key "8", VisualizerSpectrum [full]] s
  let cells = filled (mainLinesOf mono)
  assertEqual "a column" [22] (L.nub (map snd cells))
  assertEqual "full" 8 (length cells)
  stereo <-
    (.state) <$> runEventsWith visualizing 0 [key "8", VisualizerSpectrum [full, silent]] s
  assertEqual
    "rising in the top half"
    [(r, 22) | r <- [0 .. 3]]
    (filled (mainLinesOf stereo))
  hanging <-
    (.state) <$> runEventsWith visualizing 0 [key "8", VisualizerSpectrum [silent, full]] s
  assertEqual
    "hanging in the bottom half"
    [(r, 22) | r <- [4 .. 7]]
    (filled (mainLinesOf hanging))
  -- -46 dB is 0.6 of the way from -100 dB to -10 dB, so 2.4 of the 4 rows.
  let partly = VS.map (* (10 ** (-46 / 20))) full
  ending <-
    (.state) <$> runEventsWith visualizing 0 [key "8", VisualizerSpectrum [silent, partly]] s
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
  stayed <- shownWith visualizing [circle, BS.empty]
  assertEqual
    "the screen changes while samples are on it"
    2
    (Seq.length stayed.visualizer.frames)

-- | The header and the bars take the 4 rows.
test_noRoom :: Assertion
test_noRoom = do
  s <- testState (40, 4) (statusOf Stopped Nothing 0) []
  r <- runEventsWith visualizing 0 [key "8", key "space", VisualizerSamples circle] s
  assertEqual "the lines" 4 (length (imageLines (renderScreen visualizing r.state)))

test_mono :: Assertion
test_mono = do
  dots <- dotsOf [(v, v) | v <- [minBound, minBound + 1000 .. maxBound]]
  assertBool "dots on several rows" (length (L.nub (map fst dots)) > 1)
  case (dots, reverse dots) of
    ((_, top) : _, (_, bottom) : _) -> assertBool "up to the right" (top > bottom)
    _ -> assertFailure "no dots"

test_oneChannel :: Assertion
test_oneChannel = do
  dots <- dotsOf [(v, 0) | v <- [minBound, minBound + 1000 .. maxBound]]
  assertBool "dots in several columns" (length (L.nub (map snd dots)) > 1)
  assertEqual "one row" 1 (length (L.nub (map fst dots)))

-- | With two colors, a quiet sample in the center has the first, and a
-- loud one in the top right corner the second.
test_loudness :: Assertion
test_loudness = do
  let env = visualizing & #config % #visualizer % #colors .~ (style "red" NE.:| [style "blue"])
  s <- shownWith env [samples [(1000, 1000), (maxBound, maxBound)]]
  let colorsOf = [V.attrForeColor a | (a, t) <- imageSpans (renderScreen env s), T.any isBraille t]
  assertEqual "colors" [V.SetTo (V.ISOColor 4), V.SetTo (V.ISOColor 1)] colorsOf
  where
    isBraille :: Char -> Bool
    isBraille c = c >= '\x2801' && c <= '\x28ff'

    style :: T.Text -> Style
    style = either (error . T.unpack) id . parseStyle

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
  left <- spectrumOf transform 2 0 sine
  right <- spectrumOf transform 2 1 sine
  let peak = VS.maxIndex left
  assertBool
    ("the peak at " <> show (binFrequency peak) <> " Hz")
    (abs (binFrequency peak - frequency) <= binFrequency 1)
  assertBool
    ("its magnitude " <> show (left VS.! peak))
    (abs (left VS.! peak - amplitude * 0.42 / 2) < 0.01)
  assertEqual "the silent channel" 0 (VS.maximum right)

test_worker :: Assertion
test_worker = withSystemTempDirectory "visualizer" $ \dir -> do
  let path = dir </> "fifo"
  createNamedPipe path (unionFileModes ownerReadMode ownerWriteMode)
  events <- newTQueueIO
  reading <- newTVarIO (Just Ellipse)
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

-- | The spectrum comes without samples in the fifo, of the silence that the
-- window starts with.
test_workerSpectrum :: Assertion
test_workerSpectrum = withSystemTempDirectory "visualizer" $ \dir -> do
  let path = dir </> "fifo"
  createNamedPipe path (unionFileModes ownerReadMode ownerWriteMode)
  events <- newTQueueIO
  reading <- newTVarIO (Just Spectrum)
  bracket (forkIO . visualizerWorker $ source path reading events) killThread $ \_ ->
    expectWithin (atomically (readTQueue events)) >>= \case
      VisualizerSpectrum spectra -> do
        assertEqual "the channels" 2 (length spectra)
        assertEqual "the bins" [32768 `div` 2 + 1, 32768 `div` 2 + 1] (map VS.length spectra)
        assertEqual "silence" [0, 0] (map VS.maximum spectra)
      e -> assertFailure $ "event: " <> show e

test_workerFails :: Assertion
test_workerFails = withSystemTempDirectory "visualizer" $ \dir -> do
  events <- newTQueueIO
  reading <- newTVarIO (Just Ellipse)
  bracket (forkIO . visualizerWorker $ source (dir </> "missing") reading events) killThread $ \_ ->
    expectWithin (atomically (readTQueue events)) >>= \case
      VisualizerFailed _ -> pure ()
      e -> assertFailure $ "event: " <> show e

----------------------------------------
-- Helpers

-- | The default config with a data source.
visualizing :: AppEnv
visualizing = testAppEnv & #config % #visualizer % #dataSource ?~ "fifo"

-- | The state after the ellipse showed the frames, the oldest first.
shownWith :: AppEnv -> [BS.ByteString] -> IO AppState
shownWith env frames = do
  s <- testState (40, 12) (statusOf Stopped Nothing 0) []
  (.state) <$> runEventsWith env 0 (key "8" : key "space" : map VisualizerSamples frames) s

-- | The lines of the main area, after the header's two, once the visualizer
-- showed the frames.
mainLines :: AppEnv -> [BS.ByteString] -> IO [T.Text]
mainLines env frames = do
  s <- shownWith env frames
  pure . take (mainHeight s.terminalSize) . drop 2 $ imageLines (renderScreen env s)

-- | The rows and the columns of the cells with dots, from the top, after
-- the visualizer showed a frame of the samples.
dotsOf :: [(Int16, Int16)] -> IO [(Int, Int)]
dotsOf frame = do
  ls <- mainLines visualizing [samples frame]
  pure
    [ (row, column)
    | (row, l) <- zip [0 ..] ls
    , (column, c) <- zip [0 ..] (T.unpack l)
    , c /= ' '
    ]

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

source :: FilePath -> TVar (Maybe Visualization) -> TQueue AppEvent -> VisualizerSource
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
