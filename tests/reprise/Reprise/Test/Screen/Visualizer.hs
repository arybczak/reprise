module Reprise.Test.Screen.Visualizer (visualizerTests) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector.Storable qualified as VS
import Graphics.Vty qualified as V
import Optics.Core
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Test.Helpers
import Reprise.Test.Visualizer.Helpers
import Reprise.UI.Layout
import Reprise.Visualizer.Spectrum
import Reprise.Width

visualizerTests :: TestTree
visualizerTests =
  testGroup
    "visualizer"
    [ testCase "showing the visualizer reads the samples, leaving it stops" test_reading
    , testCase "space switches the visualization" test_switch
    , testCase "the bars of the spectrum" test_bars
    , testCase "the colors of a bar blend from its foot to its top" test_barColors
    , testCase "a row is as few texts as it can be" test_runs
    , testCase "without a data source, the screen says how to set one" test_noSource
    , testCase "frames after their visualization stopped are dropped" test_staleFrames
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
    , goldenVsString
        "a period of a sine and a cosine"
        ("tests" </> "reprise" </> "golden" </> "visualizer-wave.txt")
        (BL.fromStrict . T.encodeUtf8 . T.unlines <$> mainLines Wave visualizing [circle])
    , testCase "the screen says why it can't read the data source" test_failureShows
    , testCase "the debug line shows what happened to the frames" test_debugLine
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

-- | The frames of a visualization that come after another visualization or
-- another screen showed.
test_staleFrames :: Assertion
test_staleFrames =
  forM_ [minBound .. maxBound] $ \v -> do
    shown <- shownWith v visualizing []
    forM_ [("another visualization", "space"), ("another screen", "1")] $ \(what, k) -> do
      let late = show v <> " after " <> what
      stopped <- runEventsWith visualizing 0 [key k] shown
      r <- runEventsWith visualizing 0 [frameOf v] stopped.state
      assertEqual (late <> ": the screen stays") [KeepScreen] r.commands
      assertEqual
        (late <> ": the picture stays")
        stopped.state.visualizer.reading
        r.state.visualizer.reading
  where
    frameOf :: Visualization -> AppEvent
    frameOf = \case
      Spectrum -> VisualizerSpectrum (VS.singleton 1) (VS.singleton 1)
      Ellipse -> VisualizerSamples circle
      Wave -> VisualizerWave circle

-- | 50 ms at 60 frames a second are 3 frames.
test_trail :: Assertion
test_trail = do
  let env = visualizing & #config % #visualizer % #trail .~ 0.05
  r <- shownWith Ellipse env (replicate 5 circle)
  assertEqual "frames" 3 (Seq.length (ellipseFrames r))

-- | The trail of 50 ms holds 3 frames, so 3 silent ones push the circle out.
test_silence :: Assertion
test_silence = do
  let env = visualizing & #config % #visualizer % #trail .~ 0.05
  s <- shownWith Ellipse env (circle : replicate 3 BS.empty)
  assertEqual "only silence" (Seq.replicate 3 BS.empty) (ellipseFrames s)
  r <- runEventsWith env 0 [VisualizerSamples BS.empty] s
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
      Spectrum -> error "the frames of a spectrum aren't samples"
      Ellipse -> VisualizerSamples
      Wave -> VisualizerWave

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

key :: T.Text -> AppEvent
key = KeyPressed . either (error . T.unpack) id . parseKeySpec
