module LayoutTests (layoutTests) where

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Graphics.Vty qualified as V
import MPD.Types
import Optics.Core
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Reprise.Config
import Reprise.Event
import Reprise.Handler
import Reprise.Keys
import Reprise.State
import Reprise.Style
import Reprise.UI.Layout
import Utils

layoutTests :: TestTree
layoutTests =
  testGroup
    "Layout"
    [ snapshot "queue-columns" $ playing (80, 12)
    , snapshot "queue-narrow" $ playing (40, 12)
    , snapshot "queue-classic" $ press ["ctrl-t", "d"] (playing (80, 12))
    , snapshot "which-key" $ press ["ctrl-t"] (playing (80, 12))
    , snapshot "confirm" $ press ["ctrl-q", "c"] (playing (80, 12))
    , snapshot "stopped" $ testState (80, 8) (statusOf Stopped Nothing 2) (take 2 queue)
    , snapshot "empty" $ testState (80, 6) (statusOf Stopped Nothing 0) []
    , snapshot "disconnected" . (.state) $
        runEvents 0 [MpdDisconnected "gone"] (playing (80, 12))
    , snapshot "help" $ press ["f1"] (playing (80, 24))
    , snapshot "help-scrolled" $ press ["f1", "page_down"] (playing (80, 24))
    , testCase "flags end a column before the edge" test_flags
    , testCase "the title has its own style" test_titleStyle
    , testCase "the marker of a missing tag in columns" test_markerInColumns
    , testCase "the marker of a missing tag in the classic display" test_markerInClassic
    ]

-- | The marker takes the style of its column, not the marker's style.
test_markerInColumns :: Assertion
test_markerInColumns = do
  let untagged = song 0 [] 60
  -- The artist column has the style 221, which vty numbers from 16.
  assertMarkerColor "artist column" (V.Color240 205) $
    testState (80, 6) (statusOf Stopped Nothing 1) [untagged]

test_markerInClassic :: Assertion
test_markerInClassic = do
  let noLength = song 0 [(Title, ["t"])] 60 & #duration .~ Nothing
  -- The marker's style is cyan.
  assertMarkerColor "length" (V.ISOColor 6) . press ["ctrl-t", "d"] $
    testState (80, 6) (statusOf Stopped Nothing 1) [noLength]

-- | The color of the marker, with the cursor hidden, so that its style
-- doesn't cover the row's.
assertMarkerColor :: String -> V.Color -> AppState -> Assertion
assertMarkerColor msg color s0 =
  let s = s0 & #lastInput .~ s0.now - cursorHideDelay
  in -- vty joins the marker with the padding after it when their styles match.
     case [a | (a, t) <- imageSpans (renderScreen s), "<empty>" `T.isPrefixOf` T.stripStart t] of
       a : _ -> assertEqual msg (V.SetTo color) (V.attrForeColor a)
       [] -> assertFailure $ msg <> ": no marker"

test_titleStyle :: Assertion
test_titleStyle = do
  let s = playing (80, 12)
      titleAttrs = [a | (a, t) <- imageSpans (renderScreen s), "Queue (" `T.isPrefixOf` t]
  assertEqual "bold by default" [V.SetTo V.bold] (map V.attrStyle titleAttrs)
  let red = s & #config % #header % #titleStyle .~ Style (Just (Color 1)) Nothing mempty
      redAttrs = [a | (a, t) <- imageSpans (renderScreen red), "Queue (" `T.isPrefixOf` t]
  assertEqual "configured" [V.SetTo (V.ISOColor 1)] (map V.attrForeColor redAttrs)

-- | As in ncmpcpp.
test_flags :: Assertion
test_flags = do
  let st = statusOf Playing (Just 1) (length queue) & #repeat .~ True & #random .~ True
  case imageLines . renderScreen $ testState (20, 6) st queue of
    _ : flagsLine : _ -> assertEqual "line" "───────────────[rz]─" flagsLine
    ls -> assertFailure $ "too few lines: " <> show ls

-- | Render the screen at a fixed monotonic time and compare its text with a
-- golden file.
snapshot :: String -> AppState -> TestTree
snapshot name s =
  goldenVsString name ("tests" </> "golden" </> name <> ".txt")
    $ pure . BL.fromStrict . T.encodeUtf8 . T.unlines . imageLines
    $ renderScreen s

playing :: (Int, Int) -> AppState
playing size = testState size (statusOf Playing (Just 1) (length queue)) queue

press :: [T.Text] -> AppState -> AppState
press ks s = (runEvents 0 (map (KeyPressed . key) ks) s).state
  where
    key :: T.Text -> KeySpec
    key = either (error . T.unpack) id . parseKeySpec

queue :: [Song]
queue =
  [ song
      0
      [ (Artist, ["Some Artist"])
      , (Album, ["Great Album"])
      , (Title, ["First Song"])
      , (Track, ["1"])
      , (Date, ["2001"])
      ]
      61
  , song
      1
      [ (Artist, ["Some Artist"])
      , (Album, ["Great Album"])
      , (Title, ["Second Song"])
      , (Track, ["2"])
      , (Date, ["2001"])
      ]
      185
  , song
      2
      [ (Artist, ["Some Artist"])
      , (Album, ["Great Album"])
      , (Title, ["A third song with a title that doesn't fit in its column"])
      , (Track, ["3"])
      ]
      240
  , song
      3
      [ (Artist, ["Zespół", "Gość"])
      , (Album, ["Płyta"])
      , (Title, ["Zażółć gęślą jaźń"])
      , (Track, ["1/12"])
      ]
      3725
  , song 4 [(Artist, ["日本のバンド"]), (Title, ["曲"])] 30
  , song 5 [] 5
  ]
