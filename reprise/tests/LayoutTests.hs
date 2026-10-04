module LayoutTests (layoutTests) where

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import MPD.Types
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Reprise.Event
import Reprise.Keys
import Reprise.State
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
    , snapshot "help" $ press ["f1"] (playing (80, 24))
    , snapshot "help-scrolled" $ press ["f1", "page_down"] (playing (80, 24))
    ]

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
