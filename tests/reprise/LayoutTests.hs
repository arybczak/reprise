module LayoutTests (layoutTests) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Graphics.Vty qualified as V
import Optics.Core
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Format
import Reprise.Keys
import Reprise.Mpd.Protocol.Types
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
    , snapshot "queue-classic" $ press ["t", "d"] =<< playing (80, 12)
    , snapshot "which-key" $ press ["t"] =<< playing (80, 12)
    , snapshot "confirm" $ press ["c", "c"] =<< playing (80, 12)
    , snapshot "stopped" $ testState (80, 8) (statusOf Stopped Nothing 2) (take 2 queue)
    , snapshot "empty" $ testState (80, 6) (statusOf Stopped Nothing 0) []
    , snapshot "disconnected" $
        (.state) <$> (runEvents 0 [MpdDisconnected "gone"] =<< playing (80, 12))
    , snapshot "browser" $ browsing [(["2"], rootReply)]
    , snapshot "browser-directory" albums
    , snapshot "browser-columns" $ press ["t", "d"] =<< albums
    , testCase "the titles of the columns in the browser" test_browserTitles
    , snapshot "help" $ press ["f1"] =<< playing (80, 24)
    , snapshot "help-scrolled" $ press ["f1", "page_down"] =<< playing (80, 24)
    , testCase "the help's styles" test_helpStyles
    , testCase "flags end a column before the edge" test_flags
    , testCase "the title has its own style" test_titleStyle
    , testCase "the queue's title scrolls" test_queueTitleScrolls
    , testCase "the marker of a missing tag in columns" test_markerInColumns
    , testCase "the marker of a missing tag in the classic display" test_markerInClassic
    , testCase "a song without a length" test_noLength
    , testCase "a selected song has the selected style" test_selected
    , testCase "a line prompt" test_promptLine
    , testCase "the matches of a find" test_foundStyle
    , testCase "the matches of a find in the browser" test_browserFoundStyle
    , testCase "the songs in the queue in the browser" test_queuedStyle
    ]

test_promptLine :: Assertion
test_promptLine = do
  s <- press ["r", "v", "4", "0", "left"] =<< playing (40, 12)
  assertEqual
    "the line and the hint"
    (Just (True, True))
    ( (\l -> (":volume 40 " `T.isPrefixOf` l, " set volume to 40" `T.isSuffixOf` l))
        <$> lastLine s
    )
  assertEqual "the cursor" (Just (9, 11)) (promptCursor s)
  narrow <- press ["r", "v", "4", "0"] =<< playing (20, 12)
  assertEqual
    "the hint gives way to the line"
    (Just (True, True))
    ((\l -> (":volume 40 " `T.isPrefixOf` l, " set vol…" `T.isSuffixOf` l)) <$> lastLine narrow)
  notFound <- press ["/", "x", "y", "z"] =<< playing (40, 12)
  assertEqual
    "the note"
    (Just (True, True))
    ( (\l -> ("Find forward: xyz " `T.isPrefixOf` l, " no match" `T.isSuffixOf` l))
        <$> lastLine notFound
    )
  assertEqual "no prompt, no cursor" Nothing . promptCursor =<< playing (40, 12)
  -- An empty pattern threw from ICU once.
  assertEqual "an empty find" (Just "Find forward:") . fmap T.stripEnd . lastLine
    =<< press ["/"]
    =<< playing (40, 12)
  where
    lastLine :: AppState -> Maybe T.Text
    lastLine s = case reverse (imageLines (renderScreen testAppEnv s)) of
      l : _ -> Just l
      [] -> Nothing

-- | The rows that match a find in progress have the found style.
test_foundStyle :: Assertion
test_foundStyle = do
  let q = [song i [(Title, [t])] 60 | (i, t) <- zip [0 ..] ["alpha", "beta", "alpha two"]]
  s0 <- press ["/", "a", "l"] =<< testState (80, 10) (statusOf Stopped Nothing 3) q
  let s = s0 & #lastInput .~ s0.now - cursorHideDelay
      underlined title =
        [ V.attrStyle a == V.SetTo V.underline
        | (a, t) <- imageSpans (renderScreen testAppEnv s)
        , T.strip t == title
        ]
  assertEqual "found" [True] (underlined "alpha")
  assertEqual "not found" [False] (underlined "beta")
  accepted <- press ["enter"] s
  let done = accepted & #lastInput .~ accepted.now - cursorHideDelay
  assertEqual
    "only while typing"
    [False]
    [ V.attrStyle a == V.SetTo V.underline
    | (a, t) <- imageSpans (renderScreen testAppEnv done)
    , T.strip t == "alpha"
    ]

test_browserFoundStyle :: Assertion
test_browserFoundStyle = do
  -- The cursor is on the match, and its style combines with the match's.
  s <- press ["/", "S", "i", "n"] =<< browsing [(["2"], rootReply)]
  let underlined name =
        [ case V.attrStyle a of
            V.SetTo st -> V.hasStyle st V.underline
            _ -> False
        | (a, t) <- imageSpans (renderScreen testAppEnv s)
        , T.strip t == name
        ]
  assertEqual "found" [True] (underlined "[Singles]")
  assertEqual "not found" [False] (underlined "[Albums]")

-- | The queue has @dir/1.flac@, but not @dir/9.flac@.
test_queuedStyle :: Assertion
test_queuedStyle = do
  s <-
    browsing
      [ (["2"], ["directory: dir"])
      , (["enter"], ["file: dir/1.flac", "file: dir/9.flac"])
      ]
  let style name =
        [V.attrStyle a | (a, t) <- imageSpans (renderScreen testAppEnv s), T.strip t == name]
  assertEqual "queued" [V.SetTo V.bold] (style "1.flac")
  assertEqual "not queued" [V.Default] (style "9.flac")

test_selected :: Assertion
test_selected = do
  let q = [song 0 [(Title, ["first"])] 60, song 1 [(Title, ["second"])] 60]
  s0 <- press ["space"] =<< testState (80, 6) (statusOf Stopped Nothing 2) q
  let s = s0 & #lastInput .~ s0.now - cursorHideDelay
      background title =
        [ V.attrBackColor a
        | (a, t) <- imageSpans (renderScreen testAppEnv s)
        , title `T.isInfixOf` t
        ]
  -- The selected style is yellow on 24, which vty numbers from 16. vty
  -- joins the columns of the selected row into one span.
  assertEqual "selected" [V.SetTo (V.Color240 8)] (background "first")
  assertEqual "not selected" [V.Default] (background "second")

-- | The key is a value, and its description is text.
test_helpStyles :: Assertion
test_helpStyles = do
  s <- press ["f1"] =<< playing (80, 24)
  let colorOf t =
        [V.attrForeColor a | (a, t') <- imageSpans (renderScreen testAppEnv s), T.strip t' == t]
  assertEqual "the key" [V.SetTo (V.ISOColor 2)] (colorOf "/")
  assertEqual "the description" [V.SetTo (V.ISOColor 3)] (colorOf "find forward")

-- | The marker takes the style of its column, not the marker's style.
test_markerInColumns :: Assertion
test_markerInColumns = do
  let untagged = song 0 [] 60
  -- The artist column has the style 221, which vty numbers from 16.
  assertMarkerColor (marked (Just cyan)) "artist column" (V.Color240 205)
    =<< testState (80, 6) (statusOf Stopped Nothing 1) [untagged]

test_markerInClassic :: Assertion
test_markerInClassic = do
  let noLength = song 0 [(Title, ["t"])] 60 & #duration .~ Nothing
      withLength env =
        env
          & #config
            % #songs
            % #classic
            % #right
            .~ either (error . show) id (parseStyledFormat "<green>%{length}</>")
  s <- press ["t", "d"] =<< testState (80, 6) (statusOf Stopped Nothing 1) [noLength]
  assertMarkerColor (withLength (marked (Just cyan))) "its own style" (V.ISOColor 6) s
  assertMarkerColor (withLength (marked Nothing)) "the style around it" (V.ISOColor 2) s

test_noLength :: Assertion
test_noLength = do
  let noLength = song 0 [(Title, ["t"])] 60 & #duration .~ Nothing
  columns <- testState (80, 6) (statusOf Stopped Nothing 1) [noLength]
  classic <- press ["t", "d"] columns
  forM_ [("columns", columns), ("classic", classic)] $ \(name, s) ->
    case drop 2 (imageLines (renderScreen testAppEnv s)) of
      row : _ -> assertBool (name <> ": " <> T.unpack row) ("-:--" `T.isSuffixOf` row)
      [] -> assertFailure $ name <> ": no rows"

-- | The marker that 'assertMarkerColor' finds, with a style.
marked :: Maybe Style -> AppEnv
marked markerStyle =
  testAppEnv
    & #config % #lists % #missingTag .~ "<empty>"
    & #config % #lists % #missingTagStyle .~ markerStyle

cyan :: Style
cyan = Style (Just (Color 6)) Nothing mempty

-- | The color of the marker, with the cursor hidden, so that its style
-- doesn't cover the row's.
assertMarkerColor :: AppEnv -> String -> V.Color -> AppState -> Assertion
assertMarkerColor env msg color s0 =
  let s = s0 & #lastInput .~ s0.now - cursorHideDelay
  in -- vty joins the marker with the padding after it when their styles match.
     case [ a
          | (a, t) <- imageSpans (renderScreen env s)
          , "<empty>" `T.isPrefixOf` T.stripStart t
          ] of
       a : _ -> assertEqual msg (V.SetTo color) (V.attrForeColor a)
       [] -> assertFailure $ msg <> ": no marker"

test_titleStyle :: Assertion
test_titleStyle = do
  s <- playing (80, 12)
  let titleAttrs = [a | (a, t) <- imageSpans (renderScreen testAppEnv s), "Queue (" `T.isPrefixOf` t]
  assertEqual "bold by default" [V.SetTo V.bold] (map V.attrStyle titleAttrs)
  let red = testAppEnv & #config % #header % #titleStyle .~ Style (Just (Color 1)) Nothing mempty
      redAttrs = [a | (a, t) <- imageSpans (renderScreen red s), "Queue (" `T.isPrefixOf` t]
  assertEqual "configured" [V.SetTo (V.ISOColor 1)] (map V.attrForeColor redAttrs)

-- | The part after "Queue " scrolls, also while nothing plays. It starts at
-- its start, at whatever time reprise starts.
test_queueTitleScrolls :: Assertion
test_queueTitleScrolls = do
  let started = 1000
  r <-
    runEvents
      started
      [ Resized 30 12
      , MpdConnected (Version 0 24 0)
      , QueueFetched (statusOf Stopped Nothing 6, queue)
      ]
      (initialState defaultConfig)
  let titleLine s = case imageLines (renderScreen testAppEnv s) of
        l : _ -> l
        [] -> ""
  assertEqual "a redraw in a second" [1] [d | After d (Tick _) <- r.commands]
  assertBool
    ("the start: " <> T.unpack (titleLine r.state))
    ("Queue (6 songs" `T.isPrefixOf` titleLine r.state)
  let later = r.state & #now .~ started + 2
  assertBool
    ("two seconds later: " <> T.unpack (titleLine later))
    ("Queue  songs" `T.isPrefixOf` titleLine later)

-- | As in ncmpcpp.
test_flags :: Assertion
test_flags = do
  let st = statusOf Playing (Just 1) (length queue) & #repeat .~ True & #random .~ True
  s <- testState (20, 6) st queue
  case imageLines (renderScreen testAppEnv s) of
    _ : flagsLine : _ -> assertEqual "line" "───────────────[rz]─" flagsLine
    ls -> assertFailure $ "too few lines: " <> show ls

-- | Render the screen at a fixed monotonic time and compare its text with a
-- golden file.
snapshot :: String -> IO AppState -> TestTree
snapshot name s =
  goldenVsString name ("tests" </> "reprise" </> "golden" </> name <> ".txt") $
    BL.fromStrict . T.encodeUtf8 . T.unlines . imageLines . renderScreen testAppEnv <$> s

playing :: (Int, Int) -> IO AppState
playing size = testState size (statusOf Playing (Just 1) (length queue)) queue

press :: [T.Text] -> AppState -> IO AppState
press ks s = (.state) <$> runEvents 0 (map (KeyPressed . key) ks) s

-- | The browser after keys, each followed by the reply to the listing they
-- requested.
browsing :: [([T.Text], [BS.ByteString])] -> IO AppState
browsing steps = do
  s0 <- playing (80, 12)
  foldM step s0 steps
  where
    step :: AppState -> ([T.Text], [BS.ByteString]) -> IO AppState
    step s (ks, reply) = do
      r <- runEvents 0 (map (KeyPressed . key) ks) s
      case reverse r.pending of
        p : _ -> (.state) <$> runEvents 0 [replyTo reply p] r.state
        [] -> pure r.state

rootReply :: [BS.ByteString]
rootReply = ["directory: Albums", "directory: Singles", "playlist: Favourites"]

-- | The browser in a directory with an entry of every kind.
albums :: IO AppState
albums =
  browsing
    [ (["2"], rootReply)
    ,
      ( ["enter"]
      ,
        [ "directory: Albums/Live"
        , "file: Albums/01.flac"
        , "Artist: Some Artist"
        , "Title: First Song"
        , "duration: 61.000"
        , "file: Albums/02.flac"
        , "duration: 185.000"
        , "playlist: Albums/album.m3u"
        ]
      )
    ]

test_browserTitles :: Assertion
test_browserTitles = do
  let env = testAppEnv & #config % #songs % #columns % #showTitles .~ True
  inAlbums <- albums
  columns <- press ["t", "d"] inAlbums
  case imageLines (renderScreen env columns) of
    _ : _ : titles : parent : _ -> do
      assertBool ("titles: " <> T.unpack titles) ("Title" `T.isInfixOf` titles)
      assertEqual "the first item" ".." parent
    ls -> assertFailure $ "too few lines: " <> show ls
  case imageLines (renderScreen env inAlbums) of
    _ : _ : first : _ -> assertEqual "no titles in the classic display" ".." first
    ls -> assertFailure $ "too few lines: " <> show ls

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
