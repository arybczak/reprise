module LyricsTests (lyricsTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.IORef.Strict qualified as S
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Graphics.Vty qualified as V
import Optics.Core
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.App
import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Format
import Reprise.Keys
import Reprise.Lyrics
import Reprise.Lyrics.Worker
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Lyrics
import Reprise.State
import Reprise.UI.Layout
import Utils

lyricsTests :: TestTree
lyricsTests =
  testGroup
    "Lyrics"
    [ testCase "the file names are ncmpcpp's" test_fileNames
    , testCase "a name too long for a file is cut" test_longFileNames
    , testCase "the name of a long title stays" test_longName
    , testCase "l shows the lyrics of the song under the cursor" test_show
    , testCase "fetched lyrics and the other outcomes" test_outcomes
    , testCase "l on the lyrics goes back" test_back
    , testCase "nowhere to go back to" test_nowhereBack
    , testCase "back from any screen" test_backAnywhere
    , testCase "l without a song under the cursor" test_noSong
    , testCase "the lyrics of a song that the screen left are dropped" test_stale
    , testCase "the lyrics scroll" test_scroll
    , testCase "` fetches the lyrics again" test_refetch
    , testCase "the worker reads the stored lyrics" test_worker
    , testCase "the worker fetches and stores the lyrics" test_workerFetches
    , testCase "the worker asks again for what wasn't there" test_workerAsksAgain
    , testCase "the worker asks the fetchers in order" test_workerFetchers
    , testCase "the worker fetches the lyrics again" test_workerRefetches
    , testCase "a refetch that finds nothing keeps the stored lyrics" test_workerKeepsStored
    , testCase "LRC without times is stored lyrics" test_workerUntimedLrc
    , testCase "the worker without fetchers" test_workerWithoutFetchers
    , testCase "timed lyrics from LRC" test_parseLrc
    , testCase "the worker stores and reads the times" test_workerTimes
    , testCase "the line being sung" test_sung
    , testCase "scrolling stops following the song" test_stopFollowing
    , testCase "a find stops following the song" test_findStopsFollowing
    , testCase "o on another song's lyrics" test_jumpToPlayingLyrics
    , testCase "a redraw when the next line is sung" test_nextLine
    , testCase "the worker fetches in the background" test_workerInBackground
    , testCase "a request of the screen doesn't wait" test_workerTakesTurns
    , testCase "a request of the screen takes over its song's fetch" test_workerTakesOver
    , testCase "the lyrics of each new song that plays are fetched" test_fetchInBackground
    , testCase "the lyrics follow the song that plays" test_followPlaying
    , testCase "a stream has no lyrics" test_stream
    , testCase "the next song's lyrics show from their top" test_followFromTop
    , testCase "a control character is a space" test_controlCharacters
    , testCase "the title scrolls from its start for the next song" test_followTitle
    , testCase "space turns following the song on and off" test_toggleFollowing
    , testCase "e edits the lyrics" test_edit
    , testCase "the lyrics show again after the editor" test_edited
    , testCase "the editor gets the file as an argument" test_runEditor
    ]

-- | The first is the name of a file in the author's lyrics from ncmpcpp.
test_fileNames :: Assertion
test_fileNames = do
  let named artists titles = lyricsFileName (song 3 [(Artist, artists), (Title, titles)] 60)
  assertEqual
    "artist and title"
    "Akira Yamaoka - Letter - from the Lost Days.txt"
    (named ["Akira Yamaoka"] ["Letter - from the Lost Days"])
  assertEqual "the first artist" "A - T.txt" (named ["A", "B"] ["T"])
  assertEqual
    "without the characters that Windows forbids"
    "ACDC - Whats Next Re Stacks.txt"
    (named ["AC/DC"] ["Whats Next? Re: Stacks"])
  assertEqual "without a title, the file" "3.txt" (named ["A"] [])
  assertEqual "with an empty artist, the file" "3.txt" (named [""] ["T"])
  assertEqual
    "the times, with a point in the title"
    "A - Mr. Blue.lrc"
    (timedLyricsFileName (song 3 [(Artist, ["A"]), (Title, ["Mr. Blue"])] 60))

-- | The longest name in the author's library, of 285 bytes, and names of
-- characters of two bytes and of four.
test_longFileNames :: Assertion
test_longFileNames = do
  let handel =
        "14. Recitative (Soprano) There were shepherds abiding in the field, keeping "
          <> "watch over their flock by night. And lo, the angel of the Lord came upon "
          <> "them, and the glory of the Lord shone round about them, and they were sore "
          <> "afraid (Messiah, HWV 56)"
      named title = lyricsFileName (song 3 [(Artist, ["George Frideric Handel"]), (Title, [title])] 60)
      bytes = BS.length . T.encodeUtf8 . T.pack
      long = named handel
  assertBool ("cut: " <> show (bytes long)) (bytes long <= 255)
  assertBool
    "the start"
    ("George Frideric Handel - 14. Recitative (Soprano)" `L.isPrefixOf` long)
  assertEqual
    "the same for the times"
    (dropExtension long)
    ( dropExtension
        (timedLyricsFileName (song 3 [(Artist, ["George Frideric Handel"]), (Title, [handel])] 60))
    )
  assertBool "apart from a name that begins alike" (named (handel <> " 2") /= long)
  forM_ [T.replicate 200 "ż", T.replicate 100 "🎵"] $ \title -> do
    let name = named title
    assertBool
      ("cut: " <> show (bytes name))
      (bytes name <= 255 && ".txt" `L.isSuffixOf` name)

-- | The name of a long title is the same in every version, as stored
-- lyrics are found by it.
test_longName :: Assertion
test_longName =
  assertEqual
    "the name"
    ("A - " <> replicate 230 'a' <> " c98f27d3cedf8c0d.txt")
    (lyricsFileName (song 3 [(Artist, ["A"]), (Title, [T.replicate 300 "a"])] 60))

test_show :: Assertion
test_show = do
  r <- runEvents 0 [key "l"] =<< queueShown
  assertEqual "the screen" LyricsScreen (focusedView r.state).screen
  case [(t, q) | FetchLyrics t q <- r.commands] of
    [(token, requested)] -> do
      assertEqual "the song" "A - One" (songName requested.song)
      assertBool "not again" (not requested.refetch)
      assertEqual "nothing while reading" [] (filter (not . T.null) (mainLines r.state))
      loaded <- runEvents 0 [LyricsLoaded token (stored "First line\nSecond line")] r.state
      case screenLines loaded.state of
        title : _ : first : second : _ -> do
          assertBool ("the title: " <> T.unpack title) ("Lyrics: A - One" `T.isPrefixOf` title)
          assertEqual "the lyrics" ["First line", "Second line"] [first, second]
        ls -> assertFailure $ "too few lines: " <> show ls
      assertEqual "no message" Nothing loaded.state.message
    other -> assertFailure $ "one request of lyrics, not " <> show other

test_outcomes :: Assertion
test_outcomes = do
  r <- runEvents 0 [key "l"] =<< queueShown
  token <- requestToken r
  fetching <- runEvents 0 [LyricsFetching token "LRCLIB"] r.state
  assertEqual
    "fetching"
    ["Fetching the lyrics from LRCLIB…"]
    (take 1 (mainLines fetching.state))
  next <- runEvents 0 [LyricsFetching token "tekstowo.pl"] fetching.state
  assertEqual
    "the next fetcher"
    ["Fetching the lyrics from tekstowo.pl…"]
    (take 1 (mainLines next.state))
  fetched <-
    runEvents
      0
      [LyricsLoaded token (LyricsFound (Fetched "LRCLIB") (plainLyrics "Words"))]
      fetching.state
  assertEqual "fetched" ["Words"] (take 1 (mainLines fetched.state))
  assertEqual
    "where from"
    (Just "Fetched the lyrics from LRCLIB")
    ((.text) <$> fetched.state.message)
  let shown result =
        filter (not . T.null) . mainLines . (.state)
          <$> runEvents 0 [LyricsLoaded token result] r.state
  assertEqual "instrumental" ["Instrumental"] =<< shown LyricsInstrumental
  assertEqual "not stored" ["No lyrics stored"] =<< shown (LyricsMissing [])
  assertEqual
    "not found"
    ["No lyrics found on A, B or C"]
    =<< shown (LyricsMissing [("A", Nothing), ("B", Nothing), ("C", Nothing)])
  assertEqual
    "a failure"
    ["No lyrics found on B", "A is busy"]
    =<< shown (LyricsMissing [("A", Just "A is busy"), ("B", Nothing)])
  assertEqual "only a failure" ["A is busy"]
    =<< shown (LyricsMissing [("A", Just "A is busy")])
  assertEqual
    "unreadable"
    ["The lyrics can't be read"]
    =<< shown (LyricsFailed "The lyrics can't be read")

test_back :: Assertion
test_back = do
  r <- runEvents 0 [key "l", key "l"] =<< queueShown
  assertEqual "the queue" QueueScreen (focusedView r.state).screen
  assertEqual "after the lyrics" (Just LyricsScreen) (focusedView r.state).previous
  browser <- runEvents 0 [key "2"] =<< queueShown
  listed <- case browser.pending of
    [p] -> runEvents 0 [replyTo ["file: x.flac", "Title: X"] p] browser.state
    ps -> assertFailure $ "one listing, not " <> show (length ps)
  back <- runEvents 0 [key "l", key "l"] listed.state
  assertEqual "the browser" BrowserScreen (focusedView back.state).screen

-- | The queue is the first screen, so no screen was before it.
test_nowhereBack :: Assertion
test_nowhereBack = do
  r <-
    runEvents
      0
      (Resized 40 10 : map key [":", "b", "a", "c", "k", "enter"])
      (initialState defaultConfig)
  assertEqual "the queue" QueueScreen (focusedView r.state).screen
  assertEqual
    "the message"
    (Just "There is no screen to go back to")
    ((.text) <$> r.state.message)

test_noSong :: Assertion
test_noSong = do
  visualizer <- runEvents 0 [key "8", key "l"] =<< queueShown
  assertEqual
    "the visualizer"
    (Just "The visualizer screen has no songs")
    (messageOf visualizer)
  empty <- runEvents 0 [key "l"] =<< testState (40, 10) (statusOf Stopped Nothing 0) []
  assertEqual "an empty queue" (Just "There is no song under the cursor") (messageOf empty)
  browser <- runEvents 0 [key "2"] =<< queueShown
  listed <- case browser.pending of
    [p] -> runEvents 0 [replyTo ["directory: a"] p] browser.state
    ps -> assertFailure $ "one listing, not " <> show (length ps)
  onDirectory <- runEvents 0 [key "down", key "l"] listed.state
  assertEqual
    "a directory"
    (Just "There is no song under the cursor")
    (messageOf onDirectory)
  assertEqual "no request" [] [() | FetchLyrics _ _ <- onDirectory.commands]

test_stale :: Assertion
test_stale = do
  r <- runEvents 0 [key "l", key "1", key "down", key "l"] =<< queueShown
  case [t | FetchLyrics t _ <- r.commands] of
    [first, second] -> do
      stale <-
        runEvents 0 [LyricsFetching first "LRCLIB", LyricsLoaded first (stored "old")] r.state
      assertEqual "the screen stays" [KeepScreen, KeepScreen] stale.commands
      assertEqual "nothing yet" ReadingLyrics stale.state.lyrics.status
      fresh <- runEvents 0 [LyricsLoaded second (stored "new")] stale.state
      assertEqual "the new song" ["new"] (take 1 (mainLines fresh.state))
    other -> assertFailure $ "two requests of lyrics, not " <> show other

-- | 20 lines in a main area of 6 rows.
test_scroll :: Assertion
test_scroll = do
  r <- runEvents 0 [key "l"] =<< queueShown
  token <- requestToken r
  let lyrics = T.unlines ["line " <> T.pack (show i) | i <- [1 .. 20 :: Int]]
  scrolled <- runEvents 0 [LyricsLoaded token (stored lyrics), key "down"] r.state
  assertEqual "a line down" ["line 2"] (take 1 (mainLines scrolled.state))
  end <- runEvents 0 [key "end"] scrolled.state
  assertEqual "the end" ["line 15", "line 20"] (firstAndLast (mainLines end.state))
  further <- runEvents 0 [key "down"] end.state
  assertEqual "no further" ["line 15", "line 20"] (firstAndLast (mainLines further.state))
  home <- runEvents 0 [key "home"] further.state
  assertEqual "the start" ["line 1"] (take 1 (mainLines home.state))

test_refetch :: Assertion
test_refetch = do
  r <- runEvents 0 [key "l"] =<< queueShown
  token <- requestToken r
  loaded <- runEvents 0 [LyricsLoaded token (stored "old")] r.state
  again <- runEvents 0 [key "`"] loaded.state
  case [q | FetchLyrics _ q <- again.commands] of
    [requested] -> do
      assertEqual "the song" "A - One" (songName requested.song)
      assertBool "again" requested.refetch
    other -> assertFailure $ "one request, not " <> show other
  assertEqual "until they come" [] (filter (not . T.null) (mainLines again.state))
  back <- runEvents 0 [key "l"] again.state
  assertEqual "still back to the queue" QueueScreen (focusedView back.state).screen
  onQueue <- runEvents 0 [key "`"] =<< queueShown
  assertEqual "not on the queue" [] [() | FetchLyrics _ _ <- onQueue.commands]
  let withoutFetchers = testAppEnv & #config % #lyrics % #fetchers .~ []
  nowhere <- runEventsWith withoutFetchers 0 [key "`"] loaded.state
  assertEqual "no request without fetchers" [] [() | FetchLyrics _ _ <- nowhere.commands]
  againToken <- requestToken again
  kept <-
    runEvents
      0
      [ LyricsLoaded
          againToken
          ( LyricsFound
              (Kept "A - One.txt" [("LRCLIB", Nothing), ("B", Just "B is down")])
              (plainLyrics "old")
          )
      ]
      again.state
  assertEqual "the stored lyrics" ["old"] (filter (not . T.null) (mainLines kept.state))
  assertEqual
    "why they stay"
    (Just "No lyrics found on LRCLIB. B is down. The stored lyrics stay")
    (messageOf kept)

test_worker :: Assertion
test_worker = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Hello\r\nWorld\n\n"
  createDirectory (dir </> "A - Three.txt")
  withWorker dir [] $ \ask -> do
    assertEqual "stored" [LyricsLoaded 1 (stored "Hello\nWorld")] =<< ask 1 "One" False
    assertEqual "missing" [LyricsLoaded 2 (LyricsMissing [])] =<< ask 2 "Two" False
    ask 3 "Three" False >>= \case
      [LyricsLoaded 3 (LyricsFailed _)] -> pure ()
      other -> assertFailure $ "a failure, not " <> show other

test_workerFetches :: Assertion
test_workerFetches = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Stored"
  (calls, fetcher) <- counted "LRCLIB" (pure (FetchedLyrics (plainLyrics "Fetched")))
  withWorker (dir </> "new") [fetcher] $ \ask -> do
    let fetched = LyricsFound (Fetched "LRCLIB") (plainLyrics "Fetched")
    assertEqual "fetched" [LyricsFetching 1 "LRCLIB", LyricsLoaded 1 fetched]
      =<< ask 1 "Two" False
    assertEqual "stored" "Fetched\n" =<< BS.readFile (dir </> "new" </> "A - Two.txt")
    assertEqual
      "read the second time"
      [LyricsLoaded 2 (LyricsFound (Stored "A - Two.txt") (plainLyrics "Fetched"))]
      =<< ask 2 "Two" False
  assertEqual "one fetch" 1 =<< S.readIORef calls

test_workerAsksAgain :: Assertion
test_workerAsksAgain = withSystemTempDirectory "lyrics" $ \dir -> do
  (calls, missing) <- counted "A" (pure FetchedNothing)
  withWorker dir [missing] $ \ask -> do
    let notFound n = [LyricsFetching n "A", LyricsLoaded n (LyricsMissing [("A", Nothing)])]
    assertEqual "missing" (notFound 1) =<< ask 1 "One" False
    assertEqual "asked again" (notFound 2) =<< ask 2 "One" False
  assertEqual "two fetches" 2 =<< S.readIORef calls

test_workerFetchers :: Assertion
test_workerFetchers = withSystemTempDirectory "lyrics" $ \dir -> do
  (_, down) <- counted "A" (pure (FetchFailed "A is down"))
  (_, words') <- counted "B" (pure (FetchedLyrics (plainLyrics "Words")))
  (_, nothing) <- counted "C" (pure FetchedNothing)
  (_, instrumental) <- counted "D" (pure FetchedInstrumental)
  withWorker dir [down, words'] $ \ask ->
    assertEqual
      "the next fetcher"
      [ LyricsFetching 1 "A"
      , LyricsFetching 1 "B"
      , LyricsLoaded 1 (LyricsFound (Fetched "B") (plainLyrics "Words"))
      ]
      =<< ask 1 "Two" False
  withWorker dir [down, nothing] $ \ask ->
    assertEqual
      "what each did"
      [ LyricsFetching 1 "A"
      , LyricsFetching 1 "C"
      , LyricsLoaded 1 (LyricsMissing [("A", Just "A is down"), ("C", Nothing)])
      ]
      =<< ask 1 "Three" False
  withWorker dir [instrumental, words'] $ \ask ->
    assertEqual
      "an instrumental"
      [LyricsFetching 1 "D", LyricsLoaded 1 LyricsInstrumental]
      =<< ask 1 "Four" False

test_workerRefetches :: Assertion
test_workerRefetches = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Old"
  (calls, fetcher) <- counted "LRCLIB" (pure (FetchedLyrics (plainLyrics "New")))
  withWorker dir [fetcher] $ \ask -> do
    let fetched = LyricsFound (Fetched "LRCLIB") (plainLyrics "New")
    assertEqual "fetched" [LyricsFetching 1 "LRCLIB", LyricsLoaded 1 fetched]
      =<< ask 1 "One" True
    assertEqual "stored anew" "New\n" =<< BS.readFile (dir </> "A - One.txt")
  assertEqual "one fetch" 1 =<< S.readIORef calls

-- | A refetch that finds nothing leaves the stored lyrics on the screen and
-- on disk.
test_workerKeepsStored :: Assertion
test_workerKeepsStored = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Old"
  (_, down) <- counted "LRCLIB" (pure (FetchFailed "LRCLIB is down"))
  withWorker dir [down] $ \ask -> do
    assertEqual
      "kept"
      [ LyricsFetching 1 "LRCLIB"
      , LyricsLoaded
          1
          (LyricsFound (Kept "A - One.txt" [("LRCLIB", Just "LRCLIB is down")]) (plainLyrics "Old"))
      ]
      =<< ask 1 "One" True
    assertEqual
      "nothing stored"
      [ LyricsFetching 2 "LRCLIB"
      , LyricsLoaded 2 (LyricsMissing [("LRCLIB", Just "LRCLIB is down")])
      ]
      =<< ask 2 "Two" True
  assertEqual "the file" "Old" =<< BS.readFile (dir </> "A - One.txt")

-- | Lyrics pasted into a new file of synced ones, without times, are stored
-- lyrics, not missing ones that a fetch replaces.
test_workerUntimedLrc :: Assertion
test_workerUntimedLrc = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.lrc") "Pasted\nwords\n"
  (calls, fetcher) <- counted "LRCLIB" (pure (FetchedLyrics (plainLyrics "Fetched")))
  withWorker dir [fetcher] $ \ask ->
    assertEqual
      "the text"
      [LyricsLoaded 1 (LyricsFound (Stored "A - One.lrc") (plainLyrics "Pasted\nwords"))]
      =<< ask 1 "One" False
  assertEqual "no fetch" 0 =<< S.readIORef calls
  assertEqual "the file" "Pasted\nwords\n" =<< BS.readFile (dir </> "A - One.lrc")
  assertBool "no text file" . not =<< doesFileExist (dir </> "A - One.txt")

test_workerWithoutFetchers :: Assertion
test_workerWithoutFetchers = withSystemTempDirectory "lyrics" $ \dir ->
  withWorker dir [] $ \ask ->
    assertEqual "missing, not fetching" [LyricsLoaded 1 (LyricsMissing [])]
      =<< ask 1 "One" True

test_parseLrc :: Assertion
test_parseLrc = do
  assertEqual
    "lines"
    [(1, "First"), (2.5, "Second")]
    (parseLrc "[00:01.00] First\n[00:02.50]Second\n")
  assertEqual
    "a line at several times"
    [(10, "Chorus"), (20, "Verse"), (30, "Chorus")]
    (parseLrc "[00:10.00][00:30.00]Chorus\n[00:20.00]Verse")
  assertEqual "tags" [(1, "Line")] (parseLrc "[ar:Artist]\n[ti:Title]\n[00:01]Line")
  assertEqual
    "times"
    [(62, "a"), (62.345, "b"), (62.5, "c")]
    (parseLrc "[1:02]a\n[01:02.345]b\n[01:02:50]c")
  assertEqual "a pause" [(5, "")] (parseLrc "[00:05.00]")
  assertEqual "Windows' line ends" [(1, "A")] (parseLrc "[00:01.00]A\r\n")
  assertEqual "spaces before a time" [(1, "A")] (parseLrc "  [00:01.00]A")
  -- A positive offset, in milliseconds, shows the lines sooner.
  assertEqual
    "an offset"
    [(0.5, "A"), (1.5, "B")]
    (parseLrc "[offset:+500]\n[00:01.00]A\n[00:02.00]B")
  assertEqual "a negative offset" [(1.5, "A")] (parseLrc "[offset: -500]\r\n[00:01.00]A")
  assertEqual "not before the start" [(0, "A")] (parseLrc "[offset:2000]\n[00:01.00]A")
  assertEqual "not timed" Nothing (timedLyrics "Just words\nand more")
  assertEqual
    "the text of timed lyrics"
    (Just "First\nSecond\n")
    ((.plain) <$> timedLyrics "[00:01.00] First\n[00:02.50]Second")

test_workerTimes :: Assertion
test_workerTimes = withSystemTempDirectory "lyrics" $ \dir -> do
  let lrc = "[00:01.00]Sung"
      timed = Lyrics "Plain words" ((.timed) =<< timedLyrics lrc)
  (_, fetcher) <- counted "LRCLIB" (pure (FetchedLyrics timed))
  withWorker dir [fetcher] $ \ask -> do
    _ <- ask 1 "One" False
    assertEqual "the text" "Plain words\n" =<< BS.readFile (dir </> "A - One.txt")
    assertEqual "the times" "[00:01.00]Sung\n" =<< BS.readFile (dir </> "A - One.lrc")
    ask 2 "One" False >>= \case
      [LyricsLoaded 2 (LyricsFound (Stored "A - One.lrc") lyrics)] ->
        assertEqual "timed" (Just [(1, "Sung")]) ((.entries) <$> lyrics.timed)
      other -> assertFailure $ "stored lyrics, not " <> show other
  (_, untimed) <-
    counted "LRCLIB" (pure (FetchedLyrics (plainLyrics "Other words")))
  withWorker dir [untimed] $ \ask -> do
    _ <- ask 1 "One" True
    assertBool "no times" . not =<< doesFileExist (dir </> "A - One.lrc")
    assertEqual "the text" [LyricsLoaded 2 (stored "Other words")] =<< ask 2 "One" False

-- | 20 timed lines, a second apart, in a main area of 6 rows, while the
-- song is paused at 10 s.
test_sung :: Assertion
test_sung = do
  shown <- timedShown Paused
  assertEqual "the line" (Just 10) (sungLine shown)
  assertEqual "in the middle" ["line 7", "line 12"] (firstAndLast (mainLines shown))
  assertEqual "the style" ["line 10"] (boldTexts shown)
  assertEqual
    "the text's style"
    [V.SetTo (V.ISOColor 3)]
    ( L.nub
        [ V.attrForeColor a
        | (a, t) <- imageSpans (renderScreen testAppEnv shown)
        , "line" `T.isPrefixOf` t
        ]
    )
  other <- runEvents 0 [key "1", key "down", key "l"] shown
  token <- requestToken other
  loaded <-
    runEvents
      0
      [LyricsLoaded token (LyricsFound (Stored "A - One.lrc") timedTwenty)]
      other.state
  assertEqual "not of a song that doesn't play" Nothing (sungLine loaded.state)
  assertEqual "from the top" ["line 0"] (take 1 (mainLines loaded.state))
  where
    boldTexts :: AppState -> [T.Text]
    boldTexts s =
      [ t
      | (a, t) <- imageSpans (renderScreen testAppEnv s)
      , V.SetTo st <- [V.attrStyle a]
      , V.hasStyle st V.bold
      , "line" `T.isPrefixOf` t
      ]

test_stopFollowing :: Assertion
test_stopFollowing = do
  shown <- timedShown Paused
  scrolled <- runEvents 0 [key "down"] shown
  assertEqual "from where it showed" ["line 8"] (take 1 (mainLines scrolled.state))
  assertBool "not following" (not scrolled.state.lyrics.following)
  again <- runEvents 0 [key "l", key "l"] scrolled.state
  assertBool "following the next time" again.state.lyrics.following
  jumped <- runEvents 0 [key "o"] scrolled.state
  assertBool "following after o" jumped.state.lyrics.following
  assertEqual
    "the line being sung in the middle"
    ["line 7"]
    (take 1 (mainLines jumped.state))

-- | The 6 rows show line 15 in their middle, and its 15 in the style of
-- found items.
test_findStopsFollowing :: Assertion
test_findStopsFollowing = do
  shown <- timedShown Paused
  let find ks = runEvents 0 (map key ks) shown
  found <- find ["/", "1", "5", "enter"]
  assertBool "not following" (not found.state.lyrics.following)
  assertEqual "in the middle" ["line 12", "line 13", "line 14", "line 15"]
    . take 4
    $ mainLines found.state
  assertBool
    "the match"
    ( not $
        null
          [ t
          | (a, t) <- imageSpans (renderScreen testAppEnv found.state)
          , t == "15"
          , V.attrStyle a /= V.Default
          ]
    )
  cancelled <- find ["/", "1", "5", "escape"]
  assertEqual "a cancel goes back to where it showed" (take 1 (mainLines shown))
    . take 1
    $ mainLines cancelled.state

-- | On another song's lyrics, o asks for those of the song that plays.
test_jumpToPlayingLyrics :: Assertion
test_jumpToPlayingLyrics = do
  shown <- timedShown Paused
  other <- runEvents 0 [key "1", key "down", key "l"] shown
  token <- requestToken other
  loaded <- runEvents 0 [LyricsLoaded token (stored "Other words")] other.state
  jumped <- runEvents 0 [key "o"] loaded.state
  assertEqual
    "the song that plays"
    [Just ["One"]]
    [M.lookup Title r.song.tags | FetchLyrics _ r <- jumped.commands]

-- | Playing, 0.25 s after the status of 10 s: the next line at 11 s.
test_nextLine :: Assertion
test_nextLine = do
  shown <- timedShown Playing
  r <- runEvents 0.25 [Tick 0] shown
  assertEqual "at the next line" (Just 1) (nextLyricsLine r.state)

test_workerInBackground :: Assertion
test_workerInBackground = withSystemTempDirectory "lyrics" $ \dir -> do
  requested <- newTVarIO Nothing
  background <- newTVarIO Nothing
  events <- newTQueueIO
  (calls, fetcher) <- counted "LRCLIB" (pure (FetchedLyrics (plainLyrics "Ahead")))
  let source =
        LyricsSource
          { directory = dir
          , fetchers = [fetcher]
          , requested = requested
          , background = background
          , emit = atomically . writeTQueue events
          , logLine = \_ -> pure ()
          }
      one = song 0 [(Artist, ["A"]), (Title, ["One"])] 60
  bracket (forkIO (lyricsWorker source)) killThread $ \_ -> do
    atomically . writeTVar background $ Just one
    -- The file is empty for a moment after the worker creates it, and while
    -- the worker writes it, GHC's lock of the file in this process fails a
    -- read.
    ahead <- timeout (5 * 1000000) . untilJust $ do
      text <-
        either (const BS.empty) id <$> try @IOException (BS.readFile (dir </> "A - One.txt"))
      if BS.null text
        then Nothing <$ threadDelay 1000
        else pure (Just text)
    assertEqual "stored" (Just "Ahead\n") ahead
    atomically . writeTVar requested $ Just (1, LyricsRequest one False)
    loaded <- timeout (5 * 1000000) . atomically $ readTQueue events
    assertEqual "read, not fetched" (Just (LyricsLoaded 1 (stored "Ahead"))) loaded
  assertEqual "one fetch" 1 =<< S.readIORef calls
  where
    untilJust :: IO (Maybe b) -> IO b
    untilJust act = act >>= maybe (untilJust act) pure

-- | A request of the screen doesn't wait for a fetch in the background, nor
-- for an older request, whose songs it left. The fetch in the background
-- starts again after it.
test_workerTakesTurns :: Assertion
test_workerTakesTurns = withSlowOne $ \src started release next -> do
  atomically . writeTVar src.background $ Just (titled "One")
  takeMVar started
  atomically . writeTVar src.requested $ Just (1, LyricsRequest (titled "Two") False)
  assertEqual "Two" [LyricsFetching 1 "LRCLIB", LyricsLoaded 1 (fromLrclib "Two")]
    =<< sequence [next, next]
  takeMVar started
  atomically . writeTVar src.requested $ Just (2, LyricsRequest (titled "One") True)
  takeMVar started
  atomically . writeTVar src.requested $ Just (3, LyricsRequest (titled "Three") False)
  assertEqual
    "Three after a newer request"
    [ LyricsFetching 2 "LRCLIB"
    , LyricsFetching 3 "LRCLIB"
    , LyricsLoaded 3 (fromLrclib "Three")
    ]
    =<< sequence [next, next, next]
  putMVar release ()
  assertEqual "One in the background" "One\n"
    =<< expectWithin (untilStored (src.directory </> "A - One.txt"))

-- | A request of the screen for the song that a fetch in the background
-- fetches takes over the fetch.
test_workerTakesOver :: Assertion
test_workerTakesOver = withSlowOne $ \src started release next -> do
  atomically . writeTVar src.background $ Just (titled "One")
  takeMVar started
  atomically . writeTVar src.requested $ Just (1, LyricsRequest (titled "One") False)
  assertEqual "the fetcher that it asks" (LyricsFetching 1 "LRCLIB") =<< next
  putMVar release ()
  assertEqual "the lyrics" (LyricsLoaded 1 (fromLrclib "One")) =<< next
  assertBool "one fetch" . isNothing =<< tryTakeMVar started

-- | A worker whose fetcher has the lyrics of a song at once, but those of
-- One only after a release, with a signal for each start of their fetch and
-- the next event.
withSlowOne
  :: (LyricsSource -> MVar () -> MVar () -> IO AppEvent -> IO a) -> IO a
withSlowOne k = withSystemTempDirectory "lyrics" $ \dir -> do
  requested <- newTVarIO Nothing
  background <- newTVarIO Nothing
  events <- newTQueueIO
  started <- newEmptyMVar
  release <- newEmptyMVar
  let fetch s = do
        let title = fromMaybe "" (firstTag Title s)
        when (title == "One") $ putMVar started () >> readMVar release
        pure . FetchedLyrics $ plainLyrics title
      source =
        LyricsSource
          { directory = dir
          , fetchers = [Fetcher "LRCLIB" fetch]
          , requested = requested
          , background = background
          , emit = atomically . writeTQueue events
          , logLine = \_ -> pure ()
          }
  bracket (forkIO (lyricsWorker source)) killThread $ \_ ->
    k source started release (expectWithin (atomically (readTQueue events)))

-- | A song of its own file, as the worker tells songs apart by their files.
titled :: T.Text -> Song
titled t = song 0 [(Artist, ["A"]), (Title, [t])] 60 & #file .~ ("dir/" <> t <> ".flac")

fromLrclib :: T.Text -> LyricsResult
fromLrclib = LyricsFound (Fetched "LRCLIB") . plainLyrics

-- | The text of a file once it has some. The file is empty for a moment
-- after the worker creates it, and while the worker writes it, GHC's lock
-- of the file in this process fails a read.
untilStored :: FilePath -> IO BS.ByteString
untilStored file = do
  text <- either (const BS.empty) id <$> try @IOException (BS.readFile file)
  if BS.null text then threadDelay 1000 >> untilStored file else pure text

-- | While One plays, the lyrics of Two show, until Three plays.
test_followPlaying :: Assertion
test_followPlaying = do
  s <- testState (40, 10) (statusOf Playing (Just 0) 3) threeSongs
  let following = s & #toggles % #lyricsFollowPlaying .~ True
  assertBool
    "from the config"
    (initialState (defaultConfig & #lyrics % #followPlaying .~ True)).toggles.lyricsFollowPlaying
  two <- runEvents 0 [key "down", key "l"] following
  assertEqual "the song under the cursor" ["A - Two"] (askedFor two)
  three <- runEvents 0 [StatusFetched (statusOf Playing (Just 2) 3)] two.state
  assertEqual "the next song" ["A - Three"] (askedFor three)
  assertEqual
    "the same screen to go back to"
    (Just QueueScreen)
    (focusedView three.state).previous
  notFollowing <-
    runEvents 0 [key "down", key "l", StatusFetched (statusOf Playing (Just 2) 3)] s
  assertEqual "not unless it follows" ["A - Two"] (askedFor notFollowing)
  elsewhere <-
    runEvents 0 [key "l", key "l", StatusFetched (statusOf Playing (Just 2) 3)] following
  assertEqual "not on another screen" ["A - One"] (askedFor elsewhere)

-- | The next song's lyrics show from their top, wherever the last song's
-- were scrolled to.
test_followFromTop :: Assertion
test_followFromTop = do
  s <- testState (40, 10) (statusOf Playing (Just 0) 3) threeSongs
  two <- runEvents 0 [key "down", key "l"] (s & #toggles % #lyricsFollowPlaying .~ True)
  twoToken <- requestToken two
  scrolled <-
    runEvents 0 [LyricsLoaded twoToken (stored (numbered 20)), key "end"] two.state
  three <- runEvents 0 [StatusFetched (statusOf Playing (Just 2) 3)] scrolled.state
  threeToken <- requestToken three
  shown <- runEvents 0 [LyricsLoaded threeToken (stored (numbered 3))] three.state
  assertEqual "from the top" ["line 1"] (take 1 (mainLines shown.state))
  where
    numbered :: Int -> T.Text
    numbered n = T.unlines ["line " <> T.pack (show i) | i <- [1 .. n]]

-- | A tab doesn't move the rest of the line out of the screen.
test_controlCharacters :: Assertion
test_controlCharacters = do
  r <- runEvents 0 [key "l"] =<< queueShown
  token <- requestToken r
  shown <- runEvents 0 [LyricsLoaded token (stored "a\tb\nc")] r.state
  assertEqual "the lines" ["a b", "c"] (take 2 (mainLines shown.state))

test_followTitle :: Assertion
test_followTitle = do
  s <- testState (40, 10) (statusOf Playing (Just 0) 3) threeSongs
  two <- runEvents 0 [key "down", key "l"] (s & #toggles % #lyricsFollowPlaying .~ True)
  three <- runEvents 10 [StatusFetched (statusOf Playing (Just 2) 3)] two.state
  assertEqual "since the next song" (Just 10) (snd <$> three.state.titleShown)

test_toggleFollowing :: Assertion
test_toggleFollowing = do
  s <- testState (40, 10) (statusOf Playing (Just 0) 3) threeSongs
  on <- runEvents 0 [key "down", key "l", key "space"] s
  assertEqual "on" (Just "Lyrics follow playing: on") ((.text) <$> on.state.message)
  assertEqual "the song that plays at once" ["A - Two", "A - One"] (askedFor on)
  assertBool "not the queue's" (not on.state.toggles.followPlaying)
  off <- runEvents 0 [key "space"] on.state
  assertEqual "off" (Just "Lyrics follow playing: off") ((.text) <$> off.state.message)

test_edit :: Assertion
test_edit = do
  r <- runEvents 0 [key "l"] =<< queueShown
  token <- requestToken r
  loading <- runEvents 0 [key "e", key "e"] r.state
  assertEqual "not while loading" [] (edits loading)
  assertEqual
    "says why"
    (Just "The lyrics are still loading")
    ((.text) <$> loading.state.message)
  missing <- runEvents 0 [LyricsLoaded token (LyricsMissing [])] r.state
  asked <- runEvents 0 [key "e", key "e"] missing.state
  assertEqual "nothing yet" [] (edits asked)
  assertEqual
    "the choice"
    (Just "Edit which lyrics?")
    ((.question) <$> asked.state.prompt)
  assertEqual "unsynced" [Edit "edit" ("lyrics" </> "A - One.txt")] . edits
    =<< runEvents 0 [key "u"] asked.state
  assertEqual "synced" [Edit "edit" ("lyrics" </> "A - One.lrc")] . edits
    =<< runEvents 0 [key "s"] asked.state
  timed <-
    runEvents
      0
      [LyricsLoaded token (LyricsFound (Stored "A - One.lrc") timedTwenty), key "e", key "e"]
      r.state
  assertEqual "the times" [Edit "edit" ("lyrics" </> "A - One.lrc")] (edits timed)
  untimedLrc <-
    runEvents
      0
      [ LyricsLoaded token (LyricsFound (Stored "A - One.lrc") (plainLyrics "Words"))
      , key "e"
      , key "e"
      ]
      r.state
  assertEqual
    "the file they came from"
    [Edit "edit" ("lyrics" </> "A - One.lrc")]
    (edits untimedLrc)
  noEditor <-
    runEventsWith (testAppEnv & #editor .~ Nothing) 0 [key "e", key "e"] missing.state
  assertEqual "no editor" [] (edits noEditor)
  assertBool
    "says how to set one"
    (maybe False ("editor.command" `T.isInfixOf`) ((.text) <$> noEditor.state.message))
  where
    edits :: Result -> [UiCommand]
    edits r = [c | c@(Edit _ _) <- r.commands]

test_edited :: Assertion
test_edited = do
  r <- runEvents 0 [key "l"] =<< queueShown
  token <- requestToken r
  shown <- runEvents 0 [LyricsLoaded token (LyricsMissing [])] r.state
  edited <- runEvents 0 [Edited ("lyrics" </> "A - One.txt") Nothing] shown.state
  assertEqual "again" ["A - One"] (askedFor edited)
  other <- runEvents 0 [Edited ("lyrics" </> "A - Two.txt") Nothing] shown.state
  assertEqual "not of another file" [] (askedFor other)
  failed <-
    runEvents
      0
      [Edited ("lyrics" </> "A - One.txt") (Just "The editor exited with 1")]
      shown.state
  assertEqual
    "the failure"
    (Just ("The editor exited with 1", True))
    ((\m -> (m.text, m.isError)) <$> failed.state.message)

test_runEditor :: Assertion
test_runEditor = withSystemTempDirectory "editor" $ \dir -> do
  let file = dir </> "new" </> "$(touch pwned) `touch pwned` - A.txt"
  assertEqual "written" Nothing =<< runEditor "printf words >" file
  assertEqual "the file" "words" =<< BS.readFile file
  assertBool "no shell code of the name" . not =<< doesFileExist "pwned"
  assertEqual "a failure" (Just "The editor exited with 3") =<< runEditor "exit 3;" file

threeSongs :: [Song]
threeSongs =
  [ song 0 [(Artist, ["A"]), (Title, ["One"])] 60
  , song 1 [(Artist, ["A"]), (Title, ["Two"])] 60
  , song 2 [(Artist, ["A"]), (Title, ["Three"])] 60
  ]

-- | The songs whose lyrics the screen asked for.
askedFor :: Result -> [T.Text]
askedFor r = [songName q.song | FetchLyrics _ q <- r.commands]

-- | A stream has no lyrics: the songs that it plays would share one file.
test_stream :: Assertion
test_stream = do
  let stream = song 1 [(Title, ["A - Two"])] 60 & #file .~ "http://example.com/radio.mp3"
      songs = [song 0 [(Artist, ["A"]), (Title, ["One"])] 60, stream]
  s <- testState (40, 10) (statusOf Playing (Just 0) 2) songs
  shown <- runEvents 0 [key "down", key "l"] s
  assertEqual "not asked for" [] (askedFor shown)
  assertEqual "the error" (Just True) ((.isError) <$> shown.state.message)
  assertEqual "the queue stays" QueueScreen (focusedView shown.state).screen
  onFirst <- runEvents 0 [key "l"] (s & #toggles % #lyricsFollowPlaying .~ True)
  followed <- runEvents 0 [StatusFetched (statusOf Playing (Just 1) 2)] onFirst.state
  assertEqual "not followed" [] (askedFor followed)
  assertEqual "the screen stays" (Just "A - One") (songName <$> followed.state.lyrics.song)
  let env = testAppEnv & #config % #lyrics % #fetchInBackground .~ True
  streaming <- testState (40, 10) (statusOf Playing (Just 1) 2) songs
  background <- runEventsWith env 0 [Tick 0] streaming
  assertEqual
    "not in the background"
    []
    [() | FetchLyricsInBackground _ <- background.commands]

test_fetchInBackground :: Assertion
test_fetchInBackground = do
  let env = testAppEnv & #config % #lyrics % #fetchInBackground .~ True
      songs =
        [ song 0 [(Artist, ["A"]), (Title, ["One"])] 60
        , song 1 [(Artist, ["A"]), (Title, ["Two"])] 60
        ]
      fetched r = [songName s | FetchLyricsInBackground s <- r.commands]
  s <- testState (40, 10) (statusOf Playing (Just 0) 2) songs
  first <- runEventsWith env 0 [Tick 0, Tick 0] s
  assertEqual "once" ["A - One"] (fetched first)
  next <- runEventsWith env 0 [StatusFetched (statusOf Playing (Just 1) 2)] first.state
  assertEqual "the next song" ["A - Two"] (fetched next)
  off <- runEvents 0 [Tick 0] s
  assertEqual "not unless the config says so" [] (fetched off)

-- | The lyrics screen of the first song, which plays, with 'timedTwenty'.
timedShown :: PlayerState -> IO AppState
timedShown st = do
  s <-
    testState
      (40, 10)
      (statusOf st (Just 0) 2)
      [ song 0 [(Artist, ["A"]), (Title, ["One"])] 60
      , song 1 [(Artist, ["A"]), (Title, ["Two"])] 60
      ]
  r <- runEvents 0 [key "l"] s
  token <- requestToken r
  (.state)
    <$> runEvents 0 [LyricsLoaded token (LyricsFound (Stored "A - One.lrc") timedTwenty)] r.state

-- | 20 lines, @line 0@ to @line 19@, a second apart.
timedTwenty :: Lyrics
timedTwenty =
  fromMaybe (error "not timed") . timedLyrics $
    T.unlines
      [ "[00:" <> T.justifyRight 2 '0' (T.pack (show i)) <> ".00]line " <> T.pack (show i)
      | i <- [0 .. 19 :: Int]
      ]

firstAndLast :: [T.Text] -> [T.Text]
firstAndLast ls = take 1 ls <> take 1 (reverse ls)

-- | Run a worker with a directory and fetchers, and ask it for the lyrics
-- of A's songs: the events of a request, until its lyrics.
withWorker
  :: FilePath
  -> [Fetcher]
  -> ((Int -> T.Text -> Bool -> IO [AppEvent]) -> IO a)
  -> IO a
withWorker dir fetchers k = do
  requested <- newTVarIO Nothing
  background <- newTVarIO Nothing
  events <- newTQueueIO
  let source =
        LyricsSource
          { directory = dir
          , fetchers = fetchers
          , requested = requested
          , background = background
          , emit = atomically . writeTQueue events
          , logLine = \_ -> pure ()
          }
      ask token title refetch = do
        let s = song 0 [(Artist, ["A"]), (Title, [title])] 60
        atomically . writeTVar requested $ Just (token, LyricsRequest s refetch)
        untilLoaded events
  bracket (forkIO (lyricsWorker source)) killThread $ \_ -> k ask
  where
    untilLoaded :: TQueue AppEvent -> IO [AppEvent]
    untilLoaded events = do
      e <- expectWithin (atomically (readTQueue events))
      case e of
        LyricsLoaded _ _ -> pure [e]
        _ -> (e :) <$> untilLoaded events

-- | What the worker does at once, failing instead of hanging if it never
-- comes.
expectWithin :: IO b -> IO b
expectWithin act = timeout (5 * 1000000) act >>= maybe (assertFailure "nothing came") pure

-- | A fetcher with a name that counts its calls.
counted :: T.Text -> IO FetchResult -> IO (S.IORef Int, Fetcher)
counted name result = do
  calls <- S.newIORef 0
  pure (calls, Fetcher name (\_ -> S.modifyIORef calls (+ 1) >> result))

-- | The token of the one request of lyrics.
requestToken :: Result -> IO Int
requestToken r = case [t | FetchLyrics t _ <- r.commands] of
  [t] -> pure t
  other -> assertFailure $ "one request of lyrics, not " <> show (length other)

-- | Plain lyrics stored for the song A - One.
stored :: T.Text -> LyricsResult
stored = LyricsFound (Stored "A - One.txt") . plainLyrics

messageOf :: Result -> Maybe T.Text
messageOf r = (.text) <$> r.state.message

-- | The queue of two songs on a terminal of 40 by 10.
queueShown :: IO AppState
queueShown =
  testState
    (40, 10)
    (statusOf Stopped Nothing 2)
    [ song 0 [(Artist, ["A"]), (Title, ["One"])] 60
    , song 1 [(Artist, ["A"]), (Title, ["Two"])] 60
    ]

screenLines :: AppState -> [T.Text]
screenLines = imageLines . renderScreen testAppEnv

-- | The lines of the main area, after the header's two.
mainLines :: AppState -> [T.Text]
mainLines s = take (mainHeight s.terminalSize) . drop 2 $ screenLines s

key :: T.Text -> AppEvent
key = KeyPressed . either (error . T.unpack) id . parseKeySpec

-- | From the browser, which the queue showed, by the action, which no key
-- of the browser is bound to.
test_backAnywhere :: Assertion
test_backAnywhere = do
  r <- runEvents 0 (map key ["2", ":", "b", "a", "c", "k", "enter"]) =<< queueShown
  assertEqual "the queue" QueueScreen (focusedView r.state).screen
  assertEqual "after the browser" (Just BrowserScreen) (focusedView r.state).previous
