module LyricsTests (lyricsTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString qualified as BS
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Optics.Core
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Keys
import Reprise.Lyrics
import Reprise.Lyrics.Worker
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.UI.Layout
import Utils

lyricsTests :: TestTree
lyricsTests =
  testGroup
    "Lyrics"
    [ testCase "the file names are ncmpcpp's" test_fileNames
    , testCase "l shows the lyrics of the song under the cursor" test_show
    , testCase "fetched lyrics and the other outcomes" test_outcomes
    , testCase "l on the lyrics goes back" test_back
    , testCase "l without a song under the cursor" test_noSong
    , testCase "the lyrics of a song that the screen left are dropped" test_stale
    , testCase "the lyrics scroll" test_scroll
    , testCase "` fetches the lyrics again" test_refetch
    , testCase "the worker reads the stored lyrics" test_worker
    , testCase "the worker fetches and stores the lyrics" test_workerFetches
    , testCase "the worker remembers what isn't there, not failures" test_workerRemembers
    , testCase "the worker fetches the lyrics again" test_workerRefetches
    , testCase "the worker without fetchers" test_workerWithoutFetchers
    , testCase "timed lyrics from LRC" test_parseLrc
    , testCase "the worker stores and reads the times" test_workerTimes
    , testCase "the line being sung" test_sung
    , testCase "scrolling stops following the song" test_stopFollowing
    , testCase "a redraw when the next line is sung" test_nextLine
    , testCase "the worker fetches in the background" test_workerInBackground
    , testCase "the lyrics of each new song that plays are fetched" test_fetchInBackground
    , testCase "the lyrics follow the song that plays" test_followPlaying
    , testCase "space turns following the song on and off" test_toggleFollowing
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

test_show :: Assertion
test_show = do
  r <- runEvents 0 [key "l"] =<< queueShown
  assertEqual "the screen" LyricsScreen (focusedView r.state).screen
  case [(t, q) | FetchLyrics t q <- r.commands] of
    [(token, requested)] -> do
      assertEqual "the song" "A - One" (lyricsName requested.song)
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
  fetching <- runEvents 0 [LyricsFetching token] r.state
  assertEqual "fetching" ["Fetching the lyrics…"] (take 1 (mainLines fetching.state))
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
  let shown result = take 1 . mainLines . (.state) <$> runEvents 0 [LyricsLoaded token result] r.state
  assertEqual "instrumental" ["Instrumental"] =<< shown LyricsInstrumental
  assertEqual "missing" ["No lyrics found"] =<< shown LyricsMissing
  assertEqual "failed" ["LRCLIB: busy"] =<< shown (LyricsFailed "LRCLIB: busy")

test_back :: Assertion
test_back = do
  r <- runEvents 0 [key "l", key "l"] =<< queueShown
  assertEqual "the queue" QueueScreen (focusedView r.state).screen

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
      stale <- runEvents 0 [LyricsFetching first, LyricsLoaded first (stored "old")] r.state
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
      assertEqual "the song" "A - One" (lyricsName requested.song)
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

test_worker :: Assertion
test_worker = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Hello\r\nWorld\n\n"
  createDirectory (dir </> "A - Three.txt")
  withWorker dir [] $ \ask -> do
    assertEqual "stored" [LyricsLoaded 1 (stored "Hello\nWorld")] =<< ask 1 "One" False
    assertEqual "missing" [LyricsLoaded 2 LyricsMissing] =<< ask 2 "Two" False
    ask 3 "Three" False >>= \case
      [LyricsLoaded 3 (LyricsFailed _)] -> pure ()
      other -> assertFailure $ "a failure, not " <> show other

test_workerFetches :: Assertion
test_workerFetches = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Stored"
  (calls, fetcher) <-
    counted (pure (LyricsFound (Fetched "LRCLIB") (plainLyrics "Fetched")))
  withWorker (dir </> "new") [fetcher] $ \ask -> do
    let fetched = LyricsFound (Fetched "LRCLIB") (plainLyrics "Fetched")
    assertEqual "fetched" [LyricsFetching 1, LyricsLoaded 1 fetched] =<< ask 1 "Two" False
    assertEqual "stored" "Fetched\n" =<< BS.readFile (dir </> "new" </> "A - Two.txt")
    assertEqual "read the second time" [LyricsLoaded 2 (stored "Fetched")]
      =<< ask 2 "Two" False
  assertEqual "one fetch" 1 =<< readIORef calls

test_workerRemembers :: Assertion
test_workerRemembers = withSystemTempDirectory "lyrics" $ \dir -> do
  (missingCalls, missing) <- counted (pure LyricsMissing)
  withWorker dir [missing] $ \ask -> do
    assertEqual "missing" [LyricsFetching 1, LyricsLoaded 1 LyricsMissing]
      =<< ask 1 "One" False
    assertEqual "remembered" [LyricsLoaded 2 LyricsMissing] =<< ask 2 "One" False
  assertEqual "one fetch of the missing" 1 =<< readIORef missingCalls
  (brokenCalls, broken) <- counted (pure (LyricsFailed "busy"))
  withWorker dir [broken] $ \ask -> do
    let failed n = [LyricsFetching n, LyricsLoaded n (LyricsFailed "busy")]
    assertEqual "failed" (failed 1) =<< ask 1 "One" False
    assertEqual "failed again" (failed 2) =<< ask 2 "One" False
  assertEqual "two fetches of the broken" 2 =<< readIORef brokenCalls
  (_, first) <- counted (pure (LyricsFailed "down"))
  (_, second) <- counted (pure (LyricsFound (Fetched "B") (plainLyrics "Words")))
  withWorker dir [first, second] $ \ask ->
    assertEqual
      "the next fetcher"
      [LyricsFetching 1, LyricsLoaded 1 (LyricsFound (Fetched "B") (plainLyrics "Words"))]
      =<< ask 1 "Two" False

test_workerRefetches :: Assertion
test_workerRefetches = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Old"
  (calls, fetcher) <- counted (pure (LyricsFound (Fetched "LRCLIB") (plainLyrics "New")))
  withWorker dir [fetcher] $ \ask -> do
    let fetched = LyricsFound (Fetched "LRCLIB") (plainLyrics "New")
    assertEqual "fetched" [LyricsFetching 1, LyricsLoaded 1 fetched] =<< ask 1 "One" True
    assertEqual "stored anew" "New\n" =<< BS.readFile (dir </> "A - One.txt")
  assertEqual "one fetch" 1 =<< readIORef calls

test_workerWithoutFetchers :: Assertion
test_workerWithoutFetchers = withSystemTempDirectory "lyrics" $ \dir ->
  withWorker dir [] $ \ask ->
    assertEqual "missing, not fetching" [LyricsLoaded 1 LyricsMissing] =<< ask 1 "One" True

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
  assertEqual "not timed" Nothing (timedLyrics "Just words\nand more")
  assertEqual
    "the text of timed lyrics"
    (Just "First\nSecond\n")
    ((.plain) <$> timedLyrics "[00:01.00] First\n[00:02.50]Second")

test_workerTimes :: Assertion
test_workerTimes = withSystemTempDirectory "lyrics" $ \dir -> do
  let lrc = "[00:01.00]Sung"
      timed = Lyrics "Plain words" ((.timed) =<< timedLyrics lrc)
  (_, fetcher) <- counted (pure (LyricsFound (Fetched "LRCLIB") timed))
  withWorker dir [fetcher] $ \ask -> do
    _ <- ask 1 "One" False
    assertEqual "the text" "Plain words\n" =<< BS.readFile (dir </> "A - One.txt")
    assertEqual "the times" "[00:01.00]Sung\n" =<< BS.readFile (dir </> "A - One.lrc")
    ask 2 "One" False >>= \case
      [LyricsLoaded 2 (LyricsFound Stored lyrics)] ->
        assertEqual "timed" (Just [(1, "Sung")]) ((.entries) <$> lyrics.timed)
      other -> assertFailure $ "stored lyrics, not " <> show other
  (_, untimed) <-
    counted (pure (LyricsFound (Fetched "LRCLIB") (plainLyrics "Other words")))
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
  other <- runEvents 0 [key "1", key "down", key "l"] shown
  token <- requestToken other
  loaded <- runEvents 0 [LyricsLoaded token (LyricsFound Stored timedTwenty)] other.state
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
  (calls, fetcher) <- counted (pure (LyricsFound (Fetched "LRCLIB") (plainLyrics "Ahead")))
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
    ahead <- timeout (5 * 1000000) . untilJust $ do
      exists <- doesFileExist (dir </> "A - One.txt")
      if exists
        then Just <$> BS.readFile (dir </> "A - One.txt")
        else Nothing <$ threadDelay 1000
    assertEqual "stored" (Just "Ahead\n") ahead
    atomically . writeTVar requested $ Just (1, LyricsRequest one False)
    loaded <- timeout (5 * 1000000) . atomically $ readTQueue events
    assertEqual "read, not fetched" (Just (LyricsLoaded 1 (stored "Ahead"))) loaded
  assertEqual "one fetch" 1 =<< readIORef calls
  where
    untilJust :: IO (Maybe b) -> IO b
    untilJust act = act >>= maybe (untilJust act) pure

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
  assertEqual "the same screen to go back to" QueueScreen three.state.lyrics.returnTo
  notFollowing <-
    runEvents 0 [key "down", key "l", StatusFetched (statusOf Playing (Just 2) 3)] s
  assertEqual "not unless it follows" ["A - Two"] (askedFor notFollowing)
  elsewhere <-
    runEvents 0 [key "l", key "l", StatusFetched (statusOf Playing (Just 2) 3)] following
  assertEqual "not on another screen" ["A - One"] (askedFor elsewhere)

test_toggleFollowing :: Assertion
test_toggleFollowing = do
  s <- testState (40, 10) (statusOf Playing (Just 0) 3) threeSongs
  on <- runEvents 0 [key "down", key "l", key "space"] s
  assertEqual "on" (Just "Lyrics follow playing: on") ((.text) <$> on.state.message)
  assertEqual "the song that plays at once" ["A - Two", "A - One"] (askedFor on)
  assertBool "not the queue's" (not on.state.toggles.followPlaying)
  off <- runEvents 0 [key "space"] on.state
  assertEqual "off" (Just "Lyrics follow playing: off") ((.text) <$> off.state.message)

threeSongs :: [Song]
threeSongs =
  [ song 0 [(Artist, ["A"]), (Title, ["One"])] 60
  , song 1 [(Artist, ["A"]), (Title, ["Two"])] 60
  , song 2 [(Artist, ["A"]), (Title, ["Three"])] 60
  ]

-- | The songs whose lyrics the screen asked for.
askedFor :: Result -> [T.Text]
askedFor r = [lyricsName q.song | FetchLyrics _ q <- r.commands]

test_fetchInBackground :: Assertion
test_fetchInBackground = do
  let env = testAppEnv & #config % #lyrics % #fetchInBackground .~ True
      songs =
        [ song 0 [(Artist, ["A"]), (Title, ["One"])] 60
        , song 1 [(Artist, ["A"]), (Title, ["Two"])] 60
        ]
      fetched r = [lyricsName s | FetchLyricsInBackground s <- r.commands]
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
  (.state) <$> runEvents 0 [LyricsLoaded token (LyricsFound Stored timedTwenty)] r.state

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
  -> [Song -> IO LyricsResult]
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

    expectWithin :: IO b -> IO b
    expectWithin act = timeout (5 * 1000000) act >>= maybe (assertFailure "nothing came") pure

-- | A fetcher that counts its calls.
counted :: IO LyricsResult -> IO (IORef Int, Song -> IO LyricsResult)
counted result = do
  calls <- newIORef 0
  pure (calls, \_ -> modifyIORef' calls (+ 1) >> result)

-- | The token of the one request of lyrics.
requestToken :: Result -> IO Int
requestToken r = case [t | FetchLyrics t _ <- r.commands] of
  [t] -> pure t
  other -> assertFailure $ "one request of lyrics, not " <> show (length other)

stored :: T.Text -> LyricsResult
stored = LyricsFound Stored . plainLyrics

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
