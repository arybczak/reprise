module LyricsTests (lyricsTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text qualified as T
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
    runEvents 0 [LyricsLoaded token (LyricsFound (Fetched "LRCLIB") "Words")] fetching.state
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
  where
    firstAndLast :: [T.Text] -> [T.Text]
    firstAndLast ls = take 1 ls <> take 1 (reverse ls)

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
  (calls, fetcher) <- counted (pure (LyricsFound (Fetched "LRCLIB") "Fetched"))
  withWorker (dir </> "new") [fetcher] $ \ask -> do
    let fetched = LyricsFound (Fetched "LRCLIB") "Fetched"
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
  (_, second) <- counted (pure (LyricsFound (Fetched "B") "Words"))
  withWorker dir [first, second] $ \ask ->
    assertEqual
      "the next fetcher"
      [LyricsFetching 1, LyricsLoaded 1 (LyricsFound (Fetched "B") "Words")]
      =<< ask 1 "Two" False

test_workerRefetches :: Assertion
test_workerRefetches = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Old"
  (calls, fetcher) <- counted (pure (LyricsFound (Fetched "LRCLIB") "New"))
  withWorker dir [fetcher] $ \ask -> do
    let fetched = LyricsFound (Fetched "LRCLIB") "New"
    assertEqual "fetched" [LyricsFetching 1, LyricsLoaded 1 fetched] =<< ask 1 "One" True
    assertEqual "stored anew" "New\n" =<< BS.readFile (dir </> "A - One.txt")
  assertEqual "one fetch" 1 =<< readIORef calls

test_workerWithoutFetchers :: Assertion
test_workerWithoutFetchers = withSystemTempDirectory "lyrics" $ \dir ->
  withWorker dir [] $ \ask ->
    assertEqual "missing, not fetching" [LyricsLoaded 1 LyricsMissing] =<< ask 1 "One" True

-- | Run a worker with a directory and fetchers, and ask it for the lyrics
-- of A's songs: the events of a request, until its lyrics.
withWorker
  :: FilePath
  -> [Song -> IO LyricsResult]
  -> ((Int -> T.Text -> Bool -> IO [AppEvent]) -> IO a)
  -> IO a
withWorker dir fetchers k = do
  requested <- newTVarIO Nothing
  events <- newTQueueIO
  let source =
        LyricsSource
          { directory = dir
          , fetchers = fetchers
          , requested = requested
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
stored = LyricsFound Stored

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
