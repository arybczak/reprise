module Reprise.Test.Lyrics.Worker (lyricsWorkerTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.IORef.Strict qualified as S
import Data.Maybe
import Data.Text qualified as T
import Optics.Core
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Event
import Reprise.Lyrics
import Reprise.Lyrics.Worker
import Reprise.Mpd.Protocol.Types
import Reprise.Test.Helpers
import Reprise.Test.Lyrics.Helpers

lyricsWorkerTests :: TestTree
lyricsWorkerTests =
  testGroup
    "lyrics worker"
    [ testCase "the worker reads the stored lyrics" test_worker
    , testCase "the worker fetches and stores the lyrics" test_workerFetches
    , testCase "the worker asks again for what wasn't there" test_workerAsksAgain
    , testCase "the worker asks the fetchers in order" test_workerFetchers
    , testCase "the worker fetches the lyrics again" test_workerRefetches
    , testCase "a refetch that finds nothing keeps the stored lyrics" test_workerKeepsStored
    , testCase "LRC without times is stored lyrics" test_workerUntimedLrc
    , testCase "the worker without fetchers" test_workerWithoutFetchers
    , testCase "the worker stores and reads the times" test_workerTimes
    , testCase "the worker fetches in the background" test_workerInBackground
    , testCase "a request of the screen doesn't wait" test_workerTakesTurns
    , testCase "a request of the screen takes over its song's fetch" test_workerTakesOver
    ]

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

----------------------------------------
-- Helpers

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
