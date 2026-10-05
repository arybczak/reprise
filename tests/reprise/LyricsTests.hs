module LyricsTests (lyricsTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
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
    , testCase "l on the lyrics goes back" test_back
    , testCase "l without a song under the cursor" test_noSong
    , testCase "the lyrics of a song that the screen left are dropped" test_stale
    , testCase "the lyrics scroll" test_scroll
    , testCase "the worker reads the stored lyrics" test_worker
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
  case [(t, s) | FetchLyrics t s <- r.commands] of
    [(token, requested)] -> do
      assertEqual "the song" "A - One" (lyricsName requested)
      loaded <- runEvents 0 [LyricsLoaded token (LyricsFound "First line\nSecond line")] r.state
      case screenLines loaded.state of
        title : _ : first : second : _ -> do
          assertBool ("the title: " <> T.unpack title) ("Lyrics: A - One" `T.isPrefixOf` title)
          assertEqual "the lyrics" ["First line", "Second line"] [first, second]
        ls -> assertFailure $ "too few lines: " <> show ls
      missing <- runEvents 0 [LyricsLoaded token LyricsMissing] r.state
      assertEqual "missing" ["No lyrics found"] (take 1 (mainLines missing.state))
    other -> assertFailure $ "one request of lyrics, not " <> show other

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
  where
    messageOf :: Result -> Maybe T.Text
    messageOf r = (.text) <$> r.state.message

test_stale :: Assertion
test_stale = do
  r <- runEvents 0 [key "l", key "1", key "down", key "l"] =<< queueShown
  case [t | FetchLyrics t _ <- r.commands] of
    [first, second] -> do
      stale <- runEvents 0 [LyricsLoaded first (LyricsFound "old")] r.state
      assertEqual "the screen stays" [KeepScreen] stale.commands
      assertEqual "nothing yet" Nothing stale.state.lyrics.result
      fresh <- runEvents 0 [LyricsLoaded second (LyricsFound "new")] stale.state
      assertEqual "the new song" ["new"] (take 1 (mainLines fresh.state))
    other -> assertFailure $ "two requests of lyrics, not " <> show other

-- | 20 lines in a main area of 6 rows.
test_scroll :: Assertion
test_scroll = do
  r <- runEvents 0 [key "l"] =<< queueShown
  let lyrics = T.unlines ["line " <> T.pack (show i) | i <- [1 .. 20 :: Int]]
      loaded = [LyricsLoaded t (LyricsFound lyrics) | FetchLyrics t _ <- r.commands]
  scrolled <- runEvents 0 (loaded <> [key "down"]) r.state
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

test_worker :: Assertion
test_worker = withSystemTempDirectory "lyrics" $ \dir -> do
  BS.writeFile (dir </> "A - One.txt") "Hello\r\nWorld\n\n"
  createDirectory (dir </> "A - Three.txt")
  requested <- newTVarIO Nothing
  events <- newTQueueIO
  let source = LyricsSource dir requested (atomically . writeTQueue events)
      ask token title = do
        atomically . writeTVar requested $
          Just (token, song 0 [(Artist, ["A"]), (Title, [title])] 60)
        expectWithin (atomically (readTQueue events))
  bracket (forkIO (lyricsWorker source)) killThread $ \_ -> do
    assertEqual "stored" (LyricsLoaded 1 (LyricsFound "Hello\nWorld")) =<< ask 1 "One"
    assertEqual "missing" (LyricsLoaded 2 LyricsMissing) =<< ask 2 "Two"
    ask 3 "Three" >>= \case
      LyricsLoaded 3 (LyricsFailed _) -> pure ()
      other -> assertFailure $ "a failure, not " <> show other
  where
    expectWithin :: IO a -> IO a
    expectWithin act = timeout (5 * 1000000) act >>= maybe (assertFailure "nothing came") pure

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
