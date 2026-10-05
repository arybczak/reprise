module BrowserTests (browserTests) where

import Data.ByteString qualified as BS
import Data.Foldable
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Utils

browserTests :: TestTree
browserTests =
  testGroup
    "Browser"
    [ testCase "showing the browser lists the root" test_showLists
    , testCase "showing it again lists nothing" test_showAgain
    , testCase "entering a directory and leaving it" test_enterAndLeave
    , testCase "the .. entry goes up" test_parentItem
    , testCase "nothing is above the root" test_aboveRoot
    , testCase "a playlist opens like a directory" test_playlist
    , testCase "a replaced listing is dropped" test_replacedListing
    , testCase "a new connection lists again" test_reconnect
    , testCase "the browser goes up from a directory that is gone" test_gone
    , testCase "another error of a listing shows" test_listingError
    , testCase "a change of the database lists again" test_databaseChanged
    , testCase "the cursor stays where its song was" test_songGone
    , testCase "stored playlists show at the root" test_storedPlaylistsChanged
    , testCase "a listing again while another screen shows" test_changedElsewhere
    , testCase "the sort modes" test_sortModes
    , testCase "a sort keeps the cursor on its item" test_sortKeepsCursor
    , testCase "a playlist keeps its order" test_playlistOrder
    ]

test_showLists :: Assertion
test_showLists = do
  let r = press ["2"] queueShown
  assertEqual "requests" [[Request "lsinfo" []]] r.requests
  assertEqual "the screen" BrowserScreen (focusedView r.state).screen

test_showAgain :: Assertion
test_showAgain = do
  let r = press ["1", "2"] root
  assertEqual "requests" [] r.requests

test_enterAndLeave :: Assertion
test_enterAndLeave = do
  let entered = press ["down", "enter"] root
  assertEqual "the listing of b" [[Request "lsinfo" ["b"]]] entered.requests
  let inB = answer ["file: b/x.flac", "Title: X"] entered
  assertEqual "items" ["..", "song b/x.flac"] (items inB)
  assertEqual "the cursor" 0 (focusedView inB).cursor
  let left = press ["backspace"] inB
  assertEqual "the listing of the root" [[Request "lsinfo" []]] left.requests
  let back = answer rootReply left
  assertEqual "the cursor is on b" 1 (focusedView back).cursor

test_parentItem :: Assertion
test_parentItem = do
  let inA = answer ["file: a/x.flac"] (press ["enter"] root)
      r = press ["enter"] inA
  assertEqual "the listing of the root" [[Request "lsinfo" []]] r.requests

test_aboveRoot :: Assertion
test_aboveRoot = assertEqual "requests" [] (press ["backspace"] root).requests

test_playlist :: Assertion
test_playlist = do
  let opened = press ["end", "enter"] root
  assertEqual "the songs of p" [[Request "listplaylistinfo" ["p"]]] opened.requests
  let inP = answer ["file: a/x.flac", "file: b/y.flac"] opened
  assertEqual "items" ["..", "song a/x.flac", "song b/y.flac"] (items inP)
  let back = answer rootReply (press ["backspace"] inP)
  assertEqual "the cursor is on p" 2 (focusedView back).cursor

-- | A key typed before a reply goes on from the listing on its way.
test_replacedListing :: Assertion
test_replacedListing = do
  let r = press ["enter", "backspace"] root
  case r.pending of
    [first, second] -> do
      let late =
            (runEvents 0 [replyTo rootReply second, replyTo ["file: a/x.flac"] first] r.state).state
      assertEqual "the root" ["directory a", "directory b", "playlist p"] (items late)
      assertEqual "the cursor is on a" 0 (focusedView late).cursor
    _ -> assertFailure $ "requests: " <> show r.requests

test_reconnect :: Assertion
test_reconnect = do
  let inB = answer ["file: b/x.flac"] (press ["down", "enter"] root)
      r = runEvents 0 [MpdDisconnected "gone", MpdConnected (Version 0 24 0)] inB
  assertBool "the listing of b" ([Request "lsinfo" ["b"]] `elem` r.requests)
  let never = runEvents 0 [MpdConnected (Version 0 24 0)] queueShown
  assertBool "no listing" (all (all ((/= "lsinfo") . (.command))) never.requests)

test_gone :: Assertion
test_gone = do
  let entered = press ["down", "enter"] root
      gone = AckError (Ack AckNoExist 0 "lsinfo" "No such directory")
      r = failLast gone entered
  assertEqual "the listing of the root" [[Request "lsinfo" []]] r.requests
  assertEqual "no error" Nothing r.state.message

test_listingError :: Assertion
test_listingError = do
  let entered = press ["down", "enter"] root
      denied = AckError (Ack AckPermission 0 "lsinfo" "you don't have permission")
      r = failLast denied entered
  assertEqual "no listing" [] r.requests
  assertEqual "the error" (Just True) ((.isError) <$> r.state.message)
  assertEqual "the root stays" ["directory a", "directory b", "playlist p"] (items r.state)
  -- Not up from b, which failed.
  assertEqual "keys go on from the root" [] (press ["backspace"] r.state).requests

test_databaseChanged :: Assertion
test_databaseChanged = do
  let r = runEvents 0 [MpdChanged [DatabaseSubsystem]] onY
  assertBool "the listing of b" ([Request "lsinfo" ["b"]] `elem` r.requests)
  let s = answer ["file: b/w.flac", "file: b/x.flac", "file: b/y.flac"] r
  assertEqual "the cursor is on y" 3 (focusedView s).cursor
  let reconnected = runEvents 0 [MpdDisconnected "gone", MpdConnected (Version 0 24 0)] s
  assertEqual "after a new connection" 2 (focusedView (answer bReply reconnected)).cursor

test_songGone :: Assertion
test_songGone = do
  let s = answer ["file: b/x.flac"] (runEvents 0 [MpdChanged [DatabaseSubsystem]] onY)
  assertEqual "the last item" 1 (focusedView s).cursor

test_storedPlaylistsChanged :: Assertion
test_storedPlaylistsChanged = do
  let changed = runEvents 0 [MpdChanged [StoredPlaylistSubsystem]]
  assertEqual "at the root" [[Request "lsinfo" []]] (changed root).requests
  assertEqual "in a directory" [] (changed onY).requests

test_changedElsewhere :: Assertion
test_changedElsewhere = do
  let r = runEvents 0 [KeyPressed (key "1"), MpdChanged [DatabaseSubsystem]] onY
      s = answer ["file: b/w.flac", "file: b/x.flac", "file: b/y.flac"] r
      shown = press ["2"] s
  assertEqual "no listing" [] shown.requests
  assertEqual "the cursor is on y" 3 (focusedView shown.state).cursor

test_sortModes :: Assertion
test_sortModes = do
  let sorted n = items (iterate nextSort mixed !! n)
  assertEqual
    "type"
    ["directory b", "directory a", "song s.flac", "song r.flac", "playlist p"]
    (sorted 0)
  assertEqual
    "name"
    ["directory a", "directory b", "song r.flac", "song s.flac", "playlist p"]
    (sorted 1)
  assertEqual
    "mtime"
    ["directory b", "directory a", "song r.flac", "song s.flac", "playlist p"]
    (sorted 2)
  assertEqual
    "format"
    ["directory a", "directory b", "song s.flac", "song r.flac", "playlist p"]
    (sorted 3)
  assertEqual
    "none"
    ["playlist p", "song s.flac", "song r.flac", "directory b", "directory a"]
    (sorted 4)
  assertEqual "type again" (sorted 0) (sorted 5)
  assertEqual "the message" (Just "Sort: name") ((.text) <$> (nextSort mixed).message)

test_sortKeepsCursor :: Assertion
test_sortKeepsCursor = do
  let onS = (press ["down", "down"] mixed).state
  assertEqual "the cursor is on s" 3 (focusedView (nextSort onS)).cursor

test_playlistOrder :: Assertion
test_playlistOrder = do
  let opened = press ["end", "enter"] (nextSort mixed)
  assertEqual
    "items"
    ["..", "song z.flac", "song a.flac"]
    (items (answer ["file: z.flac", "file: a.flac"] opened))

----------------------------------------
-- Helpers

-- | The root with entries of every kind, in no order of any sort mode.
mixed :: AppState
mixed =
  answer
    [ "playlist: p"
    , "Last-Modified: 2026-01-03T00:00:00Z"
    , "file: s.flac"
    , "Last-Modified: 2026-01-01T00:00:00Z"
    , "Artist: Abe"
    , "Title: A"
    , "file: r.flac"
    , "Last-Modified: 2026-01-04T00:00:00Z"
    , "Artist: Zed"
    , "Title: B"
    , "directory: b"
    , "Last-Modified: 2026-01-05T00:00:00Z"
    , "directory: a"
    , "Last-Modified: 2026-01-02T00:00:00Z"
    ]
    (press ["2"] queueShown)

nextSort :: AppState -> AppState
nextSort s = (press ["ctrl-t", "o"] s).state

-- | The browser in b, with the cursor on its second song.
onY :: AppState
onY = (press ["down", "down"] (answer bReply (press ["down", "enter"] root))).state

bReply :: [BS.ByteString]
bReply = ["file: b/x.flac", "file: b/y.flac"]

-- | Fail the last request of a result.
failLast :: MpdError -> Result -> Result
failLast err r = case reverse r.pending of
  p : _ -> runEvents 0 [failureOf err p] r.state
  [] -> error "no request to fail"

queueShown :: AppState
queueShown = testState (80, 12) (statusOf Stopped Nothing 0) []

-- | The browser at the root, with the cursor on its first entry.
root :: AppState
root = answer rootReply (press ["2"] queueShown)

rootReply :: [BS.ByteString]
rootReply = ["directory: a", "directory: b", "playlist: p"]

press :: [T.Text] -> AppState -> Result
press ks = runEvents 0 (map (KeyPressed . key) ks)

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec

-- | Answer the last request of a result.
answer :: [BS.ByteString] -> Result -> AppState
answer ls r = case reverse r.pending of
  p : _ -> (runEvents 0 [replyTo ls p] r.state).state
  [] -> error "no request to answer"

items :: AppState -> [T.Text]
items s = map describe (toList s.browser.items)
  where
    describe :: BrowserItem -> T.Text
    describe = \case
      ParentItem -> ".."
      EntryItem (DirectoryEntry d) -> "directory " <> d.path
      EntryItem (SongEntry entry) -> "song " <> entry.file
      EntryItem (PlaylistEntry p) -> "playlist " <> p.path
