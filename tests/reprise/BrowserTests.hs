module BrowserTests (browserTests) where

import Data.ByteString qualified as BS
import Data.Foldable
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Header
import Reprise.Keys
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Selection
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
    , testCase "enter held on .. goes up once" test_parentHeld
    , testCase "nothing is above the root" test_aboveRoot
    , testCase "a playlist opens like a directory" test_playlist
    , testCase "a replaced listing is dropped" test_replacedListing
    , testCase "a new connection lists again" test_reconnect
    , testCase "the browser goes up from a directory that is gone" test_gone
    , testCase "another error of a listing shows" test_listingError
    , testCase "a new connection lists what a lost one failed" test_listingLost
    , testCase "a change of the database lists again" test_databaseChanged
    , testCase "the cursor stays where its song was" test_songGone
    , testCase "stored playlists show at the root" test_storedPlaylistsChanged
    , testCase "a listing again while another screen shows" test_changedElsewhere
    , testCase "the sort modes" test_sortModes
    , testCase "a sort keeps the cursor on its item" test_sortKeepsCursor
    , testCase "a playlist keeps its order" test_playlistOrder
    , testCase "enter plays a song" test_enterPlays
    , testCase "enter plays a song that is in the queue there" test_enterPlaysQueued
    , testCase "adding at a position" test_addPositions
    , testCase "add and play" test_addAndPlay
    , testCase "add or remove" test_addOrRemove
    , testCase "the songs of a playlist are loaded from it" test_addFromPlaylist
    , testCase "updating the current directory" test_updateCurrent
    , testCase "adding the selected items" test_addSelected
    , testCase "adding the selected songs of a playlist" test_addSelectedFromPlaylist
    , testCase "a range" test_selectRange
    , testCase ".. can't be selected" test_selectParent
    , testCase "the selection stays in the same listing" test_selectionKept
    , testCase "find" test_find
    , testCase "the pattern of a find is shared by the screens" test_sharedPattern
    , testCase "selecting what was found" test_selectFound
    , testCase "jump to the browser" test_jumpToBrowser
    , testCase "jump to the playing song" test_jumpToPlaying
    , testCase "a stream isn't in the browser" test_jumpWithStream
    , testCase "a long path scrolls" test_longPath
    ]

test_showLists :: Assertion
test_showLists = do
  r <- press ["2"] =<< queueShown
  assertEqual "requests" [[Request "lsinfo" []]] r.requests
  assertEqual "the screen" BrowserScreen (focusedView r.state).screen

test_showAgain :: Assertion
test_showAgain = assertEqual "requests" [] . (.requests) =<< press ["1", "2"] =<< root

test_enterAndLeave :: Assertion
test_enterAndLeave = do
  entered <- press ["down", "enter"] =<< root
  assertEqual "the listing of b" [[Request "lsinfo" ["b"]]] entered.requests
  inB <- answer ["file: b/x.flac", "Title: X"] entered
  assertEqual "items" ["..", "song b/x.flac"] (items inB)
  assertEqual "the cursor" 0 (focusedView inB).cursor
  left <- press ["backspace"] inB
  assertEqual "the listing of the root" [[Request "lsinfo" []]] left.requests
  back <- answer rootReply left
  assertEqual "the cursor is on b" 1 (focusedView back).cursor

test_parentItem :: Assertion
test_parentItem = do
  inA <- answer ["file: a/x.flac"] =<< press ["enter"] =<< root
  assertEqual "the listing of the root" [[Request "lsinfo" []]] . (.requests)
    =<< press ["enter"] inA

-- | Until the reply, the screen shows the listing of a/c, with the cursor
-- on its @..@, so enter held on it lists a each time.
test_parentHeld :: Assertion
test_parentHeld = do
  inA <- answer ["directory: a/c"] =<< press ["enter"] =<< root
  inC <- answer ["file: a/c/x.flac"] =<< press ["down", "enter"] inA
  r <- press ["enter", "enter"] inC
  assertEqual "the listings of a" (replicate 2 [Request "lsinfo" ["a"]]) r.requests

test_aboveRoot :: Assertion
test_aboveRoot = assertEqual "requests" [] . (.requests) =<< press ["backspace"] =<< root

test_playlist :: Assertion
test_playlist = do
  opened <- press ["end", "enter"] =<< root
  assertEqual "the songs of p" [[Request "listplaylistinfo" ["p"]]] opened.requests
  inP <- answer ["file: a/x.flac", "file: b/y.flac"] opened
  assertEqual "items" ["..", "song a/x.flac", "song b/y.flac"] (items inP)
  back <- answer rootReply =<< press ["backspace"] inP
  assertEqual "the cursor is on p" 2 (focusedView back).cursor

-- | A key typed before a reply goes on from the listing on its way.
test_replacedListing :: Assertion
test_replacedListing = do
  r <- press ["enter", "backspace"] =<< root
  case r.pending of
    [first, second] -> do
      late <-
        (.state)
          <$> runEvents 0 [replyTo rootReply second, replyTo ["file: a/x.flac"] first] r.state
      assertEqual "the root" ["directory a", "directory b", "playlist p"] (items late)
      assertEqual "the cursor is on a" 0 (focusedView late).cursor
    _ -> assertFailure $ "requests: " <> show r.requests

test_reconnect :: Assertion
test_reconnect = do
  inB <- answer ["file: b/x.flac"] =<< press ["down", "enter"] =<< root
  r <- runEvents 0 [MpdDisconnected "gone", MpdConnected (Version 0 24 0)] inB
  assertBool "the listing of b" ([Request "lsinfo" ["b"]] `elem` r.requests)
  never <- runEvents 0 [MpdConnected (Version 0 24 0)] =<< queueShown
  assertBool "no listing" (all (all ((/= "lsinfo") . (.command))) never.requests)

test_gone :: Assertion
test_gone = do
  let gone = AckError (Ack AckNoExist 0 "lsinfo" "No such directory")
  r <- failLast gone =<< press ["down", "enter"] =<< root
  assertEqual "the listing of the root" [[Request "lsinfo" []]] r.requests
  assertEqual "no error" Nothing r.state.message

test_listingError :: Assertion
test_listingError = do
  let denied = AckError (Ack AckPermission 0 "lsinfo" "you don't have permission")
  r <- failLast denied =<< press ["down", "enter"] =<< root
  assertEqual "no listing" [] r.requests
  assertEqual "the error" (Just True) ((.isError) <$> r.state.message)
  assertEqual "the root stays" ["directory a", "directory b", "playlist p"] (items r.state)
  -- Not up from b, which failed.
  assertEqual "keys go on from the root" [] . (.requests) =<< press ["backspace"] r.state

-- | The first listing, which a lost connection failed, comes with the next
-- connection.
test_listingLost :: Assertion
test_listingLost = do
  lost <- failLast (ConnectionError (Broken "reset")) =<< press ["2"] =<< queueShown
  assertEqual "the error" (Just True) ((.isError) <$> lost.state.message)
  r <- runEvents 0 [MpdDisconnected "reset", MpdConnected (Version 0 24 0)] lost.state
  assertBool "the listing of the root" ([Request "lsinfo" []] `elem` r.requests)

test_databaseChanged :: Assertion
test_databaseChanged = do
  r <- runEvents 0 [MpdChanged [DatabaseSubsystem]] =<< onY
  assertBool "the listing of b" ([Request "lsinfo" ["b"]] `elem` r.requests)
  s <- answer ["file: b/w.flac", "file: b/x.flac", "file: b/y.flac"] r
  assertEqual "the cursor is on y" 3 (focusedView s).cursor
  reconnected <- runEvents 0 [MpdDisconnected "gone", MpdConnected (Version 0 24 0)] s
  assertEqual "after a new connection" 2 . (.cursor) . focusedView
    =<< answer bReply reconnected

test_songGone :: Assertion
test_songGone = do
  s <- answer ["file: b/x.flac"] =<< runEvents 0 [MpdChanged [DatabaseSubsystem]] =<< onY
  assertEqual "the last item" 1 (focusedView s).cursor

test_storedPlaylistsChanged :: Assertion
test_storedPlaylistsChanged = do
  let changed = fmap (.requests) . runEvents 0 [MpdChanged [StoredPlaylistSubsystem]]
  assertEqual "at the root" [[Request "lsinfo" []]] =<< changed =<< root
  assertEqual "in a directory" [] =<< changed =<< onY

test_changedElsewhere :: Assertion
test_changedElsewhere = do
  r <- runEvents 0 [KeyPressed (key "1"), MpdChanged [DatabaseSubsystem]] =<< onY
  shown <- press ["2"] =<< answer ["file: b/w.flac", "file: b/x.flac", "file: b/y.flac"] r
  assertEqual "no listing" [] shown.requests
  assertEqual "the cursor is on y" 3 (focusedView shown.state).cursor

test_sortModes :: Assertion
test_sortModes = do
  let sorted n = items <$> iterate (>>= nextSort) mixed !! n
  byType <- sorted 0
  assertEqual
    "type"
    ["directory b", "directory a", "song s.flac", "song r.flac", "playlist p"]
    byType
  assertEqual
    "name"
    ["directory a", "directory b", "song r.flac", "song s.flac", "playlist p"]
    =<< sorted 1
  assertEqual
    "mtime"
    ["directory b", "directory a", "song r.flac", "song s.flac", "playlist p"]
    =<< sorted 2
  assertEqual
    "format"
    ["directory a", "directory b", "song s.flac", "song r.flac", "playlist p"]
    =<< sorted 3
  assertEqual
    "none"
    ["playlist p", "song s.flac", "song r.flac", "directory b", "directory a"]
    =<< sorted 4
  assertEqual "type again" byType =<< sorted 5
  assertEqual "the message" (Just "Sort: name") . message =<< nextSort =<< mixed

test_sortKeepsCursor :: Assertion
test_sortKeepsCursor = do
  onS <- (.state) <$> (press ["down", "down"] =<< mixed)
  assertEqual "the cursor is on s" 3 . (.cursor) . focusedView =<< nextSort onS

test_playlistOrder :: Assertion
test_playlistOrder = do
  opened <- press ["end", "enter"] =<< nextSort =<< mixed
  assertEqual "items" ["..", "song z.flac", "song a.flac"] . items
    =<< answer ["file: z.flac", "file: a.flac"] opened

test_enterPlays :: Assertion
test_enterPlays =
  assertEqual "requests" [[Request "add" ["a/x.flac", "1"], Request "play" ["1"]]]
    . (.requests)
    =<< press ["enter"]
    =<< onSongOfA
    =<< queued

test_enterPlaysQueued :: Assertion
test_enterPlaysQueued = do
  s <- answer ["file: dir/0.flac"] =<< press ["2"] =<< queued
  assertEqual "requests" [[Request "playid" ["1"]]] . (.requests) =<< press ["enter"] s

test_addPositions :: Assertion
test_addPositions = do
  s <- root
  let added ks = (.requests) <$> press ks s
  assertEqual "end" [[Request "add" ["a"]]] =<< added ["a", "e"]
  assertEqual "next" [[Request "add" ["a", "+0"]]] =<< added ["a", "n"]
  assertEqual "beginning" [[Request "add" ["a", "0"]]] =<< added ["a", "b"]
  assertEqual "a playlist" [[Request "load" ["p"]]] =<< added ["end", "a", "e"]
  assertEqual "the message" (Just "Added /a") . message . (.state)
    =<< press ["a", "e"] s

test_addAndPlay :: Assertion
test_addAndPlay =
  assertEqual "requests" [[Request "load" ["p", "0:", "1"], Request "play" ["1"]]]
    . (.requests)
    =<< press ["end", "a", "p"]
    =<< answer rootReply
    =<< press ["2"]
    =<< queued

test_addOrRemove :: Assertion
test_addOrRemove = do
  s <- answer ["file: dir/0.flac", "file: b/x.flac"] =<< press ["2"] =<< queued
  removed <- press ["space"] s
  assertEqual "removed" [[Request "deleteid" ["1"]]] removed.requests
  assertEqual "the cursor moves down" 1 (focusedView removed.state).cursor
  assertEqual "added" [[Request "add" ["b/x.flac"]]] . (.requests)
    =<< press ["space"] removed.state

test_addFromPlaylist :: Assertion
test_addFromPlaylist = do
  inP <- answer ["file: a/x.flac", "file: b/y.flac"] =<< press ["end", "enter"] =<< root
  assertEqual "the second song" [[Request "load" ["p", "1:2"]]] . (.requests)
    =<< press ["end", "a", "e"] inP

test_updateCurrent :: Assertion
test_updateCurrent = do
  assertEqual "here" [[Request "update" ["b"]]] . (.requests)
    =<< press ["d", "u"]
    =<< onY
  assertEqual "at the root" [[Request "update" []]] . (.requests)
    =<< press ["d", "u"]
    =<< root

-- | In their order at the end. At a position, each goes before the ones
-- after it, so in reverse.
test_addSelected :: Assertion
test_addSelected = do
  selected <- (.state) <$> (press ["insert", "end", "insert"] =<< root)
  let added ks = (.requests) <$> press ks selected
  assertEqual "at the end" [[Request "add" ["a"], Request "load" ["p"]]]
    =<< added ["a", "e"]
  assertEqual "next" [[Request "load" ["p", "0:", "+0"], Request "add" ["a", "+0"]]]
    =<< added ["a", "n"]
  assertEqual "the message" (Just "Added 2 items") . message . (.state)
    =<< press ["a", "e"] selected

test_addSelectedFromPlaylist :: Assertion
test_addSelectedFromPlaylist = do
  inP <-
    answer ["file: a.flac", "file: b.flac", "file: c.flac", "file: d.flac"]
      =<< press ["end", "enter"]
      =<< root
  r <- press ["down", "shift-down", "shift-down", "down", "insert", "a", "e"] inP
  assertEqual
    "runs"
    [[Request "load" ["p", "0:2"], Request "load" ["p", "3:4"]]]
    r.requests

test_selectRange :: Assertion
test_selectRange = do
  r <- press ["insert", "end", "insert", "v", "r", "a", "e"] =<< root
  assertEqual
    "requests"
    [[Request "add" ["a"], Request "add" ["b"], Request "load" ["p"]]]
    r.requests

test_selectParent :: Assertion
test_selectParent = do
  s <- (.state) <$> (press ["insert"] . (#views % mapped % #cursor .~ 0) =<< onY)
  assertEqual "nothing" mempty s.browser.selection.keys

test_selectionKept :: Assertion
test_selectionKept = do
  selected <- (.state) <$> (press ["up", "insert"] =<< onY)
  let relisted reply = answer reply =<< runEvents 0 [MpdChanged [DatabaseSubsystem]] selected
  assertEqual "kept" [SongKey "b/x.flac" Nothing] . selectedKeys =<< relisted bReply
  assertEqual "the song is gone" [] . selectedKeys =<< relisted ["file: b/y.flac"]
  assertEqual "another listing" [] . selectedKeys
    =<< answer rootReply
    =<< press ["backspace"] selected
  where
    selectedKeys :: AppState -> [ItemKey]
    selectedKeys s = toList s.browser.selection.keys

test_find :: Assertion
test_find = do
  r <- press ["/", "b", "enter"] =<< root
  assertEqual "the cursor is on b" 1 (focusedView r.state).cursor
  assertEqual "the pattern" (Just "b") r.state.findPattern
  assertEqual "the next match wraps around" 1 . (.cursor) . focusedView . (.state)
    =<< press ["."] r.state

test_sharedPattern :: Assertion
test_sharedPattern =
  assertEqual "the queue finds it" (Just "No match for p") . message . (.state)
    =<< press ["/", "p", "enter", "1", "."]
    =<< root

test_selectFound :: Assertion
test_selectFound = do
  r <- press ["/", "b", "enter", "v", "f"] =<< root
  assertEqual "selected" [DirectoryKey "b"] (toList r.state.browser.selection.keys)
  assertEqual "the message" (Just "1 item found and selected") (message r.state)

test_jumpToBrowser :: Assertion
test_jumpToBrowser = do
  r <- press ["g", "b"] =<< queued
  assertEqual "the screen" BrowserScreen (focusedView r.state).screen
  assertEqual "the listing of dir" [[Request "lsinfo" ["dir"]]] r.requests
  s <- answer ["file: dir/9.flac", "file: dir/0.flac"] r
  assertEqual "the cursor is on the song" 2 (focusedView s).cursor
  assertEqual "no song" (Just "There is no song under the cursor") . message . (.state)
    =<< press ["g", "b"]
    =<< queueShown
  assertEqual "a screen without songs" (Just "The help screen has no songs")
    . message
    . (.state)
    =<< press ["f1", "g", "b"]
    =<< queued

test_jumpToPlaying :: Assertion
test_jumpToPlaying = do
  playing <- testState (80, 12) (statusOf Playing (Just 0) 1) [song 0 [] 60]
  r <- press ["o"] =<< answer rootReply =<< press ["2"] playing
  assertEqual "the listing of dir" [[Request "lsinfo" ["dir"]]] r.requests
  s <- answer ["file: dir/9.flac", "file: dir/0.flac"] r
  assertEqual "the cursor is on the song" 2 (focusedView s).cursor
  assertEqual "no current song" (Just "There is no current song") . message . (.state)
    =<< press ["o"]
    =<< root

test_jumpWithStream :: Assertion
test_jumpWithStream = do
  let stream = song 0 [] 60 & #file .~ "http://example.com/stream"
  r <- press ["g", "b"] =<< testState (80, 12) (statusOf Stopped Nothing 1) [stream]
  assertEqual "no listing" [] r.requests
  assertEqual "the error" (Just True) ((.isError) <$> r.state.message)
  assertEqual "the queue stays" QueueScreen (focusedView r.state).screen

-- | Nothing plays, and the title scrolls all the same.
test_longPath :: Assertion
test_longPath = do
  let long = "a-very-long-directory-name-that-does-not-fit-in-the-header-next-to-the-volume"
  entered <-
    press ["enter"]
      =<< answer ["directory: " <> T.encodeUtf8 long]
      =<< press ["2"]
      =<< queueShown
  case reverse entered.pending of
    p : _ -> do
      listed <- runEvents 10 [replyTo [] p] entered.state
      let redraws = [d | After d (Tick _) <- listed.commands]
      assertEqual "a redraw in a second" [1] redraws
      let title = shownTitle testAppEnv
      assertBool
        ("the start: " <> T.unpack (title listed.state))
        ("Browse: /a-very-long" `T.isPrefixOf` title listed.state)
      let later = listed.state & #now .~ 13
      assertBool
        ("three seconds later: " <> T.unpack (title later))
        ("Browse: very-long" `T.isPrefixOf` title later)
    [] -> assertFailure "no listing"

----------------------------------------
-- Helpers

-- | The queue with one song, @dir/0.flac@, with the id 1.
queued :: IO AppState
queued = testState (80, 12) (statusOf Stopped Nothing 1) [song 0 [] 60]

-- | The browser in a, with the cursor on its song.
onSongOfA :: AppState -> IO AppState
onSongOfA s = do
  r <- press ["enter"] =<< answer rootReply =<< press ["2"] s
  (.state) <$> (press ["down"] =<< answer ["file: a/x.flac"] r)

-- | The root with entries of every kind, in no order of any sort mode.
mixed :: IO AppState
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
    =<< press ["2"]
    =<< queueShown

nextSort :: AppState -> IO AppState
nextSort s = (.state) <$> press ["t", "o"] s

-- | The browser in b, with the cursor on its second song.
onY :: IO AppState
onY =
  (.state)
    <$> (press ["down", "down"] =<< answer bReply =<< press ["down", "enter"] =<< root)

bReply :: [BS.ByteString]
bReply = ["file: b/x.flac", "file: b/y.flac"]

-- | Fail the last request of a result.
failLast :: MpdError -> Result -> IO Result
failLast err r = case reverse r.pending of
  p : _ -> runEvents 0 [failureOf err p] r.state
  [] -> assertFailure "no request to fail"

queueShown :: IO AppState
queueShown = testState (80, 12) (statusOf Stopped Nothing 0) []

-- | The browser at the root, with the cursor on its first entry.
root :: IO AppState
root = answer rootReply =<< press ["2"] =<< queueShown

rootReply :: [BS.ByteString]
rootReply = ["directory: a", "directory: b", "playlist: p"]

press :: [T.Text] -> AppState -> IO Result
press ks = runEvents 0 (map (KeyPressed . key) ks)

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec

-- | Answer the last request of a result.
answer :: [BS.ByteString] -> Result -> IO AppState
answer ls r = case reverse r.pending of
  p : _ -> (.state) <$> runEvents 0 [replyTo ls p] r.state
  [] -> assertFailure "no request to answer"

items :: AppState -> [T.Text]
items s = map describe (toList s.browser.items)
  where
    describe :: BrowserItem -> T.Text
    describe = \case
      ParentItem -> ".."
      EntryItem (DirectoryEntry d) -> "directory " <> d.path
      EntryItem (SongEntry entry) -> "song " <> entry.file
      EntryItem (PlaylistEntry p) -> "playlist " <> p.path

message :: AppState -> Maybe T.Text
message s = (.text) <$> s.message
