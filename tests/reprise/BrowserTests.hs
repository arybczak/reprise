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
import Reprise.Keys
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Browser
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
    , testCase "a stream isn't in the browser" test_jumpWithStream
    , testCase "a long path scrolls" test_longPath
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

test_enterPlays :: Assertion
test_enterPlays =
  assertEqual
    "requests"
    [[Request "add" ["a/x.flac", "1"], Request "play" ["1"]]]
    (press ["enter"] (onSongOfA queued)).requests

test_enterPlaysQueued :: Assertion
test_enterPlaysQueued = do
  let s = answer ["file: dir/0.flac"] (press ["2"] queued)
  assertEqual "requests" [[Request "playid" ["1"]]] (press ["enter"] s).requests

test_addPositions :: Assertion
test_addPositions = do
  let added ks = (press ks root).requests
  assertEqual "end" [[Request "add" ["a"]]] (added ["ctrl-a", "e"])
  assertEqual "next" [[Request "add" ["a", "+0"]]] (added ["ctrl-a", "n"])
  assertEqual "beginning" [[Request "add" ["a", "0"]]] (added ["ctrl-a", "b"])
  assertEqual "a playlist" [[Request "load" ["p"]]] (added ["end", "ctrl-a", "e"])
  assertEqual
    "the message"
    (Just "Added /a")
    ((.text) <$> (press ["ctrl-a", "e"] root).state.message)

test_addAndPlay :: Assertion
test_addAndPlay =
  assertEqual
    "requests"
    [[Request "load" ["p", "0:", "1"], Request "play" ["1"]]]
    (press ["end", "ctrl-a", "p"] (answer rootReply (press ["2"] queued))).requests

test_addOrRemove :: Assertion
test_addOrRemove = do
  let s = answer ["file: dir/0.flac", "file: b/x.flac"] (press ["2"] queued)
      removed = press ["space"] s
  assertEqual "removed" [[Request "deleteid" ["1"]]] removed.requests
  assertEqual "the cursor moves down" 1 (focusedView removed.state).cursor
  assertEqual
    "added"
    [[Request "add" ["b/x.flac"]]]
    (press ["space"] removed.state).requests

test_addFromPlaylist :: Assertion
test_addFromPlaylist = do
  let inP = answer ["file: a/x.flac", "file: b/y.flac"] (press ["end", "enter"] root)
  assertEqual
    "the second song"
    [[Request "load" ["p", "1:2"]]]
    (press ["end", "ctrl-a", "e"] inP).requests

test_updateCurrent :: Assertion
test_updateCurrent = do
  assertEqual "here" [[Request "update" ["b"]]] (press ["ctrl-d", "u"] onY).requests
  assertEqual "at the root" [[Request "update" []]] (press ["ctrl-d", "u"] root).requests

-- | In their order at the end. At a position, each goes before the ones
-- after it, so in reverse.
test_addSelected :: Assertion
test_addSelected = do
  let selected = (press ["insert", "end", "insert"] root).state
      added ks = (press ks selected).requests
  assertEqual
    "at the end"
    [[Request "add" ["a"], Request "load" ["p"]]]
    (added ["ctrl-a", "e"])
  assertEqual
    "next"
    [[Request "load" ["p", "0:", "+0"], Request "add" ["a", "+0"]]]
    (added ["ctrl-a", "n"])
  assertEqual
    "the message"
    (Just "Added 2 items")
    ((.text) <$> (press ["ctrl-a", "e"] selected).state.message)

test_addSelectedFromPlaylist :: Assertion
test_addSelectedFromPlaylist = do
  let inP =
        answer
          ["file: a.flac", "file: b.flac", "file: c.flac", "file: d.flac"]
          (press ["end", "enter"] root)
      r = press ["down", "shift-down", "shift-down", "down", "insert", "ctrl-a", "e"] inP
  assertEqual
    "runs"
    [[Request "load" ["p", "0:2"], Request "load" ["p", "3:4"]]]
    r.requests

test_selectRange :: Assertion
test_selectRange = do
  let r = press ["insert", "end", "insert", "ctrl-s", "r", "ctrl-a", "e"] root
  assertEqual
    "requests"
    [[Request "add" ["a"], Request "add" ["b"], Request "load" ["p"]]]
    r.requests

test_selectParent :: Assertion
test_selectParent = do
  let s = (press ["insert"] (onY & #views % mapped % #cursor .~ 0)).state
  assertEqual "nothing" mempty s.browser.selection.keys

test_selectionKept :: Assertion
test_selectionKept = do
  let selected = (press ["up", "insert"] onY).state
      relisted = answer bReply (runEvents 0 [MpdChanged [DatabaseSubsystem]] selected)
  assertEqual "kept" [SongKey "b/x.flac" Nothing] (toList relisted.browser.selection.keys)
  let gone = answer ["file: b/y.flac"] (runEvents 0 [MpdChanged [DatabaseSubsystem]] selected)
  assertEqual "the song is gone" [] (toList gone.browser.selection.keys)
  let left = answer rootReply (press ["backspace"] selected)
  assertEqual "another listing" [] (toList left.browser.selection.keys)

test_find :: Assertion
test_find = do
  let r = press ["/", "b", "enter"] root
  assertEqual "the cursor is on b" 1 (focusedView r.state).cursor
  assertEqual "the pattern" (Just "b") r.state.findPattern
  assertEqual
    "the next match wraps around"
    1
    (focusedView (press ["."] r.state).state).cursor

test_sharedPattern :: Assertion
test_sharedPattern = do
  let r = press ["/", "p", "enter", "1", "."] root
  assertEqual "the queue finds it" (Just "No match for p") ((.text) <$> r.state.message)

test_selectFound :: Assertion
test_selectFound = do
  let r = press ["/", "b", "enter", "ctrl-s", "f"] root
  assertEqual "selected" [DirectoryKey "b"] (toList r.state.browser.selection.keys)
  assertEqual "the message" (Just "1 item found and selected") ((.text) <$> r.state.message)

test_jumpToBrowser :: Assertion
test_jumpToBrowser = do
  let r = press ["G"] queued
  assertEqual "the screen" BrowserScreen (focusedView r.state).screen
  assertEqual "the listing of dir" [[Request "lsinfo" ["dir"]]] r.requests
  let s = answer ["file: dir/9.flac", "file: dir/0.flac"] r
  assertEqual "the cursor is on the song" 2 (focusedView s).cursor
  assertEqual
    "no song"
    (Just "There is no song under the cursor")
    ((.text) <$> (press ["G"] queueShown).state.message)

test_jumpWithStream :: Assertion
test_jumpWithStream = do
  let stream = song 0 [] 60 & #file .~ "http://example.com/stream"
      r = press ["G"] (testState (80, 12) (statusOf Stopped Nothing 1) [stream])
  assertEqual "no listing" [] r.requests
  assertEqual "the error" (Just True) ((.isError) <$> r.state.message)

-- | Nothing plays, and the title scrolls all the same.
test_longPath :: Assertion
test_longPath = do
  let long = "a-very-long-directory-name-that-does-not-fit-in-the-header-next-to-the-volume"
      entered = press ["enter"] (answer ["directory: " <> T.encodeUtf8 long] (press ["2"] queueShown))
  case reverse entered.pending of
    p : _ -> do
      let listed = runEvents 10 [replyTo [] p] entered.state
          redraws = [d | After d (Tick _) <- listed.commands]
      assertEqual "a redraw in a second" [1] redraws
      assertBool
        ("the start: " <> T.unpack (browserTitle listed.state))
        ("Browse: /a-very-long" `T.isPrefixOf` browserTitle listed.state)
      let later = listed.state & #now .~ 13
      assertBool
        ("three seconds later: " <> T.unpack (browserTitle later))
        ("Browse: very-long" `T.isPrefixOf` browserTitle later)
    [] -> assertFailure "no listing"

----------------------------------------
-- Helpers

-- | The queue with one song, @dir/0.flac@, with the id 1.
queued :: AppState
queued = testState (80, 12) (statusOf Stopped Nothing 1) [song 0 [] 60]

-- | The browser in a, with the cursor on its song.
onSongOfA :: AppState -> AppState
onSongOfA s =
  let r = press ["enter"] (answer rootReply (press ["2"] s))
  in (press ["down"] (answer ["file: a/x.flac"] r)).state

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
