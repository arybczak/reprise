module Reprise.Test.Screen.Browser (browserTests) where

import Data.ByteString qualified as BS
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Header
import Reprise.Keys
import Reprise.LocalFiles
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Selection
import Reprise.State
import Reprise.TagLib
import Reprise.Test.Helpers

browserTests :: TestTree
browserTests =
  testGroup
    "browser"
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
    , testGroup
        "local files"
        [ testCase "the home directory, after MPD's decoders" test_localHome
        , testCase "back to the database and again" test_localModeAgain
        , testCase "the local directory is remembered" test_localModeRemembered
        , testCase "entering a directory and leaving it" test_localEnterAndLeave
        , testCase "nothing is above the root" test_localRoot
        , testCase "the browser goes up from a directory that is gone" test_localGone
        , testCase "a change of the database lists nothing" test_localDatabaseChanged
        , testCase "a new connection asks for the decoders again" test_localReconnect
        , testCase "adding a song" test_localAdd
        , testCase "updating the current directory" test_localUpdate
        , testCase "saving a directory" test_localSaveDirectory
        , testCase "jump to the browser" test_localJump
        , testCase "adding a directory" test_localAddDirectory
        , testCase "adding a directory at a position" test_localAddDirectoryNext
        , testCase "adding and playing a directory" test_localAddAndPlay
        , testCase "adds go in in order" test_localAddsInOrder
        , testCase "adding before MPD's decoders are known" test_localAddWithoutDecoders
        , testCase "MPD refuses local files" test_localAddRefused
        ]
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
    =<< press ["u"]
    =<< onY
  assertEqual "at the root" [[Request "update" []]] . (.requests)
    =<< press ["u"]
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

-- | Hidden entries and the files whose suffixes MPD doesn't decode are left
-- out. A file whose suffix only FFmpeg's plugin decodes shows if TagLib
-- reads it, as the plugin lists formats that aren't audio too. A file of
-- another plugin shows also without tags, e.g. a tracker module.
test_localHome :: Assertion
test_localHome = do
  r <- press ["2"] =<< root
  assertEqual "the decoders" [[Request "decoders" []]] r.requests
  decoded <- answerLast decodersReply r
  assertEqual "the listing" ["/home/user"] (localListings decoded)
  listed <- listedWith homeFiles decoded
  assertEqual "no request" [] listed.requests
  let s = listed.state
  assertEqual
    "items"
    [ ".."
    , "directory /home/user/Music"
    , "song /home/user/a.flac"
    , "song /home/user/b.mod"
    , "song /home/user/video.mp4"
    ]
    (L.sort (items s))
  assertEqual
    "the song of TagLib's tags"
    [(M.fromList [(Artist, ["A"]), (Track, ["1"])], Just 61)]
    [ (entry.tags, entry.duration)
    | EntryItem (SongEntry entry) <- toList s.browser.items
    , entry.file == "/home/user/a.flac"
    ]
  assertBool
    ("the title: " <> T.unpack (shownTitle testAppEnv s))
    ("Browse local: ~" `T.isPrefixOf` shownTitle testAppEnv s)

-- | Each mode lists what it listed last, and MPD's decoders are asked for
-- once.
test_localModeAgain :: Assertion
test_localModeAgain = do
  database <- press ["2"] =<< inHome
  assertEqual "the root" [[Request "lsinfo" []]] database.requests
  again <- press ["2"] =<< answer rootReply database
  assertEqual "no request" [] again.requests
  assertEqual "the home directory" ["/home/user"] (localListings again)

-- | A local directory that the user entered is listed again after the
-- database, and each mode's cursor goes back to its item.
test_localModeRemembered :: Assertion
test_localModeRemembered = do
  entered <- press ["down", "enter"] =<< inHome
  inMusic <-
    press ["down"] . (.state)
      =<< listedWith (map (`localFile` Nothing) ["x.flac", "y.flac"]) entered
  database <- answer rootReply =<< press ["2"] inMusic.state
  assertEqual "the root's cursor" 0 (focusedView database).cursor
  onB <- press ["down", "2"] database
  assertEqual "the listing of Music" ["/home/user/Music"] (localListings onB)
  back <-
    (.state) <$> listedWith (map (`localFile` Nothing) ["x.flac", "y.flac"]) onB
  assertEqual "the cursor is on x" 1 (focusedView back).cursor
  database' <- answer rootReply =<< press ["2"] back
  assertEqual "the cursor is on b" 1 (focusedView database').cursor

test_localEnterAndLeave :: Assertion
test_localEnterAndLeave = do
  entered <- press ["down", "enter"] =<< inHome
  assertEqual "the listing of Music" ["/home/user/Music"] (localListings entered)
  inMusic <- (.state) <$> listedWith [] entered
  assertEqual "items" [".."] (items inMusic)
  left <- press ["backspace"] inMusic
  assertEqual "the listing of home" ["/home/user"] (localListings left)
  back <- (.state) <$> listedWith homeFiles left
  assertEqual "the cursor is on Music" 1 (focusedView back).cursor

test_localRoot :: Assertion
test_localRoot = do
  up <- press ["backspace"] =<< inHome
  assertEqual "the listing of /home" ["/home"] (localListings up)
  top <- press ["backspace"] . (.state) =<< listedWith [localDirectory "user"] up
  assertEqual "the listing of /" ["/"] (localListings top)
  atRoot <- (.state) <$> listedWith [localDirectory "home"] top
  assertEqual "items" ["directory /home"] (items atRoot)
  assertEqual "nothing above it" [] . localListings =<< press ["backspace"] atRoot

test_localGone :: Assertion
test_localGone = do
  entered <- press ["down", "enter"] =<< inHome
  r <-
    runEvents 0 [LocalListed (lastLocalToken entered) (Left DirectoryMissing)] entered.state
  assertEqual "the listing of home" ["/home/user"] (localListings r)
  assertEqual "no error" Nothing (message r.state)

test_localDatabaseChanged :: Assertion
test_localDatabaseChanged = do
  r <- runEvents 0 [MpdChanged [DatabaseSubsystem]] =<< inHome
  assertBool "no listing" (all (all ((/= "lsinfo") . (.command))) r.requests)
  assertEqual "no local listing" [] (localListings r)

-- | MPD may have restarted with other decoders.
test_localReconnect :: Assertion
test_localReconnect = do
  r <- runEvents 0 [MpdDisconnected "gone", MpdConnected (Version 0 24 0)] =<< inHome
  assertBool "the decoders" ([Request "decoders" []] `elem` r.requests)
  decoded <- answerLast decodersReply r
  assertEqual "the listing of home" ["/home/user"] (localListings decoded)

-- | A song that MPD can't read leaves out only itself, as in a directory.
test_localAdd :: Assertion
test_localAdd = do
  r <- press ["down", "down", "a", "e"] =<< inHome
  assertEqual "the song" [[Request "add" ["/home/user/a.flac"]]] r.requests
  added <- answerLast [] r
  assertEqual "the message" (Just "Added: a.flac") (message added.state)
  two <- press ["down", "down", "insert", "down", "insert", "a", "e"] =<< inHome
  let refused = AckError (Ack AckNoExist 0 "add" "No such song")
  failed <- failLast refused two
  assertEqual "the song after it" [[Request "add" ["/home/user/b.mod"]]] failed.requests
  done <- answerLast [] failed
  assertEqual
    "the message"
    (Just "Added 1 song, left out 1 that MPD can't read")
    (message done.state)

test_localUpdate :: Assertion
test_localUpdate = do
  r <- press ["u"] =<< inHome
  assertEqual "no request" [] r.requests
  assertEqual "the message" (Just "Local files aren't in MPD's database") (message r.state)

-- | MPD saves only the directories of its database.
test_localSaveDirectory :: Assertion
test_localSaveDirectory = do
  r <- press ["down", "e", "w"] =<< inHome
  assertEqual "no prompt" Nothing r.state.prompt
  assertEqual
    "the message"
    (Just "A local directory can't be saved as a playlist")
    (message r.state)
  asked <- press ["down", "down", "e", "w"] =<< inHome
  assertEqual "a song" (Just "Save 1 song as: ") ((.question) <$> asked.state.prompt)

-- | MPD keeps a local file in the queue by its absolute path. The other
-- browse mode then lists what it listed before.
test_localJump :: Assertion
test_localJump = do
  inQueue <- testState (80, 12) (statusOf Stopped Nothing 1) [localSong]
  database <- answer rootReply =<< press ["2", "1"] inQueue
  jumped <- press ["g", "b"] database
  assertEqual "the decoders" [[Request "decoders" []]] jumped.requests
  decoded <- answerLast decodersReply jumped
  assertEqual "the listing of Music" ["/home/user/Music"] (localListings decoded)
  s <-
    (.state) <$> listedWith [localFile "0.flac" Nothing, localFile "1.flac" Nothing] decoded
  assertEqual "the cursor is on the song" 2 (focusedView s).cursor
  assertEqual "back to the root" [[Request "lsinfo" []]] . (.requests) =<< press ["2"] s
  where
    localSong :: Song
    localSong = song 0 [] 60 & #file .~ "/home/user/Music/1.flac"

-- | MPD doesn't add a local directory, so reprise walks it and adds its
-- files. A file that MPD can't read ends the command list, so the adds
-- after it are sent again.
test_localAddDirectory :: Assertion
test_localAddDirectory = do
  r <- press ["down", "insert", "down", "insert", "a", "e"] =<< inHome
  assertEqual "the walk" [["/home/user/Music"]] [dirs | WalkLocal _ _ dirs <- r.commands]
  walked <- runEvents 0 [LocalWalked (lastWalkToken r) [musicFiles]] r.state
  let adds = map (\f -> Request "add" [f])
  assertEqual
    "the files of the walk, then the song"
    [ adds
        [ "/home/user/Music/a.flac"
        , "/home/user/Music/broken.flac"
        , "/home/user/Music/cd2/b.flac"
        , "/home/user/a.flac"
        ]
    ]
    walked.requests
  let refused = AckError (Ack AckNoExist 1 "add" "No such song")
  failed <- failLast refused walked
  assertEqual
    "the adds after the file that MPD can't read"
    [adds ["/home/user/Music/cd2/b.flac", "/home/user/a.flac"]]
    failed.requests
  done <- answerLast ["list_OK", "list_OK"] failed
  assertEqual
    "the message"
    (Just "Added 3 songs, left out 1 that MPD can't read")
    (message done.state)
  assertEqual "nothing left" [] (toList done.state.browser.localAdds)

-- | Each file goes before the ones after it, so they go in reverse.
test_localAddDirectoryNext :: Assertion
test_localAddDirectoryNext = do
  r <- press ["down", "a", "n"] =<< inHome
  walked <-
    runEvents
      0
      [LocalWalked (lastWalkToken r) [take 1 musicFiles <> drop 2 musicFiles]]
      r.state
  assertEqual
    "in reverse"
    [
      [ Request "add" ["/home/user/Music/cd2/b.flac", "+0"]
      , Request "add" ["/home/user/Music/a.flac", "+0"]
      ]
    ]
    walked.requests

test_localAddAndPlay :: Assertion
test_localAddAndPlay = do
  r <- press ["down", "a", "p"] =<< inHome
  walked <- runEvents 0 [LocalWalked (lastWalkToken r) [take 1 musicFiles]] r.state
  assertEqual "the add" [[Request "add" ["/home/user/Music/a.flac"]]] walked.requests
  added <- answerLast [] walked
  assertEqual "play from the first" [[Request "play" ["0"]]] added.requests

-- | An add waits for the adds before it, also when it has no walk, so the
-- songs go in in the order of the keys. Add and play then plays from the
-- first of its own songs.
test_localAddsInOrder :: Assertion
test_localAddsInOrder = do
  r <- press ["down", "a", "e", "down", "a", "p"] =<< inHome
  assertEqual "the song waits for the walk" [] r.requests
  walked <-
    runEvents
      0
      [LocalWalked (lastWalkToken r) [take 1 musicFiles <> drop 2 musicFiles]]
      r.state
  assertEqual
    "the files of the walk"
    [ [Request "add" ["/home/user/Music/a.flac"], Request "add" ["/home/user/Music/cd2/b.flac"]]
    ]
    walked.requests
  directory <- answerLast ["list_OK", "list_OK"] walked
  assertEqual "the song" [[Request "add" ["/home/user/a.flac"]]] directory.requests
  added <- answerLast [] directory
  assertEqual "play the song" [[Request "play" ["2"]]] added.requests
  assertEqual "nothing left" [] (toList added.state.browser.localAdds)

-- | A walk needs MPD's decoders, which a new connection asks for again.
test_localAddWithoutDecoders :: Assertion
test_localAddWithoutDecoders = do
  reconnected <- runEvents 0 [MpdConnected (Version 0 24 0)] =<< inHome
  r <- press ["down", "a", "e"] reconnected.state
  assertEqual "no walk" [] [dirs | WalkLocal _ _ dirs <- r.commands]
  assertEqual "why" (Just "MPD's decoders aren't known yet") (message r.state)
  assertEqual "nothing waits" [] (toList r.state.browser.localAdds)

-- | MPD refuses local files to a client that isn't on its socket.
test_localAddRefused :: Assertion
test_localAddRefused = do
  r <- press ["down", "a", "e"] =<< inHome
  walked <- runEvents 0 [LocalWalked (lastWalkToken r) [take 1 musicFiles]] r.state
  let refused = AckError (Ack AckPermission 0 "add" "Access to local files via TCP is not allowed")
  failed <- failLast refused walked
  assertEqual
    "the error"
    (Just "add: Access to local files via TCP is not allowed")
    (message failed.state)
  assertEqual "the add ended" [] (toList failed.state.browser.localAdds)

----------------------------------------
-- Helpers

-- | The music files of the walk of Music: a song, one that MPD can't read
-- and a song in a directory.
musicFiles :: [FilePath]
musicFiles =
  [ "/home/user/Music/a.flac"
  , "/home/user/Music/broken.flac"
  , "/home/user/Music/cd2/b.flac"
  ]

lastWalkToken :: Result -> Int
lastWalkToken r = case reverse [token | WalkLocal token _ _ <- r.commands] of
  token : _ -> token
  [] -> error "no walk"

-- | MPD's decoders: FLAC's, a tracker's, and FFmpeg's, which since MPD
-- 0.24 has the suffixes of formats that aren't audio too.
decodersReply :: [BS.ByteString]
decodersReply =
  [ "plugin: flac"
  , "suffix: flac"
  , "plugin: openmpt"
  , "suffix: mod"
  , "plugin: ffmpeg"
  , "suffix: flac"
  , "suffix: txt"
  , "suffix: mp4"
  ]

-- | The home directory: a hidden directory, a directory, a song, a tracker
-- module, which TagLib doesn't read, a picture, a text file and a video.
homeFiles :: [LocalFile]
homeFiles =
  [ localDirectory ".config"
  , localDirectory "Music"
  , localFile "a.flac" . Just $
      FileTags
        { properties = [("ARTIST", ["A"]), ("TRACKNUMBER", ["1"]), ("UNKNOWN", ["X"])]
        , seconds = Just 61
        }
  , localFile "b.mod" Nothing
  , localFile "cover.jpg" Nothing
  , localFile "notes.txt" Nothing
  , localFile "video.mp4" (Just FileTags {properties = [], seconds = Just 0})
  ]

localDirectory :: T.Text -> LocalFile
localDirectory name =
  LocalFile {name = name, isDirectory = True, lastModified = someTime, tags = Nothing}

localFile :: T.Text -> Maybe FileTags -> LocalFile
localFile name tags =
  LocalFile {name = name, isDirectory = False, lastModified = someTime, tags = tags}

someTime :: UTCTime
someTime = UTCTime (fromGregorian 2026 1 1) 0

-- | The browser in the home directory, with the cursor on its @..@.
inHome :: IO AppState
inHome =
  fmap (.state) $
    listedWith homeFiles =<< answerLast decodersReply =<< press ["2"] =<< root

-- | The local directories that a result asked to list.
localListings :: Result -> [FilePath]
localListings r = [dir | ListLocal _ _ dir <- r.commands]

lastLocalToken :: Result -> Int
lastLocalToken r = case reverse [token | ListLocal token _ _ <- r.commands] of
  token : _ -> token
  [] -> error "no local listing"

-- | Give the last local listing of a result its entries.
listedWith :: [LocalFile] -> Result -> IO Result
listedWith files r = runEvents 0 [LocalListed (lastLocalToken r) (Right files)] r.state

-- | Answer the last request of a result, and keep what it asked for.
answerLast :: [BS.ByteString] -> Result -> IO Result
answerLast ls r = case reverse r.pending of
  p : _ -> runEvents 0 [replyTo ls p] r.state
  [] -> assertFailure "no request to answer"

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
