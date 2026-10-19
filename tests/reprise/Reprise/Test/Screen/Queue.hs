module Reprise.Test.Screen.Queue (queueTests) where

import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Save
import Reprise.State
import Reprise.Test.Handler.Helpers
import Reprise.Test.Helpers
import Reprise.Test.Screen.Queue.Edits

queueTests :: TestTree
queueTests =
  testGroup
    "queue"
    [ testCase "the cursor starts at the playing song" test_jumpAtStart
    , testCase "a priority for an empty queue" test_priorityEmpty
    , testCase "activate plays the song under the cursor" test_activate
    , testCase "the add keys add copies, as in the browser" test_addCopies
    , testCase "follow the playing song" test_followPlaying
    , testCase "jump to playing centers the cursor" test_jumpCenters
    , testCase "delete the marked songs" test_delete
    , testCase "input waits for an edit of the queue" test_heldInput
    , testCase "a click during an edit goes to its song" test_heldClick
    , testCase "a fetch of the queue fails" test_fetchFailed
    , testCase "input waits for an add at a position" test_addWaits
    , testCase "an edit after a failed fetch ends" test_editAfterFailedFetch
    , testCase "an edit after another client's change fetches the changes" test_editFetched
    , testCase "a change during an edit is fetched after it" test_editChanges
    , testCase "the password prompt takes keys during an edit" test_heldPassword
    , testCase "a choice takes keys during an edit" test_heldChoice
    , testCase "move the marked songs up and down" test_moveSongs
    , testCase "move songs to a place" test_moveSongsTo
    , testCase "shuffle the selection" test_shuffleSelection
    , testCase "shuffling the whole queue asks first" test_shuffleConfirm
    , queueEditsTests
    ]

test_jumpAtStart :: Assertion
test_jumpAtStart = do
  s <- testState (80, 24) (statusOf Playing (Just 3) 5) (songs 5)
  assertEqual "cursor" 3 (focusedView s).cursor

test_priorityEmpty :: Assertion
test_priorityEmpty = do
  s <- testState (80, 24) (statusOf Stopped Nothing 0) []
  r <- keys (":" : map T.singleton "priority" <> ["space", "5", "enter"]) s
  assertEqual "nothing sent" [] r.requests
  assertEqual "why" (Just "The queue is empty") (message r)

test_activate :: Assertion
test_activate = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  assertEqual "play by id" [[Request "playid" ["2"]]] . (.requests)
    =<< keys ["down", "enter"] s

-- | Song 2 plays.
test_addCopies :: Assertion
test_addCopies = do
  s <- testState (80, 24) (statusOf Playing (Just 1) 3) (songs 3)
  atEnd <- keys ["up", "a", "e"] s
  assertEqual "at the end" [[Request "add" ["dir/0.flac"]]] atEnd.requests
  assertEqual "the message" (Just "Added: A - 0") (message atEnd)
  atStart <- keys ["insert", "down", "insert", "a", "b"] s
  assertEqual
    "in their order at the beginning"
    (editRequests [Request "add" ["dir/2.flac", "0"], Request "add" ["dir/1.flac", "0"]])
    atStart.requests
  assertEqual "the songs" (Just "Added 2 songs") (message atStart)
  assertEqual "one plays where it is" [[Request "playid" ["1"]]] . (.requests)
    =<< keys ["up", "a", "p"] s
  let copies = [Request "add" ["dir/2.flac", "3"], Request "add" ["dir/1.flac", "3"]]
  assertEqual "more play from the first copy" [copies <> [Request "play" ["3"]]]
    . (.requests)
    =<< keys ["insert", "down", "insert", "a", "p"] s
  let part = song 2 [] 60 & #range ?~ SongRange 0 (Just 60)
  withPart <- testState (80, 24) (statusOf Stopped Nothing 3) (take 2 (songs 3) <> [part])
  parts <- keys ["down", "insert", "down", "insert", "a", "e"] withPart
  assertEqual "without the part" [[Request "add" ["dir/1.flac"]]] parts.requests
  assertEqual
    "a note of the part"
    (Just "Added: A - 1, without 1 part of a file")
    (message parts)
  onlyPart <- keys ["end", "a", "e"] withPart
  assertEqual "nothing" [] onlyPart.requests
  assertEqual "why" (Just "Added nothing, without 1 part of a file") (message onlyPart)
  empty <- keys ["a", "e"] =<< testState (80, 24) (statusOf Stopped Nothing 0) []
  assertEqual "an empty queue" (Just "The queue is empty") (message empty)

test_followPlaying :: Assertion
test_followPlaying = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 5) (songs 5)
  let playing p = StatusFetched (statusOf Playing (Just p) 5)
  assertEqual "off" 0 . cursor =<< runEvents 0 [playing 3] s
  on <- keys ["down", "down", "t", "f"] s
  assertEqual "on, to the song that plays" 0 (cursor on)
  another <- runEvents 0 [playing 3] on.state
  assertEqual "another song" 3 (cursor another)
  assertEqual "not again for the same song" 4 . cursor
    =<< runEvents 0 [playing 3] . (.state)
    =<< keys ["down"] another.state

test_jumpCenters :: Assertion
test_jumpCenters = do
  -- 24 rows leave 20 for the list.
  let playingAt p = testState (80, 24) (statusOf Playing (Just p) 50) (songs 50)
      positionAfter ks p = position <$> (keys ks =<< playingAt p)
  assertEqual "at the start" (30, 20) =<< positionAfter [] 30
  assertEqual "after moving away" (30, 20) =<< positionAfter ["home", "o"] 30
  assertEqual "near the top" (3, 0) =<< positionAfter ["end", "o"] 3
  assertEqual "near the bottom" (48, 30) =<< positionAfter ["home", "o"] 48
  helpShown <- keys ["t", "f", "f1"] =<< playingAt 30
  behindHelp <- runEvents 0 [StatusFetched (statusOf Playing (Just 40) 50)] helpShown.state
  assertEqual "behind the help screen" (40, 30) . position =<< keys ["1"] behindHelp.state
  stopped <- testState (80, 24) (statusOf Stopped (Just 30) 50) (songs 50)
  assertEqual "stopped" (30, 20) . position =<< keys ["home", "o"] stopped
  none <- keys ["end", "o"] =<< testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
  assertEqual "no current song" (49, 30) (position none)
  assertEqual "its message" (Just "There is no current song") (message none)

test_delete :: Assertion
test_delete = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual
    "selected, from the end"
    (editRequests [Request "delete" ["2:3"], Request "delete" ["0:1"]])
    . (.requests)
    =<< keys ["space", "down", "space", "delete"] s
  assertEqual "under the cursor" (editRequests [Request "delete" ["1:2"]]) . (.requests)
    =<< keys ["down", "delete"] s

-- | Input waits while an edit of the queue waits for its reply, and then
-- acts on the queue after the edit.
test_heldInput :: Assertion
test_heldInput = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  let st n = statusOf Stopped Nothing n & #playlistVersion .~ PlaylistVersion 2
  -- Songs 1 and 2 selected, the cursor on 3.
  twice <- keys ["down", "space", "space", "delete", "delete"] s
  assertEqual "the first delete" (editRequests [Request "delete" ["1:3"]]) twice.requests
  replied <- runEvents 0 [QueueEditStatus (deletedAt 1 3) (st 3)] twice.state
  assertEqual
    "the queue"
    ["dir/0.flac", "dir/3.flac", "dir/4.flac"]
    (queueFiles replied.state)
  -- The cursor stays on its row, the last one of the shorter queue, as if
  -- the key had come after the reply.
  assertEqual
    "the second delete, of the song under the cursor"
    (editRequests [Request "delete" ["2:3"]])
    replied.requests
  -- A click that changes nothing waits too, and doesn't keep the reply's
  -- changes from being drawn.
  clicked <- runEvents 0 [MouseClick LeftButton 0 0] =<< (.state) <$> keys ["delete"] s
  released <- runEvents 0 [QueueEditStatus (deletedAt 0 1) (st 4)] clicked.state
  assertEqual "the click's turn" Nothing released.state.pendingEdit
  assertBool "a redraw" (KeepScreen `notElem` released.commands)
  -- The input was meant for the queue after the edit.
  failed <-
    runEvents
      0
      [QueueEditFailed (AckError (Ack AckArg 0 "delete" "Bad song index"))]
      twice.state
  assertEqual "a failure drops it" [] failed.requests
  assertEqual "nothing waits" Nothing failed.state.pendingEdit
  -- The edit's reply still comes, and a key after the new connection would
  -- act on the positions before the edit.
  reconnected <-
    keys ["delete"] . (.state)
      =<< runEvents 0 [MpdConnected (Version 0 24 0) mpdSocket] twice.state
  assertEqual "a new connection keeps it" (Just 2) (heldCount reconnected)
  assertEqual "no delete" [] [() | Request "delete" _ <- concat reconnected.requests]

-- | A click on the queue during an edit goes to the song that the screen
-- showed under it, which the edit may move or delete. Row 2 shows the first
-- song.
test_heldClick :: Assertion
test_heldClick = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  deleted <- keys ["delete"] s
  let st = statusOf Stopped Nothing 4 & #playlistVersion .~ PlaylistVersion 2
      reply = QueueEditStatus (deletedAt 0 1) st
  played <- runEvents 0 [MouseClick RightButton 10 4, reply] deleted.state
  assertEqual "the song under the click" [[Request "playid" ["3"]]] played.requests
  assertEqual "the cursor on it" 1 (focusedView played.state).cursor
  gone <- runEvents 0 [MouseClick RightButton 10 2, reply] deleted.state
  assertEqual "a song that the edit deleted" [] gone.requests

-- | A fetch of the queue that fails, e.g. as its reply is larger than MPD's
-- buffer, leaves no queue, as an edit would act on the positions of a stale
-- one. A lost connection keeps it, as the next connection fetches it.
test_fetchFailed :: Assertion
test_fetchFailed = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  changed <- runEvents 0 [MpdChanged [PlaylistSubsystem]] s
  reconnected <- runEvents 0 [MpdConnected (Version 0 24 0) mpdSocket] s
  forM_ [("the changes", changed), ("the whole queue", reconnected)] $ \(what, r) -> do
    refused <- failLastWith (ConnectionError NoReply) r
    assertEqual (what <> ": no queue") [] (queueFiles refused.state)
    assertEqual (what <> ": an error") (Just True) (isError refused)
    lost <- failLastWith (ConnectionError (Broken "reset")) r
    assertEqual (what <> ": a lost connection") 5 (length (queueFiles lost.state))
  where
    failLastWith :: MpdError -> Result -> IO Result
    failLastWith err r = case reverse r.pending of
      p : _ -> runEvents 0 [failureOf err p] r.state
      [] -> assertFailure "no request to fail"

-- | An add at a position moves the songs after it, so input waits for it as
-- for an edit. Only MPD knows the added songs, so the changes come. A
-- delete of the selection that came before them would delete what the
-- selected song's position was.
test_addWaits :: Assertion
test_addWaits = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  added <- keys ["down", "down", "insert", "a", "b", "delete"] s
  assertEqual "the add" (editRequests [Request "add" ["dir/2.flac", "0"]]) added.requests
  assertEqual "the delete waits" (Just 1) (heldCount added)
  let st = statusOf Stopped Nothing 4 & #playlistVersion .~ PlaylistVersion 2
  replied <- runEvents 0 [QueueEditStatus [] st] added.state
  assertEqual
    "the changes"
    [[Request "status" [], Request "plchanges" ["1"]]]
    replied.requests
  let copy = (songs 4 !! 3) & #file .~ "dir/2.flac" & #position ?~ SongPos 0
      movedTo p i = (songs 3 !! i) & #position ?~ SongPos p
  fetched <-
    runEvents 0 [QueueEdited (st, copy : [movedTo (i + 1) i | i <- [0 .. 2]])] replied.state
  assertEqual "the selected song" (editRequests [Request "delete" ["3:4"]]) fetched.requests

-- | A fetch that fails before the reply to an edit forgets the queue, and
-- the input that waited for the edit can't act on it. It goes, so that
-- the input after it doesn't wait for good.
test_editAfterFailedFetch :: Assertion
test_editAfterFailedFetch = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  changed <- runEvents 0 [MpdChanged [PlaylistSubsystem]] s
  fetch <- case changed.pending of
    [p] -> pure p
    ps -> assertFailure $ "not one fetch: " <> show (length ps)
  deleted <- keys ["delete", "down"] changed.state
  assertEqual "the key waits" (Just 1) (heldCount deleted)
  let st = statusOf Stopped Nothing 4 & #playlistVersion .~ PlaylistVersion 3
  replied <-
    runEvents
      0
      [failureOf (ConnectionError NoReply) fetch, QueueEditStatus (deletedAt 0 1) st]
      deleted.state
  assertEqual "no queue" [] (queueFiles replied.state)
  assertEqual "the edit ended" Nothing (heldCount replied)
  assertEqual "nothing more fetched" [] replied.requests

-- | After another client's change, the status of an edit doesn't follow the
-- mirror's version, and the changes since it come instead.
test_editFetched :: Assertion
test_editFetched = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  deleted <- keys ["delete", "down"] s
  let st = statusOf Stopped Nothing 4 & #playlistVersion .~ PlaylistVersion 3
  replied <- runEvents 0 [QueueEditStatus (deletedAt 0 1) st] deleted.state
  assertEqual
    "the changes"
    [[Request "status" [], Request "plchanges" ["1"]]]
    replied.requests
  assertEqual "the key waits" (Just 1) (heldCount replied)
  let movedTo p i = (songs 5 !! i) & #position ?~ SongPos p
  fetched <-
    runEvents 0 [QueueEdited (st, [movedTo p (p + 1) | p <- [0 .. 3]])] replied.state
  assertEqual
    "the queue"
    ["dir/1.flac", "dir/2.flac", "dir/3.flac", "dir/4.flac"]
    (queueFiles fetched.state)
  assertEqual "the key's turn" Nothing fetched.state.pendingEdit

-- | A change that MPD reports while an edit waits is fetched after the
-- edit, from the version after it, instead of with every song that the
-- edit moved.
test_editChanges :: Assertion
test_editChanges = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  deleted <- keys ["delete"] s
  changed <- runEvents 0 [MpdChanged [PlaylistSubsystem]] deleted.state
  assertEqual "nothing fetched yet" [] changed.requests
  let st = statusOf Stopped Nothing 4 & #playlistVersion .~ PlaylistVersion 2
  replied <- runEvents 0 [QueueEditStatus (deletedAt 0 1) st] changed.state
  assertEqual
    "the changes after the edit"
    [[Request "status" [], Request "plchanges" ["2"]]]
    replied.requests
  failed <-
    runEvents
      0
      [QueueEditFailed (AckError (Ack AckArg 0 "delete" "Bad song index"))]
      changed.state
  assertEqual
    "the changes after a failed edit"
    [[Request "status" [], Request "plchanges" ["1"]]]
    failed.requests

-- | The password prompt takes keys while an edit waits, as the edit may
-- wait for the password.
test_heldPassword :: Assertion
test_heldPassword = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  deleted <- keys ["delete"] s
  let refusal = AckError $ Ack AckPermission 0 "delete" "you don't have permission for \"delete\""
  asked <- runEvents 0 [PasswordNeeded refusal] deleted.state
  answered <- keys (typed "secret" <> ["enter"]) asked.state
  assertEqual "the answer" [Just "secret"] (passwordAnswers answered)
  assertEqual "nothing held" (Just 0) (heldCount answered)

-- | While an edit of the queue waits, a choice that MPD's reply opened
-- takes keys, as the prompt for the password, which the edit may wait for,
-- waits for the choice.
test_heldChoice :: Assertion
test_heldChoice = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  deleted <- keys ["delete"] s
  choosing <- runEvents 0 [SaveChecked "list" SaveQueue True] deleted.state
  replaced <- keys ["r"] choosing.state
  assertEqual "the save" [()] [() | Request "save" _ <- concat replaced.requests]
  assertEqual "closed" Nothing replaced.state.prompt

test_moveSongs :: Assertion
test_moveSongs = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  up <- keys ["down", "insert", "m"] s
  assertEqual "up" (editRequests [Request "move" ["0:1", "1"]]) up.requests
  assertEqual "the cursor follows up" 0 (cursor up)
  down <- keys ["down", "insert", "n"] s
  assertEqual "down" (editRequests [Request "move" ["2:3", "1"]]) down.requests
  assertEqual "the cursor follows down" 2 (cursor down)
  assertEqual "a cursor off the selection goes to it" 0 . cursor
    =<< keys ["down", "insert", "down", "down", "m"] s
  assertEqual "to the middle selected song" 3 . cursor
    =<< keys ["down", "space", "space", "space", "n"] s
  assertEqual "also from another selected song" 3 . cursor
    =<< keys ["down", "insert", "down", "insert", "down", "insert", "n"] s
  assertEqual "nowhere if nothing moves" 2 . cursor
    =<< keys ["insert", "down", "down", "m"] s
  -- 24 rows leave 20 for the list, and 30 downs leave the cursor at its
  -- bottom.
  long <- testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
  assertEqual "a song scrolls the list as a step" (31, 12) . position
    =<< keys (replicate 30 "down" <> ["n"]) long
  assertEqual "so does a selection" (32, 14) . position
    =<< keys (replicate 30 "down" <> ["space", "space", "space", "n"]) long
  assertEqual "under the cursor" (editRequests [Request "move" ["1:2", "2"]]) . (.requests)
    =<< keys ["down", "down", "m"] s
  top <- keys ["insert", "m"] s
  assertEqual "the top stays" [] top.requests
  assertEqual "with the cursor" 0 (cursor top)

test_moveSongsTo :: Assertion
test_moveSongsTo = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  down <- keys ["insert", "end", "M"] s
  assertEqual
    "down after the cursor"
    (editRequests [Request "move" ["0:1", "4"]])
    down.requests
  assertEqual "the cursor stays on its song" 3 (cursor down)
  up <- keys ["down", "down", "down", "insert", "up", "up", "M"] s
  assertEqual "up after the cursor" (editRequests [Request "move" ["3:4", "2"]]) up.requests
  assertEqual "the cursor stays" 1 (cursor up)
  -- Song 2 is in place already.
  around <- keys ["insert", "down", "down", "insert", "up", "M"] s
  assertEqual
    "from around the cursor"
    (editRequests [Request "move" ["0:1", "1"]])
    around.requests
  assertEqual "the cursor stays on song 1" 0 (cursor around)
  assertEqual "among the selected songs" (Just "The cursor is among the selected songs")
    . message
    =<< keys ["insert", "M"] s
  assertEqual "without a selection" (Just "Select the songs to move first") . message
    =<< keys ["M"] s
  assertEqual "end" (editRequests [Request "move" ["0:1", "4"]]) . (.requests)
    =<< keys ["insert", "e", "m", "e"] s
  assertEqual "beginning" (editRequests [Request "move" ["2:3", "0"]]) . (.requests)
    =<< keys ["down", "down", "insert", "e", "m", "b"] s
  assertEqual "next, without a current song" (Just "There is no current song") . message
    =<< keys ["end", "e", "m", "n"] s
  playing <- testState (80, 24) (statusOf Playing (Just 1) 5) (songs 5)
  assertEqual "next, the song under the cursor" (editRequests [Request "move" ["4:5", "2"]])
    . (.requests)
    =<< keys ["end", "e", "m", "n"] playing
  assertEqual
    "the playing song among them"
    (Just "The current song is among the songs to move")
    . message
    =<< keys ["home", "down", "insert", "e", "m", "n"] playing
  -- The cursor jumps to the first one of the songs. 24 rows leave 20 for
  -- the list.
  long <- testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
  let selectFive = replicate 5 "space"
  assertEqual "the cursor at the beginning" (0, 0) . position
    =<< keys (replicate 30 "down" <> selectFive <> ["e", "m", "b"]) long
  assertEqual "the cursor at the end" (45, 30) . position
    =<< keys (selectFive <> ["e", "m", "e"]) long
  playingLong <- testState (80, 24) (statusOf Playing (Just 30) 50) (songs 50)
  assertEqual "the cursor after the playing song" (28, 18) . position
    =<< keys (["home"] <> replicate 3 "space" <> ["e", "m", "n"]) playingLong

test_shuffleSelection :: Assertion
test_shuffleSelection = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  r <- keys ["space", "space", "e", "s"] s
  assertEqual "next to each other" (editRequests [Request "shuffle" ["0:2"]]) r.requests
  assertEqual "message" (Just "Shuffled 2 songs") (message r)
  apart <- keys ["space", "down", "space", "e", "s"] s
  assertEqual "apart" [] apart.requests
  assertEqual "error" (Just True) (isError apart)

test_shuffleConfirm :: Assertion
test_shuffleConfirm = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  asked <- keys ["e", "s"] s
  assertEqual
    "question"
    (Just "Shuffle 5 songs in the queue?")
    ((.question) <$> asked.state.prompt)
  assertEqual "nothing yet" [] asked.requests
  assertEqual "yes" (editRequests [Request "shuffle" []]) . (.requests)
    =<< keys ["y"] asked.state
  assertEqual "no" [] . (.requests) =<< keys ["n"] asked.state
  elsewhere <- keys ["8", "e", "s"] s
  assertEqual
    "no question on another screen"
    Nothing
    ((.question) <$> elsewhere.state.prompt)
  assertEqual "nothing on another screen" [] elsewhere.requests

----------------------------------------
-- Helpers

-- | The delete of the songs from the first position up to the second one.
deletedAt :: Int -> Int -> [QueueEdit]
deletedAt a b = [DeleteRange (Range (SongPos a) (Just (SongPos b)))]

queueFiles :: AppState -> [T.Text]
queueFiles = map (.file) . toList . (.mirror.queue)

-- | The number of the events that wait for an edit, if one waits.
heldCount :: Result -> Maybe Int
heldCount r = Seq.length . (.heldInput) <$> r.state.pendingEdit
