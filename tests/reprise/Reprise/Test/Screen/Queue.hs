module Reprise.Test.Screen.Queue (queueTests) where

import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Optics.Core hiding (below, elements)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Groups
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Connection
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Mpd.TestServer
import Reprise.Save
import Reprise.Screen.Queue.Edits
import Reprise.State
import Reprise.Test.Handler.Helpers
import Reprise.Test.Helpers

queueTests :: TestTree
queueTests =
  testGroup
    "Queue"
    [ testCase "runs" test_runs
    , testCase "delete from the last run" test_delete
    , testCase "move runs up and down" test_moveUpDown
    , testCase "move before a position" test_moveBefore
    , testCase "move to the start" test_moveToStart
    , testCase "move after a song" test_moveAfter
    , testCase "the cursor starts at the playing song" test_jumpAtStart
    , testCase "a priority for an empty queue" test_priorityEmpty
    , testCase "activate plays the song under the cursor" test_activate
    , testCase "follow the playing song" test_followPlaying
    , testCase "jump to playing centers the cursor" test_jumpCenters
    , testCase "delete the marked songs" test_deleteMarked
    , testCase "input waits for an edit of the queue" test_heldInput
    , testCase "the password prompt takes keys during an edit" test_heldPassword
    , testCase "a choice takes keys during an edit" test_heldChoice
    , testCase "move the marked songs up and down" test_moveSongs
    , testCase "move songs to a place" test_moveSongsTo
    , testCase "shuffle the selection" test_shuffleSelection
    , testCase "shuffling the whole queue asks first" test_shuffleConfirm
    , withResource (startTestServer testSongs) stopTestServer $ \getServer ->
        testProperty "MPD gives the queue of the model" (prop_model getServer)
    ]

test_runs :: Assertion
test_runs = do
  assertEqual "empty" [] (runs [])
  assertEqual "runs" [(0, 1), (3, 3), (5, 7)] (runs [0, 1, 3, 5, 6, 7])

test_delete :: Assertion
test_delete =
  assertEqual
    "requests"
    [Request "delete" ["5:6"], Request "delete" ["1:3"]]
    (commandRequests $ deletePositions [1, 2, 5])

test_moveUpDown :: Assertion
test_moveUpDown = do
  assertEqual
    "up, with a run at the top that stays"
    [Request "move" ["2:3", "3"]]
    (commandRequests $ moveUp [0, 3])
  assertEqual
    "down, with a run at the bottom that stays"
    [Request "move" ["2:3", "1"]]
    (commandRequests $ moveDown 5 [1, 4])

test_moveBefore :: Assertion
test_moveBefore = do
  assertEqual "inside the songs" Nothing (commandRequests <$> moveBefore [1, 3] 2)
  assertEqual
    "down, from the last song"
    (Just [Request "move" ["3:4", "4"], Request "move" ["1:2", "3"]])
    (commandRequests <$> moveBefore [1, 3] 5)
  assertEqual
    "up, from the first song"
    (Just [Request "move" ["2:3", "0"], Request "move" ["4:5", "1"]])
    (commandRequests <$> moveBefore [2, 4] 0)
  assertEqual
    "a run as one range"
    (Just [Request "move" ["4:5", "5"], Request "move" ["1:3", "3"]])
    (commandRequests <$> moveBefore [1, 2, 4] 6)
  assertEqual "in place already" (Just []) (commandRequests <$> moveBefore [3, 4] 5)

-- | A song at the start already stays, and the others join it, where
-- 'moveBefore' with the first position does nothing.
test_moveToStart :: Assertion
test_moveToStart = do
  assertEqual "before the first song" Nothing (commandRequests <$> moveBefore [0, 3] 0)
  assertEqual
    "the others join"
    [Request "move" ["3:4", "1"]]
    (commandRequests $ moveToStart [0, 3])

-- | Songs on both sides of the current song at 2: the one below moves up
-- first, and then the one above moves down.
test_moveAfter :: Assertion
test_moveAfter =
  assertEqual
    "below, then above"
    [Request "move" ["4:5", "3"], Request "move" ["0:1", "2"]]
    (commandRequests $ moveAfter 2 [0, 4])

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

test_followPlaying :: Assertion
test_followPlaying = do
  s <- keys ["t", "f"] =<< testState (80, 24) (statusOf Playing (Just 0) 5) (songs 5)
  assertEqual "cursor" 3 . cursor
    =<< runEvents 0 [StatusFetched (statusOf Playing (Just 3) 5)] s.state

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

test_deleteMarked :: Assertion
test_deleteMarked = do
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
      movedTo p i = (songs 5 !! i) & #position ?~ SongPos p
      files = map (.file) . toList . (.mirror.queue)
  -- Songs 1 and 2 selected, the cursor on 3.
  twice <- keys ["down", "space", "space", "delete", "delete"] s
  assertEqual "the first delete" (editRequests [Request "delete" ["1:3"]]) twice.requests
  replied <- runEvents 0 [QueueEdited (st 3, [movedTo 1 3, movedTo 2 4])] twice.state
  assertEqual "the queue" ["dir/0.flac", "dir/3.flac", "dir/4.flac"] (files replied.state)
  -- The cursor stays on its row, the last one of the shorter queue, as if
  -- the key had come after the reply.
  assertEqual
    "the second delete, of the song under the cursor"
    [[Request "delete" ["2:3"], Request "status" [], Request "plchanges" ["2"]]]
    replied.requests
  -- A click that changes nothing waits too, and doesn't keep the reply's
  -- changes from being drawn.
  clicked <- runEvents 0 [MouseClick LeftButton 0 0] =<< (.state) <$> keys ["delete"] s
  released <-
    runEvents
      0
      [QueueEdited (st 4, map (uncurry movedTo) [(0, 1), (1, 2), (2, 3), (3, 4)])]
      clicked.state
  assertEqual "the click's turn" Nothing released.state.heldInput
  assertBool "a redraw" (KeepScreen `notElem` released.commands)
  -- The input was meant for the queue after the edit.
  failed <-
    runEvents
      0
      [QueueEditFailed (AckError (Ack AckArg 0 "delete" "Bad song index"))]
      twice.state
  assertEqual "a failure drops it" [] failed.requests
  assertEqual "nothing waits" Nothing failed.state.heldInput
  reconnected <- runEvents 0 [MpdConnected (Version 0 24 0)] twice.state
  assertEqual "a new connection drops it" Nothing reconnected.state.heldInput

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
  assertEqual "nothing held" (Just Seq.empty) answered.state.heldInput

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
  assertEqual "the cursor stays" 3 . cursor
    =<< keys ["down", "insert", "down", "down", "m"] s
  assertEqual "under the cursor" (editRequests [Request "move" ["1:2", "2"]]) . (.requests)
    =<< keys ["down", "down", "m"] s
  top <- keys ["insert", "m"] s
  assertEqual "the top stays" [] top.requests
  assertEqual "with the cursor" 0 (cursor top)

test_moveSongsTo :: Assertion
test_moveSongsTo = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual "above the cursor" (editRequests [Request "move" ["0:1", "3"]]) . (.requests)
    =<< keys ["insert", "end", "M"] s
  assertEqual "among the selected songs" (Just "The cursor is among the selected songs")
    . message
    =<< keys ["insert", "down", "down", "insert", "up", "M"] s
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
  elsewhere <- keys ["space", "space", "8", "e", "s"] s
  assertEqual
    "the whole queue from another screen"
    (Just "Shuffle 5 songs in the queue?")
    ((.question) <$> elsewhere.state.prompt)

----------------------------------------
-- Model

data Operation
  = MoveUp
  | MoveDown
  | MoveBefore Int
  | MoveToStart
  | MoveAfter Int
  | Delete
  deriving stock (Show)

data Case = Case
  { queue :: [(Int, Bool)]
  -- ^ The index of a test song, and whether it's selected.
  , operation :: Operation
  }
  deriving stock (Show)

genCase :: Gen Case
genCase = do
  n <- chooseInt (1, 2 * length testSongs)
  queue <- vectorOf n $ (,) <$> chooseInt (0, length testSongs - 1) <*> arbitrary
  operation <-
    oneof
      [ elements [MoveUp, MoveDown, MoveToStart, Delete]
      , MoveBefore <$> chooseInt (0, n)
      , MoveAfter <$> chooseInt (0, n - 1)
      ]
  -- The song that the others move after isn't one of them.
  let unselected = case operation of
        MoveAfter current -> [(i, selected && p /= current) | (p, (i, selected)) <- zip [0 ..] queue]
        _ -> queue
  pure Case {queue = unselected, operation = operation}

-- | The queue after the operation, by the plain definition of the
-- operation.
model :: Case -> [Int]
model c = map fst $ case c.operation of
  MoveUp -> toList $ foldl' up (Seq.fromList c.queue) [0 .. length c.queue - 1]
  MoveDown -> toList $ foldl' down (Seq.fromList c.queue) (reverse [0 .. length c.queue - 1])
  MoveBefore target
    | any (\p -> p >= target) selected && any (\p -> p <= target) selected -> c.queue
    | otherwise ->
        [s | (p, s@(_, False)) <- indexed, p < target]
          <> filter snd c.queue
          <> [s | (p, s@(_, False)) <- indexed, p >= target]
  MoveToStart -> filter snd c.queue <> filter (not . snd) c.queue
  MoveAfter current ->
    [s | (p, s@(_, False)) <- indexed, p <= current]
      <> filter snd c.queue
      <> [s | (p, s@(_, False)) <- indexed, p > current]
  Delete -> filter (not . snd) c.queue
  where
    indexed :: [(Int, (Int, Bool))]
    indexed = zip [0 ..] c.queue

    selected :: [Int]
    selected = [p | (p, (_, True)) <- indexed]

    -- A selected song swaps places with an unselected one above it.
    up :: Seq.Seq (Int, Bool) -> Int -> Seq.Seq (Int, Bool)
    up q p = case (Seq.lookup (p - 1) q, Seq.lookup p q) of
      (Just above@(_, False), Just this@(_, True)) -> Seq.update (p - 1) this (Seq.update p above q)
      _ -> q

    down :: Seq.Seq (Int, Bool) -> Int -> Seq.Seq (Int, Bool)
    down q p = case (Seq.lookup p q, Seq.lookup (p + 1) q) of
      (Just this@(_, True), Just below@(_, False)) -> Seq.update (p + 1) this (Seq.update p below q)
      _ -> q

prop_model :: IO TestServer -> Property
prop_model getServer = forAll genCase $ \c -> ioProperty $ do
  server <- getServer
  let settings = Settings (UnixAddress server.socketPath) Nothing (Just testTimeout)
      positions = [p | (p, (_, True)) <- zip [0 ..] c.queue]
  actual <- withConnection settings $ \conn -> do
    run conn $ clear *> traverse_ (\(i, _) -> add (file i) Nothing) c.queue
    run conn $ case c.operation of
      MoveUp -> moveUp positions
      MoveDown -> moveDown (length c.queue) positions
      MoveBefore target -> sequenceA_ (moveBefore positions target)
      MoveToStart -> moveToStart positions
      MoveAfter current -> moveAfter current positions
      Delete -> deletePositions positions
    map (.file) <$> run conn playlistInfo
  pure $ actual === map file (model c)
  where
    file :: Int -> T.Text
    file i = T.pack (testSongs !! i).path

    -- A reply from the local test server that takes this long means it
    -- hangs.
    testTimeout :: Seconds
    testTimeout = 10

-- | Few songs, so that the queue repeats some.
testSongs :: [TestSong]
testSongs = [testSong (show @Int i <> ".flac") | i <- [1 .. 3]]
