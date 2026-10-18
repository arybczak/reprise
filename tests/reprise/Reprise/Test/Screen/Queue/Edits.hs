module Reprise.Test.Screen.Queue.Edits (queueEditsTests) where

import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Reprise.Groups
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Connection
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Mpd.TestServer
import Reprise.Screen.Queue.Edits

queueEditsTests :: TestTree
queueEditsTests =
  testGroup
    "edits"
    [ testCase "runs" test_runs
    , testCase "delete from the last run" test_delete
    , testCase "move runs up and down" test_moveUpDown
    , testCase "move down to a position" test_moveDownTo
    , testCase "move to the start" test_moveToStart
    , testCase "move after a song" test_moveAfter
    , withResource (startTestServer testSongs) stopTestServer $ \getServer ->
        testProperty "MPD gives the queue of the model" (prop_model getServer)
    ]

requests :: [QueueEdit] -> [Request]
requests = commandRequests . editCommands

test_runs :: Assertion
test_runs = do
  assertEqual "empty" [] (runs [])
  assertEqual "runs" [(0, 1), (3, 3), (5, 7)] (runs [0, 1, 3, 5, 6, 7])

test_delete :: Assertion
test_delete =
  assertEqual
    "requests"
    [Request "delete" ["5:6"], Request "delete" ["1:3"]]
    (requests $ deletePositions [1, 2, 5])

test_moveUpDown :: Assertion
test_moveUpDown = do
  assertEqual
    "up, with a run at the top that stays"
    [Request "move" ["2:3", "3"]]
    (requests $ moveUp [0, 3])
  assertEqual
    "down, with a run at the bottom that stays"
    [Request "move" ["2:3", "1"]]
    (requests $ moveDown 5 [1, 4])

test_moveDownTo :: Assertion
test_moveDownTo = do
  assertEqual
    "from the last song"
    [Request "move" ["3:4", "4"], Request "move" ["1:2", "3"]]
    (requests $ moveDownTo 5 [1, 3])
  assertEqual
    "a run as one range"
    [Request "move" ["4:5", "5"], Request "move" ["1:3", "3"]]
    (requests $ moveDownTo 6 [1, 2, 4])
  assertEqual "in place already" [] (requests $ moveDownTo 5 [3, 4])

-- | A song at the start already stays, and the others join it.
test_moveToStart :: Assertion
test_moveToStart =
  assertEqual
    "the others join"
    [Request "move" ["3:4", "1"]]
    (requests $ moveToStart [0, 3])

-- | Songs on both sides of the current song at 2: the one below moves up
-- first, and then the one above moves down.
test_moveAfter :: Assertion
test_moveAfter =
  assertEqual
    "below, then above"
    [Request "move" ["4:5", "3"], Request "move" ["0:1", "2"]]
    (requests $ moveAfter 2 [0, 4])

----------------------------------------
-- Model

data Operation
  = MoveUp
  | MoveDown
  | MoveDownTo Int
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
      , MoveDownTo <$> chooseInt (0, n)
      , MoveAfter <$> chooseInt (0, n - 1)
      ]
  -- The songs move down to a position after them, and the song that the
  -- others move after isn't one of them.
  let unselected = case operation of
        MoveDownTo target -> [(i, selected && p < target) | (p, (i, selected)) <- zip [0 ..] queue]
        MoveAfter current -> [(i, selected && p /= current) | (p, (i, selected)) <- zip [0 ..] queue]
        _ -> queue
  pure Case {queue = unselected, operation = operation}

-- | The queue after the operation, by the plain definition of the
-- operation.
model :: Case -> [Int]
model c = map fst $ case c.operation of
  MoveUp -> toList $ foldl' up (Seq.fromList c.queue) [0 .. length c.queue - 1]
  MoveDown -> toList $ foldl' down (Seq.fromList c.queue) (reverse [0 .. length c.queue - 1])
  MoveDownTo target ->
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
    run conn . editCommands $ case c.operation of
      MoveUp -> moveUp positions
      MoveDown -> moveDown (length c.queue) positions
      MoveDownTo target -> moveDownTo target positions
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
