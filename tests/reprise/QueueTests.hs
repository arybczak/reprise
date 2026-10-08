module QueueTests (queueTests) where

import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Reprise.Groups
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Connection
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Mpd.TestServer
import Reprise.Screen.Queue.Edits

queueTests :: TestTree
queueTests =
  testGroup
    "Queue"
    [ testCase "runs" test_runs
    , testCase "delete from the last run" test_delete
    , testCase "move runs up and down" test_moveUpDown
    , testCase "move before a position" test_moveBefore
    , testCase "move to the start" test_moveToStart
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

----------------------------------------
-- Model

data Operation
  = MoveUp
  | MoveDown
  | MoveBefore Int
  | MoveToStart
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
    oneof [elements [MoveUp, MoveDown, MoveToStart, Delete], MoveBefore <$> chooseInt (0, n)]
  pure Case {queue = queue, operation = operation}

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
