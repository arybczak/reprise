module Reprise.Test.Mpd.Mirror (mirrorTests) where

import Control.Exception
import Data.Foldable
import Data.Set qualified as S
import Data.Text qualified as T
import Optics.Core
import Test.QuickCheck hiding (shuffle)
import Test.QuickCheck.Monadic qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (shuffle)

import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Connection
import Reprise.Mpd.Protocol.Types
import Reprise.Mpd.TestServer
import Reprise.Test.Helpers

mirrorTests :: TestTree
mirrorTests =
  testGroup
    "mirror"
    [ testCase "a change replaces a song" test_replace
    , testCase "changes past the end append" test_append
    , testCase "the queue is cut to its length" test_truncate
    , testCase "a gap is an error" test_gap
    , testCase "elapsed time" test_elapsed
    , testCase "lengths follow the queue and the current song" test_lengths
    , testCase "the mirror makes the edits only without other changes" test_edits
    , withResource (startTestServer testSongs) stopTestServer $ \getServer ->
        testProperty "the mirror follows a real queue" (prop_sync getServer)
    ]

test_replace :: Assertion
test_replace = do
  let m =
        setQueue
          0
          (statusOf Stopped Nothing 3)
          [queued 0 "a", queued 1 "b", queued 2 "c"]
          emptyMirror
  r <- expectRight $ applyQueueChanges 0 (statusOf Stopped Nothing 3) [queued 1 "x"] m
  assertEqual "files" ["a", "x", "c"] (files r)

test_append :: Assertion
test_append = do
  let m = setQueue 0 (statusOf Stopped Nothing 1) [queued 0 "a"] emptyMirror
  r <-
    expectRight $
      applyQueueChanges 0 (statusOf Stopped Nothing 3) [queued 2 "c", queued 1 "b"] m
  assertEqual "files" ["a", "b", "c"] (files r)

test_truncate :: Assertion
test_truncate = do
  let m =
        setQueue
          0
          (statusOf Stopped Nothing 3)
          [queued 0 "a", queued 1 "b", queued 2 "c"]
          emptyMirror
  r <- expectRight $ applyQueueChanges 0 (statusOf Stopped Nothing 2) [queued 1 "c"] m
  assertEqual "files" ["a", "c"] (files r)
  assertEqual "the queued songs" (S.fromList [("a", Nothing), ("c", Nothing)]) r.queued

test_gap :: Assertion
test_gap = do
  let m = setQueue 0 (statusOf Stopped Nothing 1) [queued 0 "a"] emptyMirror
  assertBool "error" . either (const True) (const False) $
    applyQueueChanges 0 (statusOf Stopped Nothing 3) [queued 2 "c"] m

test_elapsed :: Assertion
test_elapsed = do
  let playing = setStatus 100 (statusOf Playing (Just 0) 1) emptyMirror
      paused = setStatus 100 (statusOf Paused (Just 0) 1) emptyMirror
  assertEqual "interpolated" (Just 12.5) (elapsedAt 102.5 playing)
  assertEqual "not past the end" (Just 60) (elapsedAt 1000 playing)
  assertEqual "paused" (Just 10) (elapsedAt 102.5 paused)

test_lengths :: Assertion
test_lengths = do
  -- Songs of 60 s, and one without a length.
  let songs = [queued 0 "a", queued 1 "b", queued 2 "c" & #duration .~ Nothing, queued 3 "d"]
      m = setQueue 0 (statusOf Stopped Nothing 4) songs emptyMirror
  assertEqual "total" 180 m.totalLength
  assertEqual "from the current song without one" 180 m.lengthFromCurrent
  let playing = setStatus 0 (statusOf Playing (Just 1) 4) m
  assertEqual "from the current song" 120 playing.lengthFromCurrent
  r <- expectRight $ applyQueueChanges 0 (statusOf Playing (Just 1) 2) [] playing
  assertEqual "total after a cut" 120 r.totalLength
  assertEqual "from the current song after a cut" 60 r.lengthFromCurrent

test_edits :: Assertion
test_edits = do
  let m =
        setQueue
          0
          (statusOf Stopped Nothing 3)
          [queued 0 "a", queued 1 "b", queued 2 "c"]
          emptyMirror
      versioned v n = statusOf Stopped Nothing n & #playlistVersion .~ PlaylistVersion v
      edits =
        [ MoveRange (onePosition (SongPos 0)) (SongPos 2)
        , DeleteRange (onePosition (SongPos 0))
        ]
  assertEqual
    "edited"
    (Just ["c", "a"])
    (files <$> applyQueueEdits 0 (versioned 3 2) edits m)
  assertEqual "another change" Nothing (files <$> applyQueueEdits 0 (versioned 4 2) edits m)
  assertEqual
    "a move in place changes nothing"
    (Just ["a", "b", "c"])
    ( files
        <$> applyQueueEdits 0 (versioned 1 3) [MoveRange (onePosition (SongPos 1)) (SongPos 1)] m
    )
  assertEqual
    "a shuffle"
    Nothing
    (files <$> applyQueueEdits 0 (versioned 2 3) [ShuffleRange Nothing] m)

-- | Random queue operations on a real server, in batches. A batch of edits
-- runs as reprise edits the queue: in one command list with the status,
-- after which the mirror makes the edits itself unless they shuffle. After
-- any other batch, the mirror fetches the changes since its version as
-- reprise does. Either way, it must equal the server's queue.
prop_sync :: IO TestServer -> Property
prop_sync getServer = forAllShrink (listOf genBatch) shrinkList' $ \batches ->
  QC.monadicIO $ do
    server <- QC.run getServer
    QC.run $ resetTestServer server
    r <- QC.run . try @MpdError . withConnection (settingsOf server) $ \conn -> do
      let -- An operation that doesn't fit the queue, e.g. a delete past its
          -- end, fails, which is part of the test.
          apply :: Operation -> IO ()
          apply op =
            run conn (operation op) `catch` \case
              AckError _ -> pure ()
              err -> throwIO err

          fetched :: Mirror -> IO Mirror
          fetched m = do
            (st, changes) <-
              run conn $
                (,)
                  <$> Reprise.Mpd.Protocol.Command.status
                  <*> maybe playlistInfo plChanges m.queueVersion
            either (fail . T.unpack) pure $ applyQueueChanges 0 st changes m

          -- A failed edit ends its command list, after the edits before it.
          edited :: Mirror -> [QueueEdit] -> IO (Either String Mirror)
          edited m edits =
            try (run conn (editCommands edits *> Reprise.Mpd.Protocol.Command.status)) >>= \case
              Left (AckError _) -> Right <$> fetched m
              Left err -> throwIO err
              Right st -> case applyQueueEdits 0 st edits m of
                Just m' -> pure (Right m')
                Nothing
                  | any isShuffle edits -> Right <$> fetched m
                  | otherwise -> pure (Left "the mirror didn't make the edits")

          loop :: Mirror -> [[Operation]] -> IO (Either String ())
          loop m = \case
            [] -> pure (Right ())
            batch : rest -> do
              synced <- case traverse editOf batch of
                Just edits -> edited m edits
                Nothing -> Right <$> (mapM_ apply batch >> fetched m)
              actual <- map unplaced <$> run conn playlistInfo
              let actualLength = sum [d | Just d <- map (.duration) actual]
              case synced of
                Left err -> pure (Left err)
                Right m'
                  | toList m'.queue /= actual ->
                      pure . Left $
                        "mirror: "
                          <> show (map (.file) (toList m'.queue))
                          <> "\nserver: "
                          <> show (map (.file) actual)
                  | m'.totalLength /= actualLength ->
                      pure . Left $
                        "total length: " <> show m'.totalLength <> ", server: " <> show actualLength
                  | otherwise -> loop m' rest
      (st, q) <- run conn $ (,) <$> Reprise.Mpd.Protocol.Command.status <*> playlistInfo
      loop (setQueue 0 st q emptyMirror) batches
    case r of
      Right (Right ()) -> pure ()
      Right (Left err) -> do
        QC.monitor (counterexample err)
        QC.assert False
      Left err -> do
        QC.monitor (counterexample (show err))
        QC.assert False
  where
    shrinkList' :: [[Operation]] -> [[[Operation]]]
    shrinkList' = shrinkList (shrinkList (const []))

    editOf :: Operation -> Maybe QueueEdit
    editOf = \case
      Edit e -> Just e
      _ -> Nothing

    isShuffle :: QueueEdit -> Bool
    isShuffle = \case
      ShuffleRange _ -> True
      _ -> False

    unplaced :: Song -> Song
    unplaced = #position .~ Nothing

----------------------------------------
-- Queue operations

data Operation
  = AddSong Int (Maybe Int)
  | Edit QueueEdit
  | Priority Int Int
  deriving stock (Show)

genBatch :: Gen [Operation]
genBatch = do
  n <- chooseInt (1, 4)
  vectorOf n genOperation
  where
    genOperation :: Gen Operation
    genOperation =
      frequency
        [
          ( 5
          , AddSong
              <$> chooseInt (0, length testSongs - 1)
              <*> oneof [pure Nothing, Just <$> chooseInt (0, 3)]
          )
        , (2, Edit . DeleteRange <$> genRange)
        , (2, Edit <$> (MoveRange <$> genRange <*> (SongPos <$> chooseInt (0, 4))))
        , (1, pure (Edit (ShuffleRange Nothing)))
        , (1, pure (Edit ClearQueue))
        , (1, Priority <$> chooseInt (0, 255) <*> chooseInt (0, 4))
        ]

    genRange :: Gen Range
    genRange = do
      start <- chooseInt (0, 4)
      len <- chooseInt (1, 2)
      pure $ Range (SongPos start) (Just (SongPos (start + len)))

operation :: Operation -> Command ()
operation = \case
  AddSong i pos -> add (T.pack (testSongs !! i).path) (At . SongPos <$> pos)
  Edit e -> editCommands [e]
  Priority p pos -> prio p [onePosition (SongPos pos)]

----------------------------------------
-- Helpers

testSongs :: [TestSong]
testSongs = [testSong ("s" <> show @Int i <> ".flac") | i <- [0 .. 4]]

settingsOf :: TestServer -> Settings
settingsOf server = Settings (UnixAddress server.socketPath) Nothing (Just testTimeout)
  where
    -- A reply from the local test server that takes this long means it
    -- hangs.
    testTimeout :: Seconds
    testTimeout = 10

queued :: Int -> T.Text -> Song
queued pos file = song pos [] 60 & #file .~ file

files :: Mirror -> [T.Text]
files m = map (.file) (toList m.queue)

expectRight :: Either T.Text a -> IO a
expectRight = either (assertFailure . T.unpack) pure
