module MirrorTests (mirrorTests) where

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
import Utils

mirrorTests :: TestTree
mirrorTests =
  testGroup
    "Mirror"
    [ testCase "a change replaces a song" test_replace
    , testCase "changes past the end append" test_append
    , testCase "the queue is cut to its length" test_truncate
    , testCase "a gap is an error" test_gap
    , testCase "elapsed time" test_elapsed
    , testCase "lengths follow the queue and the current song" test_lengths
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

-- | Random queue operations on a real server, in batches. After each batch,
-- the mirror fetches the changes since its version as reprise does, and must
-- equal the server's queue.
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

          loop :: Mirror -> [[Operation]] -> IO (Either String ())
          loop m = \case
            [] -> pure (Right ())
            batch : rest -> do
              mapM_ apply batch
              (st, changes) <-
                run conn $
                  (,)
                    <$> Reprise.Mpd.Protocol.Command.status
                    <*> maybe playlistInfo plChanges m.queueVersion
              m' <- either (fail . T.unpack) pure $ applyQueueChanges 0 st changes m
              actual <- run conn playlistInfo
              let actualLength = sum [d | Just d <- map (.duration) actual]
              if
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

----------------------------------------
-- Queue operations

data Operation
  = AddSong Int (Maybe Int)
  | DeleteRange Int Int
  | MoveRange Int Int Int
  | ShuffleAll
  | Clear
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
        , (2, DeleteRange <$> chooseInt (0, 4) <*> chooseInt (1, 2))
        , (2, MoveRange <$> chooseInt (0, 4) <*> chooseInt (1, 2) <*> chooseInt (0, 4))
        , (1, pure ShuffleAll)
        , (1, pure Clear)
        , (1, Priority <$> chooseInt (0, 255) <*> chooseInt (0, 4))
        ]

operation :: Operation -> Command ()
operation = \case
  AddSong i pos -> add (T.pack (testSongs !! i).path) (At . SongPos <$> pos)
  DeleteRange start len -> delete (Range (SongPos start) (Just (SongPos (start + len))))
  MoveRange start len target ->
    move (Range (SongPos start) (Just (SongPos (start + len)))) (At (SongPos target))
  ShuffleAll -> shuffle Nothing
  Clear -> clear
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
