module ConnectionTests (connectionTests, closedTests, timeoutTests) where

import Control.Concurrent
import Control.Concurrent.MVar.Strict qualified as S
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Connection
import Reprise.Mpd.Protocol.Idle
import Reprise.Mpd.Protocol.Types
import Reprise.Mpd.TestServer

connectionTests :: TestTree
connectionTests =
  withResource (startTestServer songs) stopTestServer $ \getServer ->
    dependentTestGroup "Connection (real server)" AllFinish $
      map
        (\(name, test) -> testCase name $ getServer >>= \s -> resetTestServer s >> test s)
        [ ("connect over a unix socket", test_connectUnix)
        , ("connect over TCP", test_connectTcp)
        , ("connect to nothing", test_connectToNothing)
        , ("a socket path that is too long", test_longSocketPath)
        , ("wrong password", test_wrongPassword)
        , ("command list", test_commandList)
        , ("ACK", test_ack)
        , ("ACK in a command list", test_ackInCommandList)
        , ("an argument with a line break", test_lineBreak)
        , ("a line too long for MPD", test_longLine)
        , ("song tags", test_songTags)
        , ("add and delete", test_addAndDelete)
        , ("move", test_move)
        , ("shuffle and clear", test_shuffleAndClear)
        , ("priority", test_priority)
        , ("plchanges", test_plChanges)
        , ("playback", test_playback)
        , ("seek", test_seek)
        , ("volume", test_volume)
        , ("options", test_options)
        , ("update", test_update)
        , ("directories and playlists", test_directoriesAndPlaylists)
        , ("stats", test_stats)
        , ("outputs", test_outputs)
        , ("readcomments", test_readComments)
        , ("idle", test_idle)
        , ("noidle", test_noidle)
        ]

-- | A connection that MPD closed for being unused fails differently on each
-- transport. On a unix socket the send fails, over TCP the read finds the
-- end. Either way MPD ran nothing, so the command can run again.
closedTests :: TestTree
closedTests =
  testGroup
    "Connection (closed by the server)"
    [ testCase "a closed unix socket" . withSystemTempDirectory "reprise" $ \dir -> do
        let path = dir </> "socket"
        assertClosed $ FakeServer N.AF_UNIX (N.SockAddrUnix path) (\_ -> pure $ UnixAddress path)
    , testCase "a closed TCP connection" $ assertClosed localTcp
    , testCase "a reply cut off in its first line" $ do
        -- MPD began the reply, so it ran the command, which mustn't run again.
        let cutOff sock = N.recv sock 4096 >> N.sendAll sock "key: va"
        r <- withFakeServer localTcp cutOff $ \address ->
          try @MpdError
            . withConnection
              Settings {address = address, password = Nothing, timeout = Just testTimeout}
            $ (`run` ping)
        case r of
          Left (ConnectionError (Broken _)) -> pure ()
          _ -> assertFailure $ "not broken: " <> show r
    ]
  where
    -- A server that greets and closes.
    assertClosed :: FakeServer -> Assertion
    assertClosed server = do
      closed <- newEmptyMVar
      withFakeServer server (\sock -> N.close sock >> putMVar closed ()) $ \address -> do
        conn <-
          connect Settings {address = address, password = Nothing, timeout = Just testTimeout}
        takeMVar closed
        r <- try @MpdError (run conn ping) `finally` close conn
        assertEqual "the error" (Left (ConnectionError Closed)) r

-- | The timeout is for each part of a reply, so that a long reply on a slow
-- connection gets the time that it needs while it comes.
timeoutTests :: TestTree
timeoutTests =
  testGroup
    "Connection (timeouts)"
    [ testCase "a reply that comes slowly" $ do
        r <- withFakeServer localTcp (reply 5) (`withShortTimeout` (`run` fields))
        assertEqual "the reply" (Right 5) r
    , testCase "a reply that stops" $ do
        r <- withFakeServer localTcp stall (`withShortTimeout` (`run` fields))
        assertEqual "the error" (Left (ConnectionError TimedOut)) r
    ]
  where
    -- Each part comes well within the timeout, and all of them after it.
    timeout' :: Seconds
    timeout' = 0.5

    gap :: Int
    gap = 150000

    withShortTimeout :: Address -> (Connection -> IO a) -> IO (Either MpdError a)
    withShortTimeout address =
      try
        . withConnection Settings {address = address, password = Nothing, timeout = Just timeout'}

    fields :: Command Int
    fields = command "fields" [] (Right . length)

    reply :: Int -> N.Socket -> IO ()
    reply n sock = do
      _ <- N.recv sock 4096
      replicateM_ n $ threadDelay gap >> N.sendAll sock "key: value\n"
      N.sendAll sock "OK\n"

    stall :: N.Socket -> IO ()
    stall sock = do
      _ <- N.recv sock 4096
      N.sendAll sock "key: value\n"
      threadDelay maxBound

-- | Where a fake server listens: the family and the address of its socket,
-- and the address that a client connects to, from the socket.
data FakeServer = FakeServer N.Family N.SockAddr (N.Socket -> IO Address)

localTcp :: FakeServer
localTcp =
  FakeServer
    N.AF_INET
    (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
    (fmap (TcpAddress "127.0.0.1") . N.socketPort)

-- | Run a fake server that greets as MPD does and then serves one
-- connection, while an action runs with the address to connect to.
withFakeServer :: FakeServer -> (N.Socket -> IO ()) -> (Address -> IO a) -> IO a
withFakeServer (FakeServer family listenAt addressOf) serve k =
  bracket (N.socket family N.Stream N.defaultProtocol) N.close $ \listener -> do
    N.bind listener listenAt
    N.listen listener 1
    address <- addressOf listener
    let server = do
          (sock, _) <- N.accept listener
          N.sendAll sock ("OK MPD " <> BS8.pack (showVersion minimumVersion) <> "\n")
          serve sock `finally` N.close sock
    bracket (forkIO server) killThread $ \_ -> k address
  where
    showVersion :: Version -> String
    showVersion (Version major minor patch) = L.intercalate "." $ map show [major, minor, patch]

-- | The songs play in real time, so a song that ended during a test would
-- change the current song under it, e.g. on a busy machine. Each lasts long
-- enough that none ends during a test.
songs :: [TestSong]
songs =
  [ TestSong
      { path = "a/one.flac"
      , tags =
          [ ("ARTIST", "Foo")
          , ("ARTIST", "Bar")
          , ("ALBUM", "Baz")
          , ("TITLE", "Zażółć")
          , ("TRACKNUMBER", "1")
          ]
      , seconds = 60
      }
  , (testSong "a/two.flac") {seconds = 60}
  , (testSong "b/three.flac") {seconds = 90}
  ]

test_connectUnix :: TestServer -> Assertion
test_connectUnix server = withConn server $ \conn -> do
  assertBool "version" (serverVersion conn >= minimumVersion)
  run conn ping

test_connectTcp :: TestServer -> Assertion
test_connectTcp server = do
  withConnection (tcpSettings server) (`run` ping)

test_connectToNothing :: TestServer -> Assertion
test_connectToNothing server = do
  r <-
    try @MpdError $
      connect (unixSettings server) {address = UnixAddress (server.socketPath <> "-missing")}
  case r of
    Left (ConnectionError (ConnectFailed _)) -> pure ()
    Left err -> assertFailure $ "unexpected error: " <> show err
    Right conn -> close conn >> assertFailure "connected"

-- | A path longer than a socket's address holds fails as a connection that
-- can't open, not as a crash.
test_longSocketPath :: TestServer -> Assertion
test_longSocketPath server = do
  r <-
    try @MpdError $
      connect (unixSettings server) {address = UnixAddress ("/" <> replicate 200 'a')}
  case r of
    Left (ConnectionError (ConnectFailed _)) -> pure ()
    Left err -> assertFailure $ "unexpected error: " <> show err
    Right conn -> close conn >> assertFailure "connected"

test_wrongPassword :: TestServer -> Assertion
test_wrongPassword server = do
  r <-
    try @MpdError $
      withConnection (unixSettings server) {password = Just "wrong"} (`run` ping)
  case r of
    Left (AckError ack) -> assertEqual "code" AckPassword ack.code
    _ -> assertFailure $ "unexpected result: " <> show r

test_commandList :: TestServer -> Assertion
test_commandList server = withConn server $ \conn -> do
  (s, song, files) <- run conn $ do
    (,,)
      <$> (addFiles ["a/one.flac", "a/two.flac"] *> status)
      <*> currentSong
      <*> (map (.file) <$> playlistInfo)
  assertEqual "queue length" 2 s.playlistLength
  assertEqual "no current song" Nothing song
  assertEqual "files" ["a/one.flac", "a/two.flac"] files

test_ack :: TestServer -> Assertion
test_ack server = withConn server $ \conn -> do
  r <- try @MpdError . run conn $ play (Just 99)
  case r of
    Left (AckError ack) -> do
      assertEqual "code" AckArg ack.code
      assertEqual "index" 0 ack.index
      assertEqual "command" "play" ack.command
    _ -> assertFailure $ "unexpected result: " <> show r
  run conn ping

-- | An argument with a line break isn't sent, so the replies stay in step.
test_lineBreak :: TestServer -> Assertion
test_lineBreak server = withConn server $ \conn -> do
  r <- try @MpdError . run conn $ add "a/one.flac\nclear" Nothing
  case r of
    Left (ProtocolError _) -> pure ()
    _ -> assertFailure $ "unexpected result: " <> show r
  run conn ping

-- | MPD would close the connection of a line that its input buffer can't
-- hold.
test_longLine :: TestServer -> Assertion
test_longLine server = withConn server $ \conn -> do
  r <- try @MpdError . run conn $ add (T.replicate 8192 "a") Nothing
  case r of
    Left (ProtocolError _) -> pure ()
    _ -> assertFailure $ "unexpected result: " <> show r
  run conn ping

test_ackInCommandList :: TestServer -> Assertion
test_ackInCommandList server = withConn server $ \conn -> do
  r <- try @MpdError . run conn $ setRepeat True *> play (Just 99) *> setRandom True
  case r of
    Left (AckError ack) -> do
      assertEqual "index" 1 ack.index
      assertEqual "command" "play" ack.command
    _ -> assertFailure $ "unexpected result: " <> show r
  s <- run conn status
  assertEqual "the command before the error ran" True s.repeat
  assertEqual "the command after the error didn't run" False s.random

test_songTags :: TestServer -> Assertion
test_songTags server = withConn server $ \conn -> do
  i <- run conn $ addId "a/one.flac" Nothing
  [song] <- run conn playlistInfo
  assertEqual "id" (Just i) song.songId
  assertEqual "position" (Just 0) song.position
  assertEqual "duration" (Just 60) song.duration
  assertEqual
    "tags"
    ( M.fromList
        [ (Artist, ["Foo", "Bar"])
        , (Album, ["Baz"])
        , (Title, ["Zażółć"])
        , (Track, ["1"])
        ]
    )
    song.tags

test_addAndDelete :: TestServer -> Assertion
test_addAndDelete server = withConn server $ \conn -> do
  run conn $ addFiles ["a/one.flac", "a/two.flac"]
  i <- run conn $ addId "b/three.flac" (Just $ At 1)
  assertQueue conn "added" ["a/one.flac", "b/three.flac", "a/two.flac"]
  run conn $ play (Just 0)
  run conn $ add "a/two.flac" (Just $ AfterCurrent 0)
  assertQueue
    conn
    "after the current song"
    ["a/one.flac", "a/two.flac", "b/three.flac", "a/two.flac"]
  run conn $ deleteId i
  assertQueue conn "deleted by id" ["a/one.flac", "a/two.flac", "a/two.flac"]
  run conn $ delete (Range 1 Nothing)
  assertQueue conn "deleted by range" ["a/one.flac"]

test_move :: TestServer -> Assertion
test_move server = withConn server $ \conn -> do
  run conn addAll
  run conn $ move (onePosition 0) (At 2)
  assertQueue conn "moved by position" ["a/two.flac", "b/three.flac", "a/one.flac"]
  [_, _, song] <- run conn playlistInfo
  Just i <- pure song.songId
  run conn $ moveId i (At 0)
  assertQueue conn "moved by id" ["a/one.flac", "a/two.flac", "b/three.flac"]

test_shuffleAndClear :: TestServer -> Assertion
test_shuffleAndClear server = withConn server $ \conn -> do
  run conn addAll
  run conn $ shuffle Nothing
  s <- run conn status
  assertEqual "length after shuffle" 3 s.playlistLength
  run conn clear
  assertQueue conn "cleared" []

test_priority :: TestServer -> Assertion
test_priority server = withConn server $ \conn -> do
  run conn addAll
  [_, s2, _] <- run conn playlistInfo
  Just i <- pure s2.songId
  run conn $ prio 5 [onePosition 0] *> prioId 7 [i]
  priorities <- map (.priority) <$> run conn playlistInfo
  assertEqual "priorities" [5, 7, 0] priorities
  -- More songs than MPD reads arguments of one command.
  run conn $ prio 3 (replicate 200 (onePosition 0)) *> prioId 4 (replicate 200 i)
  more <- map (.priority) <$> run conn playlistInfo
  assertEqual "the priorities of many songs" [3, 4, 0] more

test_plChanges :: TestServer -> Assertion
test_plChanges server = withConn server $ \conn -> do
  run conn addAll
  v <- (.playlistVersion) <$> run conn status
  run conn $ move (onePosition 2) (At 1)
  changed <- run conn $ plChanges v
  assertEqual "changed positions" [Just 1, Just 2] (map (.position) changed)
  assertEqual "changed files" ["b/three.flac", "a/two.flac"] (map (.file) changed)

test_playback :: TestServer -> Assertion
test_playback server = withConn server $ \conn -> do
  run conn addAll
  run conn $ play Nothing
  s1 <- run conn status
  assertEqual "playing" Playing s1.state
  assertEqual "first song" (Just 0) s1.currentPosition
  run conn $ pause True
  assertState conn "paused" Paused
  run conn $ pause False
  assertState conn "resumed" Playing
  run conn next
  s2 <- run conn status
  assertEqual "next" (Just 1) s2.currentPosition
  run conn previous
  s3 <- run conn status
  assertEqual "previous" (Just 0) s3.currentPosition
  nextId <- maybe (assertFailure "no next song") pure s3.nextId
  run conn $ playId nextId
  s4 <- run conn status
  assertEqual "played by id" (Just 1) s4.currentPosition
  song <- run conn currentSong
  assertEqual "current song" (Just "a/two.flac") ((.file) <$> song)
  run conn stop
  assertState conn "stopped" Stopped

test_seek :: TestServer -> Assertion
test_seek server = withConn server $ \conn -> do
  run conn $ add "b/three.flac" Nothing
  run conn $ play Nothing *> pause True *> seekCur (SeekTo 2)
  s1 <- run conn status
  assertEqual "absolute" (Just 2) s1.elapsed
  run conn $ seekCur (SeekBackward 1.5)
  s2 <- run conn status
  assertEqual "relative" (Just 0.5) s2.elapsed

test_volume :: TestServer -> Assertion
test_volume server = withConn server $ \conn -> do
  -- The software mixer reports the volume only while an output is open.
  run conn $ add "b/three.flac" Nothing *> play Nothing *> pause True
  run conn $ setVolume 40
  assertVolume conn "set" (Just 40)
  run conn $ changeVolume 5
  assertVolume conn "up" (Just 45)
  run conn $ changeVolume (-10)
  assertVolume conn "down" (Just 35)
  where
    assertVolume :: Connection -> String -> Maybe Int -> Assertion
    assertVolume conn msg v = run conn status >>= \s -> assertEqual msg v s.volume

test_options :: TestServer -> Assertion
test_options server = withConn server $ \conn -> do
  run conn $
    setRepeat True
      *> setRandom True
      *> setSingle SingleOneshot
      *> setConsume ConsumeOn
      *> setCrossfade 3
      *> setReplayGainMode ReplayGainAlbum
  s <- run conn status
  assertEqual "repeat" True s.repeat
  assertEqual "random" True s.random
  assertEqual "single" SingleOneshot s.single
  assertEqual "consume" ConsumeOn s.consume
  assertEqual "crossfade" 3 s.crossfade
  mode <- run conn replayGainStatus
  assertEqual "replay gain" ReplayGainAlbum mode

test_update :: TestServer -> Assertion
test_update server = withConn server $ \conn -> do
  job <- run conn $ update (Just "a")
  assertBool "job id" (job > 0)

test_directoriesAndPlaylists :: TestServer -> Assertion
test_directoriesAndPlaylists server = withConn server $ \conn -> do
  run conn $
    addFiles ["a/one.flac", "b/three.flac"] *> command "save" ["p"] (const (Right ()))
  -- MPD lists a directory in no particular order.
  root <- run conn $ lsInfo ""
  assertEqual
    "the root"
    ["directory a", "directory b", "playlist p"]
    (L.sort $ map describe root)
  a <- run conn $ lsInfo "a"
  assertEqual "a directory" ["file a/one.flac", "file a/two.flac"] (L.sort $ map describe a)
  playlist <- run conn $ listPlaylistInfo "p"
  assertEqual "a playlist" ["a/one.flac", "b/three.flac"] (map (.file) playlist)
  run conn $ clear *> load "p" (Just $ Range 1 Nothing) Nothing
  assertQueue conn "a part of a playlist" ["b/three.flac"]
  run conn $ load "p" (Just $ Range 0 (Just 1)) (Just $ At 0)
  assertQueue conn "at a position" ["a/one.flac", "b/three.flac"]
  -- The other tests list no playlists, but the server keeps it.
  _ <- rawCommand server "rm p"
  pure ()
  where
    describe :: Entry -> T.Text
    describe = \case
      DirectoryEntry d -> "directory " <> d.path
      SongEntry s -> "file " <> s.file
      PlaylistEntry p -> "playlist " <> p.path

test_stats :: TestServer -> Assertion
test_stats server = withConn server $ \conn -> do
  s <- run conn stats
  assertEqual "songs" 3 s.songs
  assertEqual "albums" 1 s.albums
  assertEqual "artists" 2 s.artists
  assertEqual "length of all songs" 210 s.dbPlaytime

test_outputs :: TestServer -> Assertion
test_outputs server = withConn server $ \conn -> do
  [o] <- run conn outputs
  assertEqual "name" "null" o.name
  assertEqual "enabled" True o.enabled
  run conn $ disableOutput o.outputId
  [o'] <- run conn outputs
  assertEqual "disabled" False o'.enabled
  run conn $ enableOutput o.outputId

test_idle :: TestServer -> Assertion
test_idle server = withConn server $ \idleConn -> do
  result <- S.newEmptyMVar
  _ <- forkIO $ idle idleConn [OptionsSubsystem] >>= S.putMVar result
  withConn server $ \conn -> run conn $ setRepeat True
  r <- S.takeMVar result
  assertEqual "changed" [OptionsSubsystem] r

test_noidle :: TestServer -> Assertion
test_noidle server = withConn server $ \conn -> do
  result <- S.newEmptyMVar
  _ <- forkIO $ idle conn [] >>= S.putMVar result
  -- MPD ignores a noidle that arrives before the idle, so retry until the
  -- idle returns.
  let loop = do
        noidle conn
        S.tryTakeMVar result >>= \case
          Just r -> pure r
          Nothing -> yield >> loop
  r <- loop
  assertEqual "nothing changed" [] r
  run conn ping

----------------------------------------
-- Helpers

unixSettings :: TestServer -> Settings
unixSettings server =
  Settings
    { address = UnixAddress server.socketPath
    , password = Nothing
    , timeout = Just testTimeout
    }

tcpSettings :: TestServer -> Settings
tcpSettings server = (unixSettings server) {address = TcpAddress "127.0.0.1" server.port}

-- | A reply from the local test server that takes this long means it hangs.
testTimeout :: Seconds
testTimeout = 10

-- | Add every test song, in the order of 'songs'. Adding a directory would
-- follow the order of the file system.
addAll :: Command ()
addAll = addFiles [T.pack s.path | s <- songs]

addFiles :: [T.Text] -> Command ()
addFiles = traverse_ (`add` Nothing)

withConn :: TestServer -> (Connection -> IO a) -> IO a
withConn server = withConnection (unixSettings server)

assertQueue :: Connection -> String -> [T.Text] -> Assertion
assertQueue conn msg files = do
  queue <- run conn playlistInfo
  assertEqual msg files (map (.file) queue)

assertState :: Connection -> String -> PlayerState -> Assertion
assertState conn msg st = run conn status >>= \s -> assertEqual msg st s.state

-- | The comments as the file has them, also the second value of a name.
test_readComments :: TestServer -> Assertion
test_readComments server = withConn server $ \conn -> do
  comments <- run conn $ readComments "a/one.flac"
  assertEqual "the artists" ["Foo", "Bar"] [v | (k, v) <- comments, k == "ARTIST"]
  assertEqual "the title" (Just "Zażółć") (lookup "TITLE" comments)
