-- | A real MPD server for tests.
--
-- The module talks to MPD with a few raw protocol lines, so that it doesn't
-- depend on the library it helps to test.
module Reprise.Mpd.TestServer
  ( -- * Songs
    TestSong (..)
  , testSong

    -- * Server
  , TestServer (..)
  , startTestServer
  , stopTestServer
  , resetTestServer
  , rawCommand
  ) where

import Control.Concurrent
import Control.Concurrent.MVar.Strict qualified as S
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

----------------------------------------
-- Songs

-- | A silent FLAC file in the music directory.
data TestSong = TestSong
  { path :: FilePath
  -- ^ Relative to the music directory.
  , tags :: [(T.Text, T.Text)]
  -- ^ Vorbis comments, e.g. @("ARTIST", "x")@. A name may repeat.
  , seconds :: Int
  }
  deriving stock (Eq, Show)

-- | A song without tags that lasts a second.
testSong :: FilePath -> TestSong
testSong path = TestSong {path = path, tags = [], seconds = 1}

----------------------------------------
-- Server

data TestServer = TestServer
  { directory :: FilePath
  , socketPath :: FilePath
  , port :: N.PortNumber
  -- ^ The TCP port on 127.0.0.1.
  , process :: ProcessHandle
  }

-- | Start @mpd@ in a new temporary directory, and wait until it listens and
-- its database has the songs.
startTestServer :: [TestSong] -> IO TestServer
startTestServer songs = do
  tmp <- getCanonicalTemporaryDirectory
  directory <- createTempDirectory tmp "mpd-test"
  let musicDir = directory </> "music"
      playlistDir = directory </> "playlists"
      logFile = directory </> "log"
      outputFile = directory </> "output"
      configFile = directory </> "mpd.conf"
  createDirectoryIfMissing True musicDir
  createDirectoryIfMissing True playlistDir
  forM_ songs $ writeSong musicDir
  port <- freePort
  let socketPath = directory </> "socket"
  writeFile configFile $
    unlines
      [ "music_directory " <> show musicDir
      , "playlist_directory " <> show playlistDir
      , "db_file " <> show (directory </> "db")
      , "log_file " <> show logFile
      , "bind_to_address " <> show socketPath
      , "bind_to_address \"127.0.0.1\""
      , "port " <> show (show port)
      , "auto_update \"no\""
      , "audio_output {"
      , "  type \"null\""
      , "  name \"null\""
      , "  mixer_type \"software\""
      , "}"
      ]
  -- mpd prints its first messages before it opens the log file.
  (_, _, _, process) <- withFile outputFile WriteMode $ \output ->
    createProcess
      (proc "mpd" ["--no-daemon", configFile])
        { std_out = UseHandle output
        , std_err = UseHandle output
        }
  let server =
        TestServer
          { directory = directory
          , socketPath = socketPath
          , port = port
          , process = process
          }
  flip onException (stopTestServer server) $ do
    waitUntilListening server [outputFile, logFile]
    waitForDatabase server
  pure server

-- | Stop @mpd@ and remove its directory.
stopTestServer :: TestServer -> IO ()
stopTestServer server = do
  terminateProcess server.process
  _ <- waitForProcess server.process
  removeDirectoryRecursive server.directory

-- | Bring the server back to its state after the start: an empty queue, no
-- playback and the default options.
resetTestServer :: TestServer -> IO ()
resetTestServer server =
  mapM_
    (rawCommand server)
    [ "stop"
    , "clear"
    , "repeat 0"
    , "random 0"
    , "single 0"
    , "consume 0"
    , "crossfade 0"
    , "replay_gain_mode off"
    , "setvol 100"
    ]

-- | Send one command line on a new connection, and return the lines of the
-- reply before @OK@. An @ACK@ throws an exception.
rawCommand :: TestServer -> BS.ByteString -> IO [BS.ByteString]
rawCommand server line = withRawConnection server $ \conn -> rawExchange conn line

----------------------------------------
-- Helpers

data RawConnection = RawConnection N.Socket (S.MVar BS.ByteString)

withRawConnection :: TestServer -> (RawConnection -> IO a) -> IO a
withRawConnection server action =
  bracket (N.socket N.AF_UNIX N.Stream N.defaultProtocol) N.close $ \sock -> do
    N.connect sock (N.SockAddrUnix server.socketPath)
    conn <- RawConnection sock <$> S.newMVar BS.empty
    greeting <- readLine conn
    unless ("OK MPD " `BS.isPrefixOf` greeting)
      $ throwIO . userError
      $ "test server: unexpected greeting " <> show greeting
    action conn

rawExchange :: RawConnection -> BS.ByteString -> IO [BS.ByteString]
rawExchange conn@(RawConnection sock _) line = do
  N.sendAll sock (line <> "\n")
  let loop acc = do
        l <- readLine conn
        if
          | l == "OK" -> pure (reverse acc)
          | "ACK " `BS.isPrefixOf` l -> throwIO . userError $ "test server: " <> BS8.unpack l
          | otherwise -> loop (l : acc)
  loop []

readLine :: RawConnection -> IO BS.ByteString
readLine (RawConnection sock buffer) = S.modifyMVar buffer go
  where
    go :: BS.ByteString -> IO (BS.ByteString, BS.ByteString)
    go buf = case BS8.elemIndex '\n' buf of
      Just i -> pure (BS.drop (i + 1) buf, BS.take i buf)
      Nothing -> do
        chunk <- N.recv sock chunkSize
        when (BS.null chunk) $ throwIO (userError "test server: the connection closed")
        go (buf <> chunk)

    -- The replies of the test server are short.
    chunkSize :: Int
    chunkSize = 4096

writeSong :: FilePath -> TestSong -> IO ()
writeSong musicDir song = do
  let file = musicDir </> song.path
  createDirectoryIfMissing True (takeDirectory file)
  let args =
        [ "--silent"
        , "--force-raw-format"
        , "--endian=little"
        , "--sign=signed"
        , "--channels=1"
        , "--bps=" <> show bitsPerSample
        , "--sample-rate=" <> show sampleRate
        , "--output-name=" <> file
        ]
          <> concat [["-T", T.unpack (name <> "=" <> value)] | (name, value) <- song.tags]
          <> ["-"]
      bytes = BS.replicate (song.seconds * sampleRate * bitsPerSample `div` 8) 0
  (Just input, _, _, process) <- createProcess (proc "flac" args) {std_in = CreatePipe}
  hSetBinaryMode input True
  BS.hPut input bytes
  hClose input
  waitForProcess process >>= \case
    ExitSuccess -> pure ()
    ExitFailure code -> throwIO . userError $ "flac failed with " <> show code <> " for " <> file
  where
    -- Low values keep the files small.
    bitsPerSample :: Int
    bitsPerSample = 16

    sampleRate :: Int
    sampleRate = 8000

-- | A TCP port that is free now. Another process could take it before @mpd@
-- starts, which is unlikely enough for tests.
freePort :: IO N.PortNumber
freePort = bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \sock -> do
  N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
  N.socketPort sock

waitUntilListening :: TestServer -> [FilePath] -> IO ()
waitUntilListening server logFiles = loop
  where
    loop :: IO ()
    loop =
      getProcessExitCode server.process >>= \case
        Just code -> do
          logs <- forM logFiles $ \f -> readFile f `catch` \(_ :: IOException) -> pure ""
          throwIO . userError $ "mpd exited with " <> show code <> ":\n" <> concat logs
        Nothing ->
          try @IOException (withRawConnection server $ \_ -> pure ()) >>= \case
            Right () -> pure ()
            Left _ -> threadDelay pollInterval >> loop

    -- mpd takes about 40 ms to start listening (measured on a desktop), so
    -- polling every 5 ms delays the tests by little.
    pollInterval :: Int
    pollInterval = 5000

-- | Update the database and wait until the update ends. MPD remembers the
-- changes since the last @idle@, so an update that ends between @status@ and
-- @idle@ isn't missed.
waitForDatabase :: TestServer -> IO ()
waitForDatabase server = withRawConnection server $ \conn -> do
  _ <- rawExchange conn "update"
  let loop = do
        statusLines <- rawExchange conn "status"
        when (any ("updating_db: " `BS.isPrefixOf`) statusLines) $ do
          _ <- rawExchange conn "idle update"
          loop
  loop
