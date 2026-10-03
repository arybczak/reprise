{-# OPTIONS_HADDOCK not-home #-}

-- | The internals of a connection, shared by "MPD.Connection" and
-- "MPD.Idle".
module MPD.Internal.Connection
  ( -- * Connection
    Connection (..)
  , Address (..)
  , Settings (..)
  , connect
  , close
  , minimumVersion

    -- * Round trips
  , exchange
  , exchangeCommand
  , withTimeout
  , sendRaw
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N
import Network.Socket.ByteString.Lazy qualified as NL
import System.Timeout qualified as T

import MPD.Command
import MPD.Protocol.Request
import MPD.Protocol.Response
import MPD.Types

-- | A connection to MPD. It is not safe to use from more than one thread at a
-- time, except for 'MPD.Idle.noidle'.
--
-- @since 0.1.0.0
data Connection = Connection
  { socket :: N.Socket
  , buffer :: IORef ByteString
  -- ^ Bytes that arrived after the last line that was read.
  , chunkSize :: Int
  , timeout :: Maybe Seconds
  , version :: Version
  }

-- | Where MPD listens.
--
-- @since 0.1.0.0
data Address
  = TcpAddress N.HostName N.PortNumber
  | UnixAddress FilePath
  deriving stock (Eq, Show)

-- | @since 0.1.0.0
data Settings = Settings
  { address :: Address
  , password :: Maybe Text
  , timeout :: Maybe Seconds
  -- ^ How long to wait for a connection or a reply. 'Nothing' waits
  -- forever. @idle@ always waits forever.
  }
  deriving stock (Eq, Show)

-- | Open a connection, check the version of MPD and send the password.
--
-- @since 0.1.0.0
connect :: Settings -> IO (Either MpdError Connection)
connect settings = withTimeout settings.timeout . handleIO connectFailed $ do
  bracketOnError open N.close $ \sock -> do
    buffer <- newIORef BS.empty
    -- A receive never returns more than the socket's buffer holds.
    chunkSize <- N.getSocketOption sock N.RecvBuffer
    greeting <- readLine sock chunkSize buffer
    case parseGreeting =<< greeting of
      Nothing -> do
        N.close sock
        pure . Left . ConnectionError . ConnectFailed $
          "unexpected greeting: " <> maybe "" decode greeting
      Just version
        | version < minimumVersion -> do
            N.close sock
            pure . Left . ConnectionError $ UnsupportedVersion version
        | otherwise -> do
            let conn =
                  Connection
                    { socket = sock
                    , buffer = buffer
                    , chunkSize = chunkSize
                    , timeout = settings.timeout
                    , version = version
                    }
            case settings.password of
              Nothing -> pure $ Right conn
              Just p -> do
                r <- exchangeCommand conn (MPD.Command.password p)
                case r of
                  Right () -> pure $ Right conn
                  Left err -> do
                    N.close sock
                    pure $ Left err
  where
    open :: IO N.Socket
    open = case settings.address of
      UnixAddress path -> do
        sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
        N.connect sock (N.SockAddrUnix path) `onException` N.close sock
        pure sock
      TcpAddress host port -> do
        let hints = N.defaultHints {N.addrSocketType = N.Stream}
        addrs <- N.getAddrInfo (Just hints) (Just host) (Just (show port))
        firstSuccessful addrs

    firstSuccessful :: [N.AddrInfo] -> IO N.Socket
    firstSuccessful = \case
      [] -> throwIO . userError $ "no address for " <> show settings.address
      [a] -> openAddr a
      a : as -> openAddr a `catch` \(_ :: IOException) -> firstSuccessful as

    openAddr :: N.AddrInfo -> IO N.Socket
    openAddr a = do
      sock <- N.socket (N.addrFamily a) (N.addrSocketType a) (N.addrProtocol a)
      N.connect sock (N.addrAddress a) `onException` N.close sock
      pure sock

    connectFailed :: IOException -> MpdError
    connectFailed = ConnectionError . ConnectFailed . T.pack . displayException

    parseGreeting :: ByteString -> Maybe Version
    parseGreeting l = do
      v <- BS.stripPrefix "OK MPD " l
      case map BS8.readInt (BS8.split '.' v) of
        [Just (major, ""), Just (minor, ""), Just (patch, "")] -> Just $ Version major minor patch
        _ -> Nothing

-- | The oldest version of MPD that the library supports. It needs the
-- relative positions of 0.23.
minimumVersion :: Version
minimumVersion = Version 0 23 0

-- | @since 0.1.0.0
close :: Connection -> IO ()
close conn = N.close conn.socket

-- | Send requests and read the reply, without a timeout.
exchange :: Connection -> B.Builder -> IO (Either MpdError [[Field]])
exchange conn request = handleIO broken $ do
  NL.sendAll conn.socket (B.toLazyByteString request)
  readReply
  where
    readReply :: IO (Either MpdError [[Field]])
    readReply = go True []
      where
        go :: Bool -> [ByteString] -> IO (Either MpdError [[Field]])
        go first acc =
          readLine conn.socket conn.chunkSize conn.buffer >>= \case
            Nothing
              | first -> pure . Left $ ConnectionError Closed
              | otherwise ->
                  pure . Left . ConnectionError $ Broken "the connection closed in the middle of a reply"
            Just l
              | isFinalLine l -> pure . parseReply $ reverse (l : acc)
              | otherwise -> go False (l : acc)

    broken :: IOException -> MpdError
    broken = ConnectionError . Broken . T.pack . displayException

-- | Run a command without the timeout.
exchangeCommand :: Connection -> Command a -> IO (Either MpdError a)
exchangeCommand conn cmd = case commandRequests cmd of
  [] -> pure $ parseCommandReply cmd []
  requests -> (>>= parseCommandReply cmd) <$> exchange conn (renderRequests requests)

-- | Send bytes without reading a reply.
sendRaw :: Connection -> B.Builder -> IO (Either MpdError ())
sendRaw conn bytes = handleIO broken . fmap Right $ NL.sendAll conn.socket (B.toLazyByteString bytes)
  where
    broken :: IOException -> MpdError
    broken = ConnectionError . Broken . T.pack . displayException

-- | Run an action with a timeout, if there is one.
withTimeout :: Maybe Seconds -> IO (Either MpdError a) -> IO (Either MpdError a)
withTimeout = \case
  Nothing -> id
  Just s -> \action ->
    T.timeout (ceiling (s * microsecondsPerSecond)) action >>= \case
      Nothing -> pure . Left $ ConnectionError TimedOut
      Just r -> pure r
  where
    microsecondsPerSecond :: Seconds
    microsecondsPerSecond = 1000000

-- | Read a line without its newline. 'Nothing' if the connection closed.
readLine :: N.Socket -> Int -> IORef ByteString -> IO (Maybe ByteString)
readLine sock chunkSize buffer = do
  buf <- readIORef buffer
  case BS.elemIndex newline buf of
    Just i -> do
      writeIORef buffer $! BS.drop (i + 1) buf
      pure . Just $ BS.take i buf
    Nothing -> do
      chunk <- N.recv sock chunkSize
      if BS.null chunk
        then pure Nothing
        else do
          writeIORef buffer $! buf <> chunk
          readLine sock chunkSize buffer
  where
    newline :: Word8
    newline = 10

handleIO :: (IOException -> MpdError) -> IO (Either MpdError a) -> IO (Either MpdError a)
handleIO toError = handle (pure . Left . toError)
