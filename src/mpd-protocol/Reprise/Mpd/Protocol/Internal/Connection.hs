{-# OPTIONS_HADDOCK not-home #-}

-- | The internals of a connection, shared by "Reprise.Mpd.Protocol.Connection" and
-- "Reprise.Mpd.Protocol.Idle".
module Reprise.Mpd.Protocol.Internal.Connection
  ( -- * Connection
    Connection (..)
  , Address (..)
  , Settings (..)
  , connect
  , close

    -- * Round trips
  , exchange
  , exchangeCommand
  , withTimeout
  , sendRaw
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.IORef.Strict qualified as S
import Data.Text qualified as T
import Data.Word
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N
import Network.Socket.ByteString.Lazy qualified as NL
import System.Timeout qualified as T

import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Response
import Reprise.Mpd.Protocol.Types

-- | A connection to MPD. It is not safe to use from more than one thread at a
-- time, except for 'Reprise.Mpd.Protocol.Idle.noidle'.
data Connection = Connection
  { socket :: N.Socket
  , buffer :: S.IORef BS.ByteString
  -- ^ Bytes that arrived after the last line that was read.
  , chunkSize :: Int
  , timeout :: Maybe Seconds
  , version :: Version
  }

-- | Where MPD listens.
data Address
  = TcpAddress N.HostName N.PortNumber
  | UnixAddress FilePath
  deriving stock (Eq, Show)

data Settings = Settings
  { address :: Address
  , password :: Maybe T.Text
  , timeout :: Maybe Seconds
  -- ^ How long to wait for a connection or a reply. 'Nothing' waits
  -- forever. @idle@ always waits forever.
  }
  deriving stock (Eq, Show)

-- | Open a connection, check the version of MPD and send the password.
-- Throws 'MpdError'.
connect :: Settings -> IO Connection
connect settings = withTimeout settings.timeout . convertIO connectFailed $ do
  bracketOnError open N.close $ \sock -> do
    buffer <- S.newIORef BS.empty
    -- A receive never returns more than the socket's buffer holds.
    chunkSize <- N.getSocketOption sock N.RecvBuffer
    greeting <- readLine sock chunkSize buffer
    case parseGreeting =<< greeting of
      Nothing ->
        throwIO . ConnectionError . ConnectFailed $
          "unexpected greeting: " <> maybe "" decode greeting
      Just version
        | version < minimumVersion -> throwIO . ConnectionError $ UnsupportedVersion version
        | otherwise -> do
            let conn =
                  Connection
                    { socket = sock
                    , buffer = buffer
                    , chunkSize = chunkSize
                    , timeout = settings.timeout
                    , version = version
                    }
            mapM_ (exchangeCommand conn . Reprise.Mpd.Protocol.Command.password) settings.password
            pure conn
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

    parseGreeting :: BS.ByteString -> Maybe Version
    parseGreeting l = do
      v <- BS.stripPrefix "OK MPD " l
      case traverse readInt (BS8.split '.' v) of
        Just [major, minor, patch] -> Just $ Version major minor patch
        _ -> Nothing

close :: Connection -> IO ()
close conn = N.close conn.socket

-- | Send requests and read the reply, without a timeout. Throws 'MpdError'.
exchange :: Connection -> B.Builder -> IO [[Field]]
exchange conn request = convertIO broken $ do
  NL.sendAll conn.socket (B.toLazyByteString request)
  go True []
  where
    go :: Bool -> [BS.ByteString] -> IO [[Field]]
    go first acc =
      readLine conn.socket conn.chunkSize conn.buffer >>= \case
        Nothing
          | first -> throwIO $ ConnectionError Closed
          | otherwise ->
              throwIO . ConnectionError $ Broken "the connection closed in the middle of a reply"
        Just l
          | isFinalLine l -> either throwIO pure . parseReply $ reverse (l : acc)
          | otherwise -> go False (l : acc)

-- | Run a command without the timeout. Throws 'MpdError'.
exchangeCommand :: Connection -> Command a -> IO a
exchangeCommand conn cmd = do
  parts <- case commandRequests cmd of
    [] -> pure []
    requests -> case [r | r <- requests, any (T.elem '\n') r.arguments] of
      -- MPD would read a second command after the line break, and send a
      -- reply that no request reads.
      r : _ -> throwIO . ProtocolError $ r.command <> ": an argument has a line break"
      [] -> exchange conn (renderRequests requests)
  either throwIO pure $ parseCommandReply cmd parts

-- | Send bytes without reading a reply. Throws 'MpdError'.
sendRaw :: Connection -> B.Builder -> IO ()
sendRaw conn bytes = convertIO broken . NL.sendAll conn.socket $ B.toLazyByteString bytes

-- | Run an action with a timeout, if there is one. Throws 'TimedOut'.
withTimeout :: Maybe Seconds -> IO a -> IO a
withTimeout = \case
  Nothing -> id
  Just s -> \action ->
    T.timeout (fromInteger (min (toInteger (maxBound @Int)) microseconds)) action
      >>= maybe (throwIO $ ConnectionError TimedOut) pure
    where
      -- A timeout too long for an Int waits as long as one can.
      microseconds :: Integer
      microseconds = ceiling (s * microsecondsPerSecond)
  where
    microsecondsPerSecond :: Seconds
    microsecondsPerSecond = 1000000

-- | Read a line without its newline. 'Nothing' if the connection closed.
readLine :: N.Socket -> Int -> S.IORef BS.ByteString -> IO (Maybe BS.ByteString)
readLine sock chunkSize buffer = do
  buf <- S.readIORef buffer
  case BS.elemIndex newline buf of
    Just i -> do
      S.writeIORef buffer (BS.drop (i + 1) buf)
      pure . Just $ BS.take i buf
    Nothing -> do
      chunk <- N.recv sock chunkSize
      if BS.null chunk
        then pure Nothing
        else do
          S.writeIORef buffer (buf <> chunk)
          readLine sock chunkSize buffer
  where
    newline :: Word8
    newline = 10

broken :: IOException -> MpdError
broken = ConnectionError . Broken . T.pack . displayException

-- | Turn an I/O error of the socket into an 'MpdError', so that a caller
-- catches one type.
convertIO :: (IOException -> MpdError) -> IO a -> IO a
convertIO toError = handle (throwIO . toError)
