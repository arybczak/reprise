-- | A connection to MPD for the worker threads. The operations throw
-- 'MpdError'.
module Reprise.Effect.Mpd
  ( -- * Effect
    Mpd (..)

    -- ** Handlers
  , runMpd

    -- ** Operations
  , connectMpd
  , runCommand
  , waitIdle
  , disconnectMpd
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception qualified as E
import Data.IORef.Strict qualified as S
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception

import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Connection
import Reprise.Mpd.Protocol.Idle
import Reprise.Mpd.Protocol.Types

data Mpd :: Effect where
  Connect :: Maybe T.Text -> Mpd m Version
  RunCommand :: Command a -> Mpd m a
  WaitIdle :: STM () -> Mpd m (Maybe [Subsystem])
  Disconnect :: Mpd m ()

type instance DispatchOf Mpd = Dynamic

-- | Run the effect with one connection at a time. A command without a
-- connection opens one first, with the password of the last connection
-- that opened, or of the settings before the first.
--
-- A connection goes to the first of the settings whose address MPD listens
-- on. The first that MPD answers on is the one, also when it refuses the
-- password, so that the user is asked for it.
runMpd :: IOE :> es => NE.NonEmpty Settings -> Eff (Mpd : es) a -> Eff es a
runMpd candidates action = do
  ref <- liftIO $ S.newIORef Nothing
  passwordRef <- liftIO $ S.newIORef (NE.head candidates).password
  let disconnect = S.readIORef ref >>= mapM_ close >> S.writeIORef ref Nothing
      connected p = do
        conn <- connectFirst (fmap (\s -> s {password = p}) candidates)
        S.writeIORef ref (Just conn)
        S.writeIORef passwordRef p
        pure conn
      withConnection' :: (Connection -> IO a) -> IO a
      withConnection' f = S.readIORef ref >>= maybe (connected =<< S.readIORef passwordRef) pure >>= f
  let handled = interpretWith_ action $ \case
        Connect p -> liftIO $ do
          disconnect
          serverVersion <$> connected p
        -- A command without requests, e.g. one that marks a point in the
        -- queue of requests, needs no connection.
        RunCommand cmd
          | null (commandRequests cmd) -> either throwIO pure $ parseCommandReply cmd []
          | otherwise -> liftIO $ withConnection' (`run` cmd)
        WaitIdle interrupted ->
          liftIO . withConnection' $ \conn ->
            either (const Nothing) Just <$> race (atomically interrupted) (idle conn [])
        Disconnect -> liftIO disconnect
  handled `finally` liftIO disconnect
  where
    connectFirst :: NE.NonEmpty Settings -> IO Connection
    connectFirst (s NE.:| rest) = case NE.nonEmpty rest of
      Nothing -> connect s
      Just others ->
        connect s `E.catch` \case
          ConnectionError (ConnectFailed _) -> connectFirst others
          err -> E.throwIO err

-- | Open a new connection, with a password or without one, closing the old
-- one. If it opens, the connections that open by themselves for a command
-- send the password too.
connectMpd :: Mpd :> es => Maybe T.Text -> Eff es Version
connectMpd = send . Connect

runCommand :: Mpd :> es => Command a -> Eff es a
runCommand = send . RunCommand

-- | Wait for changes of any subsystem, or until the transaction returns.
-- After it returned, the connection is unusable.
waitIdle :: Mpd :> es => STM () -> Eff es (Maybe [Subsystem])
waitIdle = send . WaitIdle

disconnectMpd :: Mpd :> es => Eff es ()
disconnectMpd = send Disconnect
