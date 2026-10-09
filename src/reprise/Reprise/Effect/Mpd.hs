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
import Data.IORef.Strict qualified as S
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
runMpd :: IOE :> es => Settings -> Eff (Mpd : es) a -> Eff es a
runMpd settings action = do
  ref <- liftIO $ S.newIORef Nothing
  passwordRef <- liftIO $ S.newIORef settings.password
  let disconnect = S.readIORef ref >>= mapM_ close >> S.writeIORef ref Nothing
      connected p = do
        conn <- connect settings {password = p}
        S.writeIORef ref (Just conn)
        S.writeIORef passwordRef p
        pure conn
      withConnection' :: (Connection -> IO a) -> IO a
      withConnection' f = S.readIORef ref >>= maybe (connected =<< S.readIORef passwordRef) pure >>= f
  let handled = interpretWith_ action $ \case
        Connect p -> liftIO $ do
          disconnect
          serverVersion <$> connected p
        RunCommand cmd -> liftIO $ withConnection' (`run` cmd)
        WaitIdle interrupted ->
          liftIO . withConnection' $ \conn ->
            either (const Nothing) Just <$> race (atomically interrupted) (idle conn [])
        Disconnect -> liftIO disconnect
  handled `finally` liftIO disconnect

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
