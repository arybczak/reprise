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
  Connect :: Mpd m Version
  RunCommand :: Command a -> Mpd m a
  WaitIdle :: Mpd m [Subsystem]
  Disconnect :: Mpd m ()

type instance DispatchOf Mpd = Dynamic

-- | Run the effect with one connection at a time. A command without a
-- connection opens one first. Each connection reads the password anew, so
-- that it has the one that the user gave last.
runMpd :: IOE :> es => Settings -> IO (Maybe T.Text) -> Eff (Mpd : es) a -> Eff es a
runMpd settings currentPassword action = do
  ref <- liftIO $ S.newIORef Nothing
  let disconnect = S.readIORef ref >>= mapM_ close >> S.writeIORef ref Nothing
      connected = do
        p <- currentPassword
        conn <- connect settings {password = p}
        S.writeIORef ref (Just conn)
        pure conn
      withConnection' :: (Connection -> IO a) -> IO a
      withConnection' f = S.readIORef ref >>= maybe connected pure >>= f
  let handled = interpretWith_ action $ \case
        Connect -> liftIO $ do
          disconnect
          serverVersion <$> connected
        RunCommand cmd -> liftIO $ withConnection' (`run` cmd)
        WaitIdle -> liftIO $ withConnection' (`idle` [])
        Disconnect -> liftIO disconnect
  handled `finally` liftIO disconnect

-- | Open a new connection, closing the old one.
connectMpd :: Mpd :> es => Eff es Version
connectMpd = send Connect

runCommand :: Mpd :> es => Command a -> Eff es a
runCommand = send . RunCommand

-- | Wait for changes of any subsystem.
waitIdle :: Mpd :> es => Eff es [Subsystem]
waitIdle = send WaitIdle

disconnectMpd :: Mpd :> es => Eff es ()
disconnectMpd = send Disconnect
