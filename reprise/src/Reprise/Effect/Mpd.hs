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

import Data.IORef
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import MPD.Command
import MPD.Connection
import MPD.Idle
import MPD.Types

data Mpd :: Effect where
  Connect :: Mpd m Version
  RunCommand :: Command a -> Mpd m a
  WaitIdle :: Mpd m [Subsystem]
  Disconnect :: Mpd m ()

type instance DispatchOf Mpd = Dynamic

-- | Run the effect with one connection at a time. A command without a
-- connection opens one first.
runMpd :: IOE :> es => Settings -> Eff (Mpd : es) a -> Eff es a
runMpd settings action = do
  ref <- liftIO $ newIORef Nothing
  let disconnect = readIORef ref >>= mapM_ close >> writeIORef ref Nothing
      connected = do
        conn <- connect settings
        writeIORef ref (Just conn)
        pure conn
      withConnection' :: (Connection -> IO a) -> IO a
      withConnection' f = readIORef ref >>= maybe connected pure >>= f
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
