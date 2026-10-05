-- | The threads that talk to MPD. Each is a plain blocking loop on its own
-- connection, and sends events to the UI. An 'MpdError' becomes an event
-- here, because an exception can't reach the UI thread.
module Reprise.Mpd.Worker
  ( Workers (..)
  , idleWorker
  , commandWorker
  , retryInterval
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Text qualified as T
import Effectful
import Effectful.Exception

import Reprise.Effect.Mpd
import Reprise.Effect.MpdRequest
import Reprise.Event
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Types

-- | What the workers need from the rest of the program.
data Workers = Workers
  { emit :: AppEvent -> IO ()
  , logLine :: T.Text -> IO ()
  , requests :: TQueue PendingRequest
  }

-- | How long to wait before connecting again, as ncmpcpp does.
retryInterval :: Int
retryInterval = 1000000

-- | Run @idle@ in a loop and send the changes. Reconnect after an error.
idleWorker :: (Mpd :> es, IOE :> es) => Workers -> Eff es ()
idleWorker w = forever $ do
  try @MpdError connectMpd >>= \case
    Left err -> disconnected err
    Right version -> do
      liftIO . w.emit $ MpdConnected version
      forever (waitIdle >>= liftIO . w.emit . MpdChanged) `catch` \err -> do
        logError w err
        disconnectMpd
        disconnected err
  where
    disconnected :: IOE :> es => MpdError -> Eff es ()
    disconnected err = do
      liftIO . w.emit . MpdDisconnected $ T.pack (displayException err)
      liftIO $ threadDelay retryInterval

-- | Run the requests one at a time and send the events of their replies.
commandWorker :: (Mpd :> es, IOE :> es) => Workers -> Eff es ()
commandWorker w = forever $ do
  PendingRequest cmd onFailure k <- liftIO . atomically $ readTQueue w.requests
  try (runCommand cmd `catch` retryClosed cmd) >>= \case
    Right a -> liftIO . w.emit $ k a
    Left err -> do
      case err of
        ConnectionError _ -> do
          logError w err
          disconnectMpd
        ProtocolError _ -> logError w err
        AckError _ -> pure ()
      liftIO . w.emit $ onFailure err
  where
    -- MPD closes a connection that was unused for a while, before it runs
    -- the command, so the command runs again on a new connection.
    retryClosed :: Mpd :> es => Command a -> MpdError -> Eff es a
    retryClosed cmd = \case
      ConnectionError Closed -> do
        disconnectMpd
        runCommand cmd
      err -> throwIO err

logError :: IOE :> es => Workers -> MpdError -> Eff es ()
logError w = liftIO . w.logLine . T.pack . displayException
