-- | The threads that talk to MPD. Each is a plain blocking loop on its own
-- connection, and sends events to the UI.
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
import MPD.Types

import Reprise.Effect.Mpd
import Reprise.Effect.MpdRequest
import Reprise.Event
import Reprise.Handler

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
  connectMpd >>= \case
    Left err -> do
      liftIO . w.emit . MpdDisconnected $ describeMpdError err
      liftIO $ threadDelay retryInterval
    Right version -> do
      liftIO . w.emit $ MpdConnected version
      let loop =
            waitIdle >>= \case
              Right subsystems -> do
                liftIO . w.emit $ MpdChanged subsystems
                loop
              Left err -> do
                logError w err
                disconnectMpd
                liftIO . w.emit . MpdDisconnected $ describeMpdError err
                liftIO $ threadDelay retryInterval
      loop

-- | Run the requests one at a time and send the events of their replies.
commandWorker :: (Mpd :> es, IOE :> es) => Workers -> Eff es ()
commandWorker w = forever $ do
  PendingRequest cmd k <- liftIO . atomically $ readTQueue w.requests
  r <-
    runCommand cmd >>= \case
      -- MPD closes a connection that was unused for a while, before it
      -- runs the command, so the command runs again on a new connection.
      Left (ConnectionError Closed) -> do
        disconnectMpd
        runCommand cmd
      other -> pure other
  case r of
    Left err@(ConnectionError _) -> do
      logError w err
      disconnectMpd
    Left err@(ProtocolError _) -> logError w err
    _ -> pure ()
  liftIO . w.emit $ k r

logError :: IOE :> es => Workers -> MpdError -> Eff es ()
logError w = liftIO . w.logLine . describeMpdError
