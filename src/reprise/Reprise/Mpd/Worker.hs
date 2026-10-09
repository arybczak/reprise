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
import Reprise.Exception
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Types

-- | What the workers need from the rest of the program.
data Workers = Workers
  { emit :: AppEvent -> IO ()
  , logLine :: T.Text -> IO ()
  , requests :: TQueue PendingRequest
  , password :: TVar (Maybe T.Text)
  -- ^ The password that the connections send.
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
      liftIO . w.emit . MpdDisconnected $ exceptionText err
      liftIO $ threadDelay retryInterval

-- | Run the requests one at a time and send the events of their replies.
--
-- When MPD refuses a command without a password, or refuses the password,
-- the worker asks for one with 'PasswordNeeded'. The command and every
-- request after it wait for the answer, so that they still run in order.
-- A password that MPD accepts is kept for the next connections of both
-- workers. A cancel fails the refused command.
commandWorker :: (Mpd :> es, IOE :> es) => Workers -> Eff es ()
commandWorker w = forever $ runRequests . pure =<< nextRequest
  where
    nextRequest :: IOE :> es => Eff es PendingRequest
    nextRequest = liftIO . atomically $ readTQueue w.requests

    runRequests :: (Mpd :> es, IOE :> es) => [PendingRequest] -> Eff es ()
    runRequests = \case
      [] -> pure ()
      PasswordAnswer _ : rs -> runRequests rs
      r@(PendingRequest cmd onFailure k) : rs ->
        try (runCommand cmd `catch` retryClosed cmd) >>= \case
          Right a -> do
            liftIO . w.emit $ k a
            runRequests rs
          Left err
            | refused err -> askPassword err onFailure r rs
            | otherwise -> do
                failed err onFailure
                runRequests rs

    -- The refused request, with its failure event, and the requests that
    -- wait for it.
    askPassword
      :: (Mpd :> es, IOE :> es)
      => MpdError -> (MpdError -> AppEvent) -> PendingRequest -> [PendingRequest] -> Eff es ()
    askPassword err onFailure r held = do
      liftIO . w.emit $ PasswordNeeded err
      waitForAnswer held
      where
        waitForAnswer :: (Mpd :> es, IOE :> es) => [PendingRequest] -> Eff es ()
        waitForAnswer rs =
          nextRequest >>= \case
            PasswordAnswer Nothing -> do
              liftIO . w.emit $ onFailure err
              runRequests rs
            PasswordAnswer (Just p) ->
              authenticate p >>= \case
                Nothing -> runRequests (r : rs)
                Just err'
                  | refused err' -> askPassword err' onFailure r rs
                  | otherwise -> do
                      failed err' onFailure
                      runRequests rs
            other -> waitForAnswer (rs <> [other])

    -- A new connection sends the password, which also works when MPD
    -- refused the one that the connection had.
    authenticate :: (Mpd :> es, IOE :> es) => T.Text -> Eff es (Maybe MpdError)
    authenticate p = do
      old <- liftIO . atomically $ swapTVar w.password (Just p)
      try connectMpd >>= \case
        Right _ -> pure Nothing
        Left err -> do
          liftIO . atomically $ writeTVar w.password old
          pure (Just err)

    failed :: (Mpd :> es, IOE :> es) => MpdError -> (MpdError -> AppEvent) -> Eff es ()
    failed err onFailure = do
      case err of
        ConnectionError _ -> do
          logError w err
          disconnectMpd
        ProtocolError _ -> logError w err
        AckError _ -> pure ()
      liftIO . w.emit $ onFailure err

    refused :: MpdError -> Bool
    refused = \case
      AckError ack -> ack.code `elem` [AckPermission, AckPassword]
      _ -> False

    -- MPD closes a connection that was unused for a while, before it runs
    -- the command, so the command runs again on a new connection.
    retryClosed :: Mpd :> es => Command a -> MpdError -> Eff es a
    retryClosed cmd = \case
      ConnectionError Closed -> do
        disconnectMpd
        runCommand cmd
      err -> throwIO err

logError :: IOE :> es => Workers -> MpdError -> Eff es ()
logError w = liftIO . w.logLine . exceptionText
