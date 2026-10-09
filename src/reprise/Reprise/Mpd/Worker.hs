-- | The threads that talk to MPD. Each is a plain blocking loop on its own
-- connection, and sends events to the UI. An 'MpdError' becomes an event
-- here, because an exception can't reach the UI thread.
module Reprise.Mpd.Worker
  ( Workers (..)
  , idleWorker
  , commandWorker
  , retryInterval
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.Text qualified as T
import Effectful
import Effectful.Exception

import Reprise.Effect.Clock
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
  -- ^ The password that MPD accepted last, which the idle connection sends
  -- when it connects again.
  , retryDelay :: Double
  -- ^ How long to wait before connecting again, 'retryInterval' but in
  -- the tests.
  , commandConnectionFailed :: TVar Bool
  -- ^ Whether the connection of the commands failed since the idle
  -- connection opened.
  }

-- | How long to wait before connecting again, as ncmpcpp does.
retryInterval :: Double
retryInterval = 1

-- | Run @idle@ in a loop and send the changes. Reconnect after an error.
-- A connection that MPD refuses for its password connects again with the
-- next password, which the command worker gets from the user.
--
-- A peer that is gone, e.g. after a change of the network, never ends
-- @idle@. When the connection of the commands fails, the idle connection
-- opens anew too, since it may wait for such a peer.
idleWorker :: (Mpd :> es, IOE :> es) => Workers -> Eff es ()
idleWorker w = forever $ do
  sent <- liftIO $ readTVarIO w.password
  liftIO . atomically $ writeTVar w.commandConnectionFailed False
  try @MpdError (connectMpd sent) >>= \case
    Left err -> disconnected sent err
    Right version -> do
      emitEvent w $ MpdConnected version
      watch `catch` \err -> do
        logError w err
        disconnectMpd
        disconnected sent err
  where
    watch :: (Mpd :> es, IOE :> es) => Eff es ()
    watch =
      waitIdle (readTVar w.commandConnectionFailed >>= check) >>= \case
        Just changed -> do
          emitEvent w $ MpdChanged changed
          watch
        Nothing -> disconnectMpd

    -- After the password that the connection sent. Only the command worker
    -- asks for a password, so a refused connection sends it a command that
    -- needs what @idle@ needs, which MPD refuses too, e.g. at the start with
    -- a wrong password, before any command of the user.
    disconnected :: IOE :> es => Maybe T.Text -> MpdError -> Eff es ()
    disconnected sent err = do
      emitEvent w . MpdDisconnected $ exceptionText err
      liftIO $
        if refused err
          then do
            atomically . writeTQueue w.requests $
              PendingRequest status (const MpdDone) (const MpdDone)
            atomically $ readTVar w.password >>= check . (/= sent)
          else delaySeconds w.retryDelay

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
            emitEvent w $ k a
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
      emitEvent w $ PasswordNeeded err
      waitForAnswer held
      where
        waitForAnswer :: (Mpd :> es, IOE :> es) => [PendingRequest] -> Eff es ()
        waitForAnswer rs =
          nextRequest >>= \case
            PasswordAnswer Nothing -> do
              emitEvent w $ onFailure err
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
    authenticate p =
      try (connectMpd (Just p)) >>= \case
        Right _ -> do
          liftIO . atomically $ writeTVar w.password (Just p)
          pure Nothing
        Left err -> pure (Just err)

    failed :: (Mpd :> es, IOE :> es) => MpdError -> (MpdError -> AppEvent) -> Eff es ()
    failed err onFailure = do
      case err of
        ConnectionError _ -> do
          logError w err
          disconnectMpd
          liftIO . atomically $ writeTVar w.commandConnectionFailed True
        ProtocolError _ -> logError w err
        AckError _ -> pure ()
      emitEvent w $ onFailure err

    -- MPD closes a connection that was unused for a while, before it runs
    -- the command, so the command runs again on a new connection.
    retryClosed :: Mpd :> es => Command a -> MpdError -> Eff es a
    retryClosed cmd = \case
      ConnectionError Closed -> do
        disconnectMpd
        runCommand cmd
      err -> throwIO err

emitEvent :: IOE :> es => Workers -> AppEvent -> Eff es ()
emitEvent w = liftIO . w.emit

logError :: IOE :> es => Workers -> MpdError -> Eff es ()
logError w = liftIO . w.logLine . exceptionText

-- | Whether MPD refused a command without a password, or the password.
refused :: MpdError -> Bool
refused = \case
  AckError ack -> ack.code `elem` [AckPermission, AckPassword]
  _ -> False
