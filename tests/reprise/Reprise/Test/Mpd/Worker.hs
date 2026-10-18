module Reprise.Test.Mpd.Worker (workerTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Monad
import Data.IORef.Strict qualified as S
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import GHC.Clock
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Effect.Mpd
import Reprise.Effect.MpdRequest
import Reprise.Event
import Reprise.Exception
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Connection hiding (run)
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Mpd.TestServer
import Reprise.Mpd.Worker

workerTests :: TestTree
workerTests =
  testGroup
    "worker"
    [ testCase "a command runs again after MPD closed the connection" test_retryClosed
    , testCase "a failed command sends its failure event" test_failed
    , testCase "a broken connection is closed" test_broken
    , testCase "a refused command runs again with the password" test_password
    , testCase "a wrong password asks again" test_wrongPassword
    , testCase "the requests after a refused one wait for it" test_passwordHolds
    , testCase "a cancelled password fails the refused command" test_passwordCancelled
    , testCase "the idle connection sends the password" test_idlePassword
    , testCase "a refused idle connection waits for the password" test_idleRefused
    , testCase "a wrong password at the start asks for another" test_idleWrongPassword
    , testCase "a failed command connection reopens the idle one" test_idleAfterCommandFailed
    , testCase "a command without requests needs no connection" test_noConnection
    , withResource oversizedServer stopTestServer $ \getServer ->
        testGroup
          "MPD's limits"
          [ testCase
              "a reply that MPD refuses fails without a reconnect"
              (test_oversizedReply getServer)
          , testCase "a command list too large for MPD" (test_largeCommandList getServer)
          ]
    ]

test_retryClosed :: Assertion
test_retryClosed = do
  r <- runWorker [Left (ConnectionError Closed), Right ()] [stop] []
  assertEqual "events" [MpdDone] r.events
  assertEqual "calls" ["run stop", "disconnect", "run stop"] r.calls

test_failed :: Assertion
test_failed = do
  let ack = Ack AckArg 0 "play" "Bad song index"
  r <- runWorker [Left (AckError ack)] [play (Just 99)] []
  assertEqual "events" [MpdFailed [Request "play" ["99"]] (AckError ack)] r.events
  assertEqual "the connection stays" ["run play"] r.calls
  assertEqual "the idle connection stays" False r.connectionFailed

test_broken :: Assertion
test_broken = do
  let err = ConnectionError (Broken "reset")
  r <- runWorker [Left err, Right ()] [stop, stop] []
  assertEqual "events" [MpdFailed [Request "stop" []] err, MpdDone] r.events
  assertEqual "calls" ["run stop", "disconnect", "run stop"] r.calls
  assertEqual "the idle connection opens anew" True r.connectionFailed

test_password :: Assertion
test_password = do
  r <- runWorker [Left refusal, Right (), Right ()] [stop] [Just "secret"]
  assertEqual "events" [PasswordNeeded refusal, MpdDone] r.events
  assertEqual "calls" ["run stop", "connect secret", "run stop"] r.calls
  assertEqual "the password for the idle connection" (Just "secret") r.password

test_wrongPassword :: Assertion
test_wrongPassword = do
  r <- runWorker [Left refusal, Left wrongPassword] [stop] [Just "wrong", Nothing]
  assertEqual
    "events"
    [ PasswordNeeded refusal
    , PasswordNeeded wrongPassword
    , MpdFailed [Request "stop" []] wrongPassword
    ]
    r.events
  assertEqual "the password stays" Nothing r.password

test_passwordHolds :: Assertion
test_passwordHolds = do
  r <- runWorker [Left refusal] [stop, pause True] [Just "secret"]
  assertEqual "events" [PasswordNeeded refusal, MpdDone, MpdDone] r.events
  assertEqual "calls" ["run stop", "connect secret", "run stop", "run pause"] r.calls

test_passwordCancelled :: Assertion
test_passwordCancelled = do
  r <- runWorker [Left refusal] [stop, pause True] [Nothing]
  assertEqual
    "events"
    [PasswordNeeded refusal, MpdFailed [Request "stop" []] refusal, MpdDone]
    r.events
  assertEqual "calls" ["run stop", "run pause"] r.calls

-- | The idle connection sends the password that MPD accepted last.
test_idlePassword :: Assertion
test_idlePassword = withIdleWorker [] (Just "secret") $ \events callsRef _ -> do
  connected <- expectWithin . atomically $ readTQueue events
  assertEqual "connected" (MpdConnected (Version 0 24 0)) connected
  assertEqual "calls" ["connect secret"] =<< S.readIORef callsRef

-- | An idle connection that MPD refuses for its password waits for the
-- next password, instead of connecting again and again. A command makes the
-- command worker ask for it.
test_idleRefused :: Assertion
test_idleRefused = withIdleWorker [Right (), Left refusal] Nothing $ \events callsRef workers -> do
  let nextEvent = expectWithin . atomically $ readTQueue events
  assertEqual "connected" (MpdConnected (Version 0 24 0)) =<< nextEvent
  assertEqual "refused" (MpdDisconnected (exceptionText refusal)) =<< nextEvent
  assertEqual "a command that asks" [Request "status" []] . pendingRequestLines
    =<< expectWithin (atomically (readTQueue workers.requests))
  -- Ten times the retry delay of the tests.
  threadDelay 100000
  assertEqual "not again" ["connect", "disconnect"] . reverse =<< S.readIORef callsRef
  atomically $ writeTVar workers.password (Just "secret")
  assertEqual "with the password" (MpdConnected (Version 0 24 0)) =<< nextEvent
  assertEqual
    "connected again"
    ["connect", "disconnect", "connect secret"]
    . reverse
    =<< S.readIORef callsRef

-- | A wrong password at the start, e.g. in the config, asks for another
-- one, though the user sent no command yet.
test_idleWrongPassword :: Assertion
test_idleWrongPassword = do
  events <- newTQueueIO
  passwordVar <- newTVarIO (Just "wrong")
  workers <- testWorkers events passwordVar
  -- MPD refuses the password of every connection but the right one's. A
  -- command without a connection opens one with the password of the last
  -- one that opened, as 'runMpd' does.
  let mpd :: IOE :> es => S.IORef (Maybe T.Text) -> Eff (Mpd : es) a -> Eff es a
      mpd passwordRef = interpret_ $ \case
        Connect p -> liftIO $ connected p <* S.writeIORef passwordRef p
        RunCommand cmd -> do
          _ <- liftIO $ connected =<< S.readIORef passwordRef
          either throwIO pure $ parseCommandReply cmd [[] | _ <- commandRequests cmd]
        WaitIdle interrupted -> Nothing <$ liftIO (atomically interrupted)
        Disconnect -> pure ()
        where
          connected :: Maybe T.Text -> IO Version
          connected = \case
            Just "secret" -> pure $ Version 0 24 0
            _ -> E.throwIO wrongPassword
      start worker = do
        passwordRef <- S.newIORef (Just "wrong")
        forkIO . runEff $ mpd passwordRef worker
  E.bracket (mapM start [idleWorker workers, commandWorker workers]) (mapM_ killThread) $ \_ -> do
    asked <- expectWithin . untilEvent events $ \case PasswordNeeded _ -> True; _ -> False
    assertEqual "asked" (PasswordNeeded wrongPassword) asked
    atomically . writeTQueue workers.requests $ PasswordAnswer (Just "secret")
    assertEqual "connected" (MpdConnected (Version 0 24 0))
      =<< expectWithin (untilEvent events (\case MpdConnected _ -> True; _ -> False))

-- | A command without requests, which marks a point in the queue of
-- requests, e.g. of a quit, runs without a connection, also when MPD can't
-- be reached.
test_noConnection :: Assertion
test_noConnection = do
  let nowhere =
        Settings
          { address = UnixAddress "/nonexistent/socket"
          , password = Nothing
          , timeout = Just 1
          }
  r <-
    expectWithin . E.try @MpdError . runEff . runMpd (nowhere NE.:| []) $ runCommand (pure ())
  assertEqual "done" (Right ()) r

-- | The idle connection opens anew after the connection of the commands
-- failed, since it may wait for a peer that is gone. It waits first, as a
-- command may fail on every connection, and each fetches the queue again.
test_idleAfterCommandFailed :: Assertion
test_idleAfterCommandFailed = withIdleWorker [] Nothing $ \events callsRef workers -> do
  let nextEvent = expectWithin . atomically $ readTQueue events
  assertEqual "connected" (MpdConnected (Version 0 24 0)) =<< nextEvent
  start <- getMonotonicTime
  atomically $ writeTVar workers.commandConnectionFailed True
  assertEqual "connected again" (MpdConnected (Version 0 24 0)) =<< nextEvent
  end <- getMonotonicTime
  assertBool "after the wait" (end - start >= workers.retryDelay)
  assertEqual "calls" ["connect", "disconnect", "connect"] . reverse
    =<< S.readIORef callsRef
  assertEqual "not failed any more" False =<< readTVarIO workers.commandConnectionFailed

-- | MPD closes the connection rather than send a reply larger than its
-- output buffer, e.g. of a long queue. That isn't its close of an unused
-- connection, as a new connection fails the same way, so the command fails
-- after one more try. The idle connection stays, as each new connection
-- fetches the queue again.
test_oversizedReply :: IO TestServer -> Assertion
test_oversizedReply getServer = do
  server <- getServer
  let mpd :: Eff [Mpd, IOE] a -> IO a
      mpd = runEff . runMpd (serverSettings server NE.:| [])
      fetch = (,) <$> status <*> playlistInfo
  mpd . runCommand $ replicateM_ songs (add "a.flac" Nothing)
  events <- newTQueueIO
  passwordVar <- newTVarIO Nothing
  workers <- testWorkers events passwordVar
  E.bracket
    (mapM (forkIO . mpd) [idleWorker workers, commandWorker workers])
    (mapM_ killThread)
    $ \_ -> do
      _ <- expectWithin . untilEvent events $ \case MpdConnected _ -> True; _ -> False
      -- The fetch then goes to a connection that ran a command, which MPD's
      -- close of an unused connection could have closed.
      atomically . writeTQueue workers.requests $
        PendingRequest status (MpdFailed (commandRequests status)) (const MpdDone)
      _ <- expectWithin $ untilEvent events (== MpdDone)
      atomically . writeTQueue workers.requests $
        PendingRequest fetch (MpdFailed (commandRequests fetch)) (const MpdDone)
      failure <- expectWithin . untilEvent events $ \case MpdFailed {} -> True; _ -> False
      assertEqual
        "the failure"
        (MpdFailed (commandRequests fetch) (ConnectionError NoReply))
        failure
      assertEqual "the idle connection stays" False
        =<< readTVarIO workers.commandConnectionFailed
  where
    -- Their @playlistinfo@ takes over 100 kB, more than the 17 kB of
    -- 'oversizedServer'.
    songs :: Int
    songs = 1000

-- | MPD closes the connection of a command list larger than its
-- @max_command_list_size@, 2 MiB by default, e.g. of the adds of a large
-- selection, without running any of it. A connection that opened for the
-- list fails with the setting to raise.
test_largeCommandList :: IO TestServer -> Assertion
test_largeCommandList getServer = do
  server <- getServer
  -- Each ping is 5 bytes with its line break.
  let pings = replicateM_ (2 * 1024 * 1024 `div` 5 + 1) ping
  r <- E.try @MpdError . runEff . runMpd (serverSettings server NE.:| []) $ runCommand pings
  assertEqual "the error" (Left (ConnectionError CommandListTooLarge)) r

-- | A server whose output buffer takes 17 kB: 16 kB of each client's
-- buffer, and 1 kB more that @max_output_buffer_size@ allows.
oversizedServer :: IO TestServer
oversizedServer = startTestServerWith ["max_output_buffer_size \"1\""] [testSong "a.flac"]

----------------------------------------
-- Helpers

serverSettings :: TestServer -> Settings
serverSettings server =
  Settings {address = UnixAddress server.socketPath, password = Nothing, timeout = Just 5}

-- | The next event that fits, after the events before it.
untilEvent :: TQueue AppEvent -> (AppEvent -> Bool) -> IO AppEvent
untilEvent events p = atomically (readTQueue events) >>= \e -> if p e then pure e else untilEvent events p

refusal :: MpdError
refusal = AckError $ Ack AckPermission 0 "stop" "you don't have permission for \"stop\""

wrongPassword :: MpdError
wrongPassword = AckError $ Ack AckPassword 0 "password" "incorrect password"

-- | Wait for what the workers do at once, failing instead of hanging if it
-- never comes.
expectWithin :: IO a -> IO a
expectWithin act = timeout (5 * 1000000) act >>= maybe (assertFailure "nothing came") pure

data WorkerRun = WorkerRun
  { events :: [AppEvent]
  , calls :: [String]
  , password :: Maybe T.Text
  -- ^ The password for the next connections.
  , connectionFailed :: Bool
  }

-- | Run the command worker for the commands, with an MPD that replies to
-- the commands and the connections with the outcomes in turn. Each
-- 'PasswordNeeded' gets the next answer.
runWorker :: [Either MpdError ()] -> [Command ()] -> [Maybe T.Text] -> IO WorkerRun
runWorker outcomes commands answers = do
  outcomesRef <- S.newIORef outcomes
  callsRef <- S.newIORef []
  events <- newTQueueIO
  passwordVar <- newTVarIO Nothing
  workers <- testWorkers events passwordVar
  let requests = workers.requests
  -- With the failure event of 'request'.
  atomically . forM_ commands $ \cmd ->
    writeTQueue requests $
      PendingRequest cmd (MpdFailed (commandRequests cmd)) (const MpdDone)
  let worker = forkIO . runEff . runScripted outcomesRef callsRef $ commandWorker workers
      -- A command ends in its reply or its failure, and each answer in
      -- another event.
      collect :: [Maybe T.Text] -> Int -> IO [AppEvent]
      collect pending n
        | n == 0 = pure []
        | otherwise = do
            e <- expectWithin . atomically $ readTQueue events
            pending' <- case (e, pending) of
              (PasswordNeeded _, a : as) -> do
                atomically . writeTQueue requests $ PasswordAnswer a
                pure as
              _ -> pure pending
            (e :) <$> collect pending' (n - 1)
      run = do
        received <- collect answers (length commands + length answers)
        calls <- S.readIORef callsRef
        p <- readTVarIO passwordVar
        failed <- readTVarIO workers.commandConnectionFailed
        pure $ WorkerRun received (reverse calls) p failed
  E.bracket worker killThread (const run)

-- | Run the idle worker with an MPD that replies to each connection and to
-- each @idle@ with the next outcome, while an action runs.
withIdleWorker
  :: [Either MpdError ()]
  -> Maybe T.Text
  -> (TQueue AppEvent -> S.IORef [String] -> Workers -> IO a)
  -> IO a
withIdleWorker outcomes firstPassword k = do
  outcomesRef <- S.newIORef outcomes
  callsRef <- S.newIORef []
  events <- newTQueueIO
  passwordVar <- newTVarIO firstPassword
  workers <- testWorkers events passwordVar
  let worker = forkIO . runEff . runScripted outcomesRef callsRef $ idleWorker workers
  E.bracket worker killThread $ \_ -> k events callsRef workers

-- | What the workers need, with a short retry delay.
testWorkers :: TQueue AppEvent -> TVar (Maybe T.Text) -> IO Workers
testWorkers events passwordVar = do
  requests <- newTQueueIO
  failed <- newTVarIO False
  pure
    Workers
      { emit = atomically . writeTQueue events
      , logLine = \_ -> pure ()
      , requests = requests
      , password = passwordVar
      , retryDelay = 0.01
      , commandConnectionFailed = failed
      }

-- | An MPD that replies to each command, each connection and each @idle@
-- with the next outcome. Without outcomes, it replies, and @idle@ waits
-- until it is stopped.
runScripted
  :: IOE :> es
  => S.IORef [Either MpdError ()]
  -> S.IORef [String]
  -> Eff (Mpd : es) a
  -> Eff es a
runScripted outcomesRef callsRef = interpret_ $ \case
  Connect p -> do
    call . unwords $ "connect" : map T.unpack (maybeToList p)
    either throwIO (const . pure $ Version 0 24 0) =<< nextOutcome
  RunCommand cmd -> do
    call . unwords $ "run" : [T.unpack r.command | r <- commandRequests cmd]
    nextOutcome >>= \case
      Left err -> throwIO err
      Right () -> either throwIO pure $ parseCommandReply cmd [[] | _ <- commandRequests cmd]
  WaitIdle interrupted ->
    nextOutcome >>= either throwIO (\() -> Nothing <$ liftIO (atomically interrupted))
  Disconnect -> call "disconnect"
  where
    call :: IOE :> es => String -> Eff es ()
    call c = liftIO $ S.modifyIORef callsRef (c :)

    nextOutcome :: IOE :> es => Eff es (Either MpdError ())
    nextOutcome = liftIO . S.atomicModifyIORef outcomesRef $ \case
      o : os -> (os, o)
      [] -> ([], Right ())
