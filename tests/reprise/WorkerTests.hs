module WorkerTests (workerTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Monad
import Data.IORef.Strict qualified as S
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Effect.Mpd
import Reprise.Effect.MpdRequest
import Reprise.Event
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Mpd.Worker

workerTests :: TestTree
workerTests =
  testGroup
    "Worker"
    [ testCase "a command runs again after MPD closed the connection" test_retryClosed
    , testCase "a failed command sends its failure event" test_failed
    , testCase "a broken connection is closed" test_broken
    , testCase "a refused command runs again with the password" test_password
    , testCase "a wrong password asks again" test_wrongPassword
    , testCase "the requests after a refused one wait for it" test_passwordHolds
    , testCase "a cancelled password fails the refused command" test_passwordCancelled
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

test_broken :: Assertion
test_broken = do
  let err = ConnectionError (Broken "reset")
  r <- runWorker [Left err, Right ()] [stop, stop] []
  assertEqual "events" [MpdFailed [Request "stop" []] err, MpdDone] r.events
  assertEqual "calls" ["run stop", "disconnect", "run stop"] r.calls

test_password :: Assertion
test_password = do
  r <- runWorker [Left refusal, Right (), Right ()] [stop] [Just "secret"]
  assertEqual "events" [PasswordNeeded refusal, MpdDone] r.events
  assertEqual "calls" ["run stop", "connect", "run stop"] r.calls
  assertEqual "the password for the next connections" (Just "secret") r.password

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
  assertEqual "calls" ["run stop", "connect", "run stop", "run pause"] r.calls

test_passwordCancelled :: Assertion
test_passwordCancelled = do
  r <- runWorker [Left refusal] [stop, pause True] [Nothing]
  assertEqual
    "events"
    [PasswordNeeded refusal, MpdFailed [Request "stop" []] refusal, MpdDone]
    r.events
  assertEqual "calls" ["run stop", "run pause"] r.calls

----------------------------------------
-- Helpers

refusal :: MpdError
refusal = AckError $ Ack AckPermission 0 "stop" "you don't have permission for \"stop\""

wrongPassword :: MpdError
wrongPassword = AckError $ Ack AckPassword 0 "password" "incorrect password"

data WorkerRun = WorkerRun
  { events :: [AppEvent]
  , calls :: [String]
  , password :: Maybe T.Text
  -- ^ The password for the next connections.
  }

-- | Run the command worker for the commands, with an MPD that replies to
-- the commands and the connections with the outcomes in turn. Each
-- 'PasswordNeeded' gets the next answer.
runWorker :: [Either MpdError ()] -> [Command ()] -> [Maybe T.Text] -> IO WorkerRun
runWorker outcomes commands answers = do
  outcomesRef <- S.newIORef outcomes
  callsRef <- S.newIORef []
  requests <- newTQueueIO
  events <- newTQueueIO
  passwordVar <- newTVarIO Nothing
  let workers =
        Workers
          { emit = atomically . writeTQueue events
          , logLine = \_ -> pure ()
          , requests = requests
          , password = passwordVar
          }
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
            e <- atomically $ readTQueue events
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
        pure $ WorkerRun received (reverse calls) p
  E.bracket worker killThread (const run)

-- | An MPD that replies to each command and each connection with the next
-- outcome.
runScripted
  :: IOE :> es
  => S.IORef [Either MpdError ()]
  -> S.IORef [String]
  -> Eff (Mpd : es) a
  -> Eff es a
runScripted outcomesRef callsRef = interpret_ $ \case
  Connect -> do
    call "connect"
    either throwIO (const . pure $ Version 0 24 0) =<< nextOutcome
  RunCommand cmd -> do
    call . unwords $ "run" : [T.unpack r.command | r <- commandRequests cmd]
    nextOutcome >>= \case
      Left err -> throwIO err
      Right () -> either throwIO pure $ parseCommandReply cmd [[] | _ <- commandRequests cmd]
  WaitIdle -> liftIO . forever $ threadDelay maxBound
  Disconnect -> call "disconnect"
  where
    call :: IOE :> es => String -> Eff es ()
    call c = liftIO $ S.modifyIORef callsRef (c :)

    nextOutcome :: IOE :> es => Eff es (Either MpdError ())
    nextOutcome = liftIO . S.atomicModifyIORef outcomesRef $ \case
      o : os -> (os, o)
      [] -> ([], Right ())
