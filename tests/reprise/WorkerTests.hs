module WorkerTests (workerTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Monad
import Data.IORef
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
    , testCase "a failed command sends MpdFailed" test_failed
    , testCase "a broken connection is closed" test_broken
    ]

test_retryClosed :: Assertion
test_retryClosed = do
  (events, calls) <- runWorker [Left (ConnectionError Closed), Right ()] [stop]
  assertEqual "events" [MpdDone] events
  assertEqual "calls" ["run stop", "disconnect", "run stop"] calls

test_failed :: Assertion
test_failed = do
  let ack = Ack AckArg 0 "play" "Bad song index"
  (events, calls) <- runWorker [Left (AckError ack)] [play (Just 99)]
  assertEqual "events" [MpdFailed [Request "play" ["99"]] (AckError ack)] events
  assertEqual "the connection stays" ["run play"] calls

test_broken :: Assertion
test_broken = do
  let err = ConnectionError (Broken "reset")
  (events, calls) <- runWorker [Left err, Right ()] [stop, stop]
  assertEqual "events" [MpdFailed [Request "stop" []] err, MpdDone] events
  assertEqual "calls" ["run stop", "disconnect", "run stop"] calls

----------------------------------------
-- Helpers

-- | Run the command worker for the commands, with an MPD that replies with
-- the outcomes in turn. Returns the events and the calls of the effect.
runWorker :: [Either MpdError ()] -> [Command ()] -> IO ([AppEvent], [String])
runWorker outcomes commands = do
  outcomesRef <- newIORef outcomes
  callsRef <- newIORef []
  requests <- newTQueueIO
  events <- newTQueueIO
  let workers =
        Workers
          { emit = atomically . writeTQueue events
          , logLine = \_ -> pure ()
          , requests = requests
          }
  atomically $ mapM_ (writeTQueue requests . (`PendingRequest` const MpdDone)) commands
  let worker = forkIO . runEff . runScripted outcomesRef callsRef $ commandWorker workers
      collect = do
        received <- mapM (const . atomically $ readTQueue events) commands
        calls <- readIORef callsRef
        pure (received, reverse calls)
  E.bracket worker killThread (const collect)

-- | An MPD that replies to each command with the next outcome.
runScripted
  :: IOE :> es
  => IORef [Either MpdError ()]
  -> IORef [String]
  -> Eff (Mpd : es) a
  -> Eff es a
runScripted outcomesRef callsRef = interpret_ $ \case
  Connect -> pure (Version 0 24 0)
  RunCommand cmd -> do
    call . unwords $ "run" : [T.unpack r.command | r <- commandRequests cmd]
    outcome <- liftIO . atomicModifyIORef' outcomesRef $ \case
      o : os -> (os, o)
      [] -> ([], Right ())
    case outcome of
      Left err -> throwIO err
      Right () -> either throwIO pure $ parseCommandReply cmd [[] | _ <- commandRequests cmd]
  WaitIdle -> liftIO . forever $ threadDelay maxBound
  Disconnect -> call "disconnect"
  where
    call :: IOE :> es => String -> Eff es ()
    call c = liftIO $ modifyIORef' callsRef (c :)
