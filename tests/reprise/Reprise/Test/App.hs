module Reprise.Test.App (appTests) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Function
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.App
import Reprise.Event
import Reprise.Exception

appTests :: TestTree
appTests =
  testGroup
    "app"
    [ testCase "the editor gets the file as an argument" test_runEditor
    , testCase "a signal interrupts the editor" test_signalDuringEditor
    , testCase "an action in the background" test_inBackground
    , testCase "renames only rename" test_renameAll
    ]

-- | An empty directory of the new name is replaced, as @rename@ does. One
-- with a file stops the renames, and nothing of either directory goes.
test_renameAll :: Assertion
test_renameAll = withSystemTempDirectory "rename" $ \dir -> do
  mapM_ (createDirectory . (dir </>)) ["a", "b", "empty", "full"]
  BS.writeFile (dir </> "full" </> "x") "x"
  BS.writeFile (dir </> "b" </> "y") "y"
  r <-
    renameAll
      [ (dir </> "a", dir </> "empty")
      , (dir </> "b", dir </> "full")
      , (dir </> "c", dir </> "d")
      ]
  case r of
    DirectoriesRenamed n failure -> do
      assertEqual "renamed" 1 n
      assertBool "why the next one wasn't" (isJust failure)
    other -> assertFailure $ "not the renames' event: " <> show other
  assertBool "over the empty directory" . not =<< doesDirectoryExist (dir </> "a")
  assertBool "the file in the full directory" =<< doesFileExist (dir </> "full" </> "x")
  assertBool "the directory that wasn't renamed" =<< doesFileExist (dir </> "b" </> "y")

-- | The event comes from another thread, also when the action fails, so
-- that a listing never waits for good.
test_inBackground :: Assertion
test_inBackground = do
  events <- newTBQueueIO 1
  let next =
        maybe (assertFailure "no event") pure
          =<< timeout 10000000 (atomically (readTBQueue events))
  release <- newEmptyMVar
  returned <-
    timeout 10000000 $
      inBackground events (MessageExpired 1 <$ takeMVar release) (const (MessageExpired 0))
  assertEqual "back before the action ends" (Just ()) returned
  putMVar release ()
  assertEqual "the event" (MessageExpired 1) =<< next
  inBackground events (throwIO (userError "gone")) (const (MessageExpired 2))
  assertEqual "the failure" (MessageExpired 2) =<< next

test_runEditor :: Assertion
test_runEditor = withSystemTempDirectory "editor" $ \dir -> do
  let file = dir </> "new" </> "$(touch pwned) `touch pwned` - A.txt"
  assertEqual "written" Nothing =<< runEditor "printf words >" file
  assertEqual "the file" "words" =<< BS.readFile file
  -- A file left behind would fail every later run.
  pwned <- doesFileExist "pwned"
  when pwned $ removeFile "pwned"
  assertBool "no shell code of the name" (not pwned)
  assertEqual "a failure" (Just "The editor exited with 3") =<< runEditor "exit 3;" file

-- | A signal ends reprise also while the editor runs, rather than passing
-- for the editor's failure.
test_signalDuringEditor :: Assertion
test_signalDuringEditor = withSystemTempDirectory "editor" $ \dir -> do
  let file = dir </> "lyrics.txt"
  result <- newEmptyMVar
  editing <-
    forkIO $ putMVar result =<< try @Signalled (runEditor "touch \"$1\"; sleep 5; true" file)
  started <- timeout 10000000 . fix $ \wait ->
    doesFileExist file >>= \case
      True -> pure ()
      False -> threadDelay 10000 >> wait
  assertEqual "the editor started" (Just ()) started
  throwTo editing (Signalled 15)
  assertEqual "the signal" (Just (Left (Signalled 15)))
    =<< timeout 10000000 (takeMVar result)
