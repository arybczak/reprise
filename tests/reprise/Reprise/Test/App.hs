module Reprise.Test.App (appTests) where

import Control.Concurrent.STM
import Control.Exception
import Data.ByteString qualified as BS
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.App
import Reprise.Event

appTests :: TestTree
appTests =
  testGroup
    "app"
    [ testCase "the editor gets the file as an argument" test_runEditor
    , testCase "an action in the background" test_inBackground
    ]

-- | The event comes from another thread, also when the action fails, so
-- that a listing never waits for good.
test_inBackground :: Assertion
test_inBackground = do
  events <- newTBQueueIO 1
  let next =
        maybe (assertFailure "no event") pure
          =<< timeout 10000000 (atomically (readTBQueue events))
  inBackground events (pure (MessageExpired 1)) (const (MessageExpired 0))
  assertEqual "the event" (MessageExpired 1) =<< next
  inBackground events (throwIO (userError "gone")) (const (MessageExpired 2))
  assertEqual "the failure" (MessageExpired 2) =<< next

test_runEditor :: Assertion
test_runEditor = withSystemTempDirectory "editor" $ \dir -> do
  let file = dir </> "new" </> "$(touch pwned) `touch pwned` - A.txt"
  assertEqual "written" Nothing =<< runEditor "printf words >" file
  assertEqual "the file" "words" =<< BS.readFile file
  assertBool "no shell code of the name" . not =<< doesFileExist "pwned"
  assertEqual "a failure" (Just "The editor exited with 3") =<< runEditor "exit 3;" file
