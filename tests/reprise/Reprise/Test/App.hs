module Reprise.Test.App (appTests) where

import Data.ByteString qualified as BS
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.App

appTests :: TestTree
appTests =
  testGroup
    "app"
    [ testCase "the editor gets the file as an argument" test_runEditor
    ]

test_runEditor :: Assertion
test_runEditor = withSystemTempDirectory "editor" $ \dir -> do
  let file = dir </> "new" </> "$(touch pwned) `touch pwned` - A.txt"
  assertEqual "written" Nothing =<< runEditor "printf words >" file
  assertEqual "the file" "words" =<< BS.readFile file
  assertBool "no shell code of the name" . not =<< doesFileExist "pwned"
  assertEqual "a failure" (Just "The editor exited with 3") =<< runEditor "exit 3;" file
