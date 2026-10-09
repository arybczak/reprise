module FileTests (fileTests) where

import Data.ByteString qualified as BS
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.File

fileTests :: TestTree
fileTests =
  testGroup
    "File"
    [ testCase "write a file whole" test_writeFileAtomically
    , testCase "write through a symbolic link" test_symlink
    ]

-- | A link to a file, e.g. of a repository of dotfiles, stays a link, and
-- the file that it points to gets the bytes.
test_symlink :: Assertion
test_symlink = withSystemTempDirectory "file" $ \dir -> do
  createDirectory (dir </> "dotfiles")
  let target = dir </> "dotfiles" </> "history"
      link = dir </> "history"
  BS.writeFile target "old"
  createFileLink ("dotfiles" </> "history") link
  writeFileAtomically link "new"
  assertBool "still a link" =<< pathIsSymbolicLink link
  assertEqual "the file that it points to" "new" =<< BS.readFile target

test_writeFileAtomically :: Assertion
test_writeFileAtomically = withSystemTempDirectory "file" $ \dir -> do
  let path = dir </> "lyrics.txt"
      plain = dir </> "plain.txt"
  writeFileAtomically path "first"
  writeFileAtomically path "second"
  assertEqual "replaced" "second" =<< BS.readFile path
  assertEqual "no temporary file left" ["lyrics.txt"] =<< listDirectory dir
  BS.writeFile plain "plain"
  expected <- getPermissions plain
  assertEqual "the permissions of a new file" expected =<< getPermissions path
