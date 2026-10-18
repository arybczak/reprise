module Reprise.Test.File (fileTests) where

import Control.Exception
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as T
import GHC.IO.Encoding
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Posix.IO
import System.Posix.IO.ByteString qualified as PB
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.File

fileTests :: TestTree
fileTests =
  testGroup
    "file"
    [ testCase "write a file whole" test_writeFileAtomically
    , testCase "write through a symbolic link" test_symlink
    , testCase "the log is in UTF-8" test_logUtf8
    , testCase "a line that can't be logged is lost" test_logFull
    ]

-- | The locale's encoding may lack characters of e.g. a song's name, such
-- as ASCII's with @LANG=C@.
test_logUtf8 :: Assertion
test_logUtf8 = withSystemTempDirectory "file" $ \dir -> do
  let path = dir </> "reprise.log"
  -- The encoding of the locale is global, so it changes only while the log
  -- opens, as a handle takes it then. Char8 cuts characters past Latin-1.
  logLine <-
    bracket getLocaleEncoding setLocaleEncoding $ \_ ->
      setLocaleEncoding char8 >> openLog path
  logLine "Zażółć"
  -- GHC locks the file of the log's handle, which stays open, against its
  -- handles in the same process.
  size <- getFileSize path
  logged <-
    bracket (openFd path ReadOnly defaultFileFlags) closeFd (`PB.fdRead` fromIntegral size)
  assertBool "the line" $ T.encodeUtf8 "Zażółć\n" `BS.isSuffixOf` logged

-- | Each worker logs its errors, also the error of its own log, so a log
-- that throws would end the worker.
test_logFull :: Assertion
test_logFull = do
  logLine <- openLog "/dev/full"
  logLine "lost"

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
