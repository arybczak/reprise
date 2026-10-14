-- | Writing the files that reprise keeps.
module Reprise.File
  ( writeFileAtomically
  , openLog
  ) where

import Control.Concurrent.MVar.Strict qualified as S
import Control.Exception
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import System.Directory
import System.FilePath
import System.IO
import System.Posix.IO
import System.Posix.Unistd

import Reprise.Exception

-- | Replace a file whole: the bytes go to a temporary file next to it, which
-- then takes its name. A reader, e.g. another reprise, never sees half of
-- the file, and a write that fails leaves the old file. The file gets the
-- permissions of a new file, as 'BS.writeFile' gives.
--
-- A symbolic link stays: the file that it points to gets the bytes, e.g. one
-- in a repository of dotfiles.
writeFileAtomically :: FilePath -> BS.ByteString -> IO ()
writeFileAtomically path bytes = do
  target <- canonicalizePath path
  bracketOnError
    (openTempFileWithDefaultPermissions (takeDirectory target) (takeFileName target))
    (\(temp, h) -> hClose h >> removeFile temp)
    ( \(temp, h) -> do
        BS.hPut h bytes
        hClose h
        -- XFS and Btrfs can write the rename before the bytes, so a crash
        -- between them would leave an empty file.
        bracket (openFd temp ReadOnly defaultFileFlags) closeFd fileSynchronise
        renameFile temp target
    )

-- | Open a log, which gets a line with its time for each call.
--
-- A line that can't be written, e.g. on a full disk, is lost: errors go to
-- the log, so an error of the log would have nowhere to go, and would stop
-- the workers. The log is in UTF-8, as the locale's encoding may lack
-- characters of e.g. a song's name.
openLog :: FilePath -> IO (T.Text -> IO ())
openLog file = do
  h <- openFile file AppendMode
  hSetEncoding h utf8
  hSetBuffering h LineBuffering
  lock <- S.newMVar ()
  pure $ \msg -> S.withMVar lock $ \() -> do
    time <- getZonedTime
    T.hPutStrLn h (T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z " time) <> msg)
      `catchSync` \_ -> pure ()
