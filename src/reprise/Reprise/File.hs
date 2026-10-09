-- | Writing the files that reprise keeps.
module Reprise.File
  ( writeFileAtomically
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import System.Directory
import System.FilePath
import System.IO
import System.Posix.IO
import System.Posix.Unistd

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
