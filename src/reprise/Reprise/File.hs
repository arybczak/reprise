-- | Writing the files that reprise keeps.
module Reprise.File
  ( writeFileAtomically
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import System.Directory
import System.FilePath
import System.IO

-- | Replace a file whole: the bytes go to a temporary file next to it, which
-- then takes its name. A reader, e.g. another reprise, never sees half of
-- the file, and a write that fails leaves the old file. The file gets the
-- permissions of a new file, as 'BS.writeFile' gives.
writeFileAtomically :: FilePath -> BS.ByteString -> IO ()
writeFileAtomically path bytes =
  bracketOnError
    (openTempFileWithDefaultPermissions (takeDirectory path) (takeFileName path))
    (\(temp, h) -> hClose h >> removeFile temp)
    ( \(temp, h) -> do
        BS.hPut h bytes
        hClose h
        renameFile temp path
    )
