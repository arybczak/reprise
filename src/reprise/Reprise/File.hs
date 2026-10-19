-- | Writing the files that reprise keeps, and naming files.
module Reprise.File
  ( writeFileAtomically
  , openLog

    -- * Names
  , fileName
  , forbiddenInName
  , nameLimit
  ) where

import Control.Concurrent.MVar.Strict qualified as S
import Control.Exception
import Data.Bits
import Data.ByteString qualified as BS
import Data.Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Word
import Numeric
import System.Directory
import System.FilePath
import System.IO
import System.Posix.IO
import System.Posix.Unistd

import Reprise.Exception

----------------------------------------
-- Names

-- | A name of a file from text, without the characters that
-- 'forbiddenInName' forbids, in at most a number of bytes of UTF-8. A longer name
-- is cut, at a character, and ends in a hash of the whole name, so that two
-- long names that begin alike stay apart.
fileName :: Int -> T.Text -> FilePath
fileName room text
  | utf8Length name <= room = T.unpack name
  | otherwise = T.unpack $ cut (room - utf8Length suffix) name <> suffix
  where
    name :: T.Text
    name = T.filter (not . forbiddenInName) text

    -- FNV-1a is the same on every machine and in every version, as a
    -- file's name must be. 64 bits keep two long names that begin alike
    -- apart, and leave most of the room to the name.
    suffix :: T.Text
    suffix = " " <> T.pack (showHex (fnv1a (T.encodeUtf8 name)) "")

    -- The offset basis and the prime are those of 64-bit FNV-1a.
    fnv1a :: BS.ByteString -> Word64
    fnv1a = BS.foldl' (\h b -> (h `xor` fromIntegral b) * 0x100000001b3) 0xcbf29ce484222325

    -- The longest start of text that is at most a number of bytes.
    cut :: Int -> T.Text -> T.Text
    cut n t = T.take (length (takeWhile (<= n) (scanl1 (+) (map charLength (T.unpack t))))) t

    utf8Length :: T.Text -> Int
    utf8Length = T.foldl' (\n c -> n + charLength c) 0

    charLength :: Char -> Int
    charLength c
      | ord c < 0x80 = 1
      | ord c < 0x800 = 2
      | ord c < 0x10000 = 3
      | otherwise = 4

-- | Whether a character can't be in a file's name that reprise makes. Those
-- that Windows forbids are left out, as ncmpcpp does with its default
-- @generate_win32_compatible_filenames@, so that the files can move to
-- such a file system. @/@ is one, so a name is never a path. So are control
-- characters, e.g. a newline of a tag that TagLib read, which would also
-- break the lines of MPD's replies that list the name.
forbiddenInName :: Char -> Bool
forbiddenInName c = isControl c || c `elem` ("\"*/:<>?\\|" :: String)

-- | The longest name of a file, in bytes of UTF-8: Linux's @NAME_MAX@, of
-- ext4, XFS, Btrfs and ZFS. Such a name also fits the 255 units of UTF-16
-- of NTFS and exFAT, and the 255 characters of APFS. A fixed limit, not the
-- one of the file system, keeps a name the same when its file moves to
-- another one.
nameLimit :: Int
nameLimit = 255

----------------------------------------
-- Writing

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
