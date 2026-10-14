-- | The directories of the local file system, which the browser lists
-- itself, as MPD's @listfiles@ leaves out symbolic links, and the tags of
-- their files, which TagLib reads, as MPD's FFmpeg plugin takes long for
-- videos.
module Reprise.LocalFiles
  ( -- * Music
    Suffixes (..)
  , suffixesOf
  , isMusic
  , mpdTags

    -- * Directories
  , LocalFile (..)
  , ListingError (..)
  , listLocalDirectory
  ) where

import Control.Applicative
import Control.Exception
import Data.Either
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath
import System.IO.Error
import System.Posix.Files

import Reprise.Exception
import Reprise.Mpd.Protocol.Types
import Reprise.TagLib

----------------------------------------
-- Music

-- | The suffixes of the files that MPD decodes, in lower case.
data Suffixes = Suffixes
  { trusted :: S.Set T.Text
  -- ^ Of the plugins other than FFmpeg's, which list the formats that they
  -- play.
  , ffmpegOnly :: S.Set T.Text
  -- ^ Only of FFmpeg's plugin, which since MPD 0.24 lists all of FFmpeg's
  -- formats, also ones that aren't audio, e.g. @txt@.
  }
  deriving stock (Eq, Show)

suffixesOf :: [Decoder] -> Suffixes
suffixesOf ds =
  let trusted = suffixes (/= "ffmpeg")
  in Suffixes {trusted = trusted, ffmpegOnly = suffixes (== "ffmpeg") `S.difference` trusted}
  where
    suffixes :: (T.Text -> Bool) -> S.Set T.Text
    suffixes plugin = S.fromList [T.toLower s | d <- ds, plugin d.plugin, s <- d.suffixes]

-- | Whether a file is music: MPD decodes its suffix, and if only FFmpeg's
-- plugin does, TagLib reads the file.
isMusic :: Suffixes -> T.Text -> Maybe FileTags -> Bool
isMusic s name tags = case suffixOf name of
  Just suffix
    | suffix `S.member` s.trusted -> True
    | suffix `S.member` s.ffmpegOnly -> isJust tags
  _ -> False

-- | Whether MPD decodes a file's suffix, so that its tags are read.
decodes :: Suffixes -> T.Text -> Bool
decodes s name = case suffixOf name of
  Just suffix -> suffix `S.member` s.trusted || suffix `S.member` s.ffmpegOnly
  Nothing -> False

suffixOf :: T.Text -> Maybe T.Text
suffixOf name = case T.breakOnEnd "." name of
  (before, suffix) | not (T.null before) -> Just (T.toLower suffix)
  _ -> Nothing

-- | The tags of a file as MPD names them. TagLib's names are MPD's up to
-- case, except for those of Vorbis comments that MPD maps in its
-- @src/lib/xiph/XiphTags.cxx@.
mpdTags :: FileTags -> M.Map Tag [T.Text]
mpdTags tags =
  M.fromListWith
    (flip (<>))
    [(t, values) | (key, values) <- tags.properties, Just t <- [tagOf key]]
  where
    tagOf :: T.Text -> Maybe Tag
    tagOf key = tagFromName key <|> M.lookup (T.toCaseFold key) xiphTags

    xiphTags :: M.Map T.Text Tag
    xiphTags =
      M.fromList
        [ ("tracknumber", Track)
        , ("discnumber", Disc)
        , ("description", Comment)
        , ("movementname", Movement)
        , ("movement", MovementNumber)
        ]

----------------------------------------
-- Directories

-- | An entry of a directory, after its symbolic link if it is one.
data LocalFile = LocalFile
  { name :: T.Text
  , isDirectory :: Bool
  , lastModified :: UTCTime
  , tags :: Maybe FileTags
  -- ^ Of a file whose suffix MPD decodes, if TagLib reads it.
  }
  deriving stock (Eq, Show)

data ListingError
  = DirectoryMissing
  | -- | Why the directory can't be read.
    DirectoryUnreadable T.Text
  deriving stock (Eq, Show)

-- | The directories and the regular files of a directory, by name. Another
-- kind of file is left out, e.g. a FIFO, whose open would wait for a writer,
-- and so is an entry that can't be read, e.g. a broken symbolic link.
listLocalDirectory :: Suffixes -> FilePath -> IO (Either ListingError [LocalFile])
listLocalDirectory suffixes dir =
  try (listDirectory dir) >>= \case
    Left err
      | isDoesNotExistError err -> pure (Left DirectoryMissing)
      | otherwise -> pure . Left . DirectoryUnreadable $ exceptionText err
    Right names ->
      Right . catMaybes . rights <$> traverse (try @IOException . entry) (L.sort names)
  where
    entry :: FilePath -> IO (Maybe LocalFile)
    entry name = do
      let path = dir </> name
      status <- getFileStatus path
      tags <-
        if isRegularFile status && decodes suffixes (T.pack name)
          then readFileTags path
          else pure Nothing
      pure $
        if isDirectory status || isRegularFile status
          then
            Just
              LocalFile
                { name = T.pack name
                , isDirectory = isDirectory status
                , lastModified = posixSecondsToUTCTime (modificationTimeHiRes status)
                , tags = tags
                }
          else Nothing
