-- | The lyrics of songs, stored as ncmpcpp stores them: a text file for each
-- song in a directory, so that both read the lyrics of the other.
module Reprise.Lyrics
  ( LyricsResult (..)
  , lyricsName
  , lyricsFileName
  ) where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import System.FilePath

import Reprise.Mpd.Protocol.Types

-- | What the lyrics of a song came to.
data LyricsResult
  = LyricsFound T.Text
  | LyricsMissing
  | -- | Why they can't be had.
    LyricsFailed T.Text
  deriving stock (Eq, Show)

-- | What a song's lyrics are known by, as ncmpcpp names them: the first
-- artist and the first title, or without both, the name of the song's file
-- without its extension.
lyricsName :: Song -> T.Text
lyricsName song = case (firstTag Artist, firstTag Title) of
  (Just artist, Just title) -> artist <> " - " <> title
  _ -> T.pack . dropExtension . takeFileName $ T.unpack song.file
  where
    firstTag :: Tag -> Maybe T.Text
    firstTag t = mfilter (not . T.null) $ M.lookup t song.tags >>= listToMaybe

-- | The file of a song's lyrics in the directory of lyrics: its name
-- without the characters that Windows forbids in a file name, as ncmpcpp
-- with its default @generate_win32_compatible_filenames@ removes them.
lyricsFileName :: Song -> FilePath
lyricsFileName song = T.unpack (T.filter (`notElem` forbidden) (lyricsName song)) <> ".txt"
  where
    forbidden :: String
    forbidden = "\"*/:<>?\\|"
