-- | The lyrics of songs, stored as ncmpcpp stores them: a text file for each
-- song in a directory, so that both read the lyrics of the other.
module Reprise.Lyrics
  ( LyricsRequest (..)
  , LyricsResult (..)
  , LyricsOrigin (..)
  , lyricsName
  , lyricsFileName
  , firstTag
  , cleanTitle
  ) where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import System.FilePath

import Reprise.Mpd.Protocol.Types

-- | What the lyrics screen asks the worker for.
data LyricsRequest = LyricsRequest
  { song :: Song
  , refetch :: Bool
  -- ^ Fetch the lyrics even if they are stored, and store them anew.
  }
  deriving stock (Eq, Show)

-- | What the lyrics of a song came to.
data LyricsResult
  = LyricsFound LyricsOrigin T.Text
  | -- | The song has no words.
    LyricsInstrumental
  | LyricsMissing
  | -- | Why they can't be had now.
    LyricsFailed T.Text
  deriving stock (Eq, Show)

data LyricsOrigin
  = Stored
  | -- | From a fetcher with the name.
    Fetched T.Text
  deriving stock (Eq, Show)

-- | What a song's lyrics are known by, as ncmpcpp names them: the first
-- artist and the first title, or without both, the name of the song's file
-- without its extension.
lyricsName :: Song -> T.Text
lyricsName song = case (firstTag Artist song, firstTag Title song) of
  (Just artist, Just title) -> artist <> " - " <> title
  _ -> T.pack . dropExtension . takeFileName $ T.unpack song.file

-- | The file of a song's lyrics in the directory of lyrics: its name
-- without the characters that Windows forbids in a file name, as ncmpcpp
-- with its default @generate_win32_compatible_filenames@ removes them.
lyricsFileName :: Song -> FilePath
lyricsFileName song = T.unpack (T.filter (`notElem` forbidden) (lyricsName song)) <> ".txt"
  where
    forbidden :: String
    forbidden = "\"*/:<>?\\|"

-- | The first value of a tag of a song, unless it is empty.
firstTag :: Tag -> Song -> Maybe T.Text
firstTag t song = mfilter (not . T.null) $ M.lookup t song.tags >>= listToMaybe

-- | A title without what follows it in brackets, e.g. @(Bonus Track)@ or
-- @[Film Score]@, which a database of lyrics doesn't have. A title that is
-- all brackets stays.
cleanTitle :: T.Text -> T.Text
cleanTitle title = case T.unsnoc stripped of
  Just (rest, close)
    | Just open <- lookup close [(')', '('), (']', '[')]
    , Just before <- opening open close 1 (reverse (T.unpack rest))
    , kept <- T.stripEnd (T.pack (reverse before))
    , not (T.null kept) ->
        cleanTitle kept
  _ -> stripped
  where
    stripped :: T.Text
    stripped = T.stripEnd title

    -- What is before the bracket that a closing one closes, of text
    -- reversed from the closing one, at a depth of nested brackets.
    opening :: Char -> Char -> Int -> String -> Maybe String
    opening open close depth = \case
      [] -> Nothing
      c : cs
        | c == open && depth == 1 -> Just cs
        | c == open -> opening open close (depth - 1) cs
        | c == close -> opening open close (depth + 1) cs
        | otherwise -> opening open close depth cs
