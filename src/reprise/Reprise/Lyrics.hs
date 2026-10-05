-- | The lyrics of songs, stored as ncmpcpp stores them: a text file for each
-- song in a directory, so that both read the lyrics of the other. The times
-- of their lines, which ncmpcpp has no use for, are in a file next to it.
module Reprise.Lyrics
  ( LyricsRequest (..)
  , LyricsResult (..)
  , LyricsOrigin (..)
  , Fetcher (..)
  , FetchResult (..)
  , Lyrics (..)
  , TimedLyrics (..)
  , plainLyrics
  , timedLyrics
  , parseLrc
  , lyricsName
  , lyricsFileName
  , timedLyricsFileName
  , sameSong
  , firstTag
  , cleanTitle
  ) where

import Control.Monad
import Data.Char
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ratio
import Data.Text qualified as T
import System.FilePath
import Text.Read

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
  = LyricsFound LyricsOrigin Lyrics
  | -- | The song has no words.
    LyricsInstrumental
  | -- | Neither stored nor fetched: the fetchers that were asked, in order,
    -- with why each that failed did.
    LyricsMissing [(T.Text, Maybe T.Text)]
  | -- | Why the stored lyrics can't be read.
    LyricsFailed T.Text
  deriving stock (Eq, Show)

-- | Where lyrics that aren't stored are fetched from.
data Fetcher = Fetcher
  { name :: T.Text
  -- ^ For the screen, e.g. @LRCLIB@.
  , fetch :: Song -> IO FetchResult
  }

-- | What a fetcher had for a song.
data FetchResult
  = FetchedLyrics Lyrics
  | -- | The song has no words.
    FetchedInstrumental
  | FetchedNothing
  | -- | Why it can't fetch now.
    FetchFailed T.Text
  deriving stock (Eq, Show)

data LyricsOrigin
  = Stored
  | -- | From a fetcher with the name.
    Fetched T.Text
  deriving stock (Eq, Show)

-- | The text of lyrics, and the times of its lines if they are known.
data Lyrics = Lyrics
  { plain :: T.Text
  , timed :: Maybe TimedLyrics
  }
  deriving stock (Eq, Show)

-- | Lyrics whose lines have the times when they are sung.
data TimedLyrics = TimedLyrics
  { lrc :: T.Text
  -- ^ As LRC, in which they are stored.
  , entries :: [(Seconds, T.Text)]
  -- ^ The lines by their times.
  }
  deriving stock (Eq, Show)

plainLyrics :: T.Text -> Lyrics
plainLyrics text = Lyrics text Nothing

-- | Lyrics from LRC, unless no line has a time. Their text is the lines
-- without the times.
timedLyrics :: T.Text -> Maybe Lyrics
timedLyrics lrc = case parseLrc lrc of
  [] -> Nothing
  entries -> Just $ Lyrics (T.unlines (map snd entries)) (Just (TimedLyrics lrc entries))

-- | The lines of LRC by their times, e.g. @[01:23.45]A line@. A line with
-- several times is sung at each, and a tag without a time, e.g.
-- @[ar:Artist]@, isn't a line.
parseLrc :: T.Text -> [(Seconds, T.Text)]
parseLrc = L.sortOn fst . concatMap timedLine . T.lines
  where
    timedLine :: T.Text -> [(Seconds, T.Text)]
    timedLine l = let (times, text) = tagged l in [(t, T.strip text) | t <- times]

    tagged :: T.Text -> ([Seconds], T.Text)
    tagged l = case T.stripPrefix "[" l of
      Just rest
        | (tag, end) <- T.breakOn "]" rest
        , not (T.null end)
        , Just t <- time tag ->
            let (times, text) = tagged (T.drop 1 end) in (t : times, text)
      _ -> ([], l)

    -- Minutes and seconds, with a fraction after a point or a colon.
    time :: T.Text -> Maybe Seconds
    time tag = case T.splitOn ":" tag of
      [m, s] -> at m s
      [m, s, fraction] -> at m (s <> "." <> fraction)
      _ -> Nothing
      where
        at :: T.Text -> T.Text -> Maybe Seconds
        at m s = do
          let (whole, fraction) = T.breakOn "." s
              digits = T.drop 1 fraction
          guard $ T.all isDigit (m <> whole <> digits)
          minutes <- readMaybe @Integer (T.unpack m)
          seconds <- readMaybe @Integer (T.unpack whole)
          part <-
            if T.null digits
              then Just 0
              else (% (10 ^ T.length digits)) <$> readMaybe @Integer (T.unpack digits)
          pure . fromRational $ fromInteger (minutes * 60 + seconds) + part

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
lyricsFileName song = lyricsBaseName song <> ".txt"

-- | The file of a song's timed lyrics, next to the file of its lyrics.
-- ncmpcpp doesn't read it.
timedLyricsFileName :: Song -> FilePath
timedLyricsFileName song = lyricsBaseName song <> ".lrc"

lyricsBaseName :: Song -> FilePath
lyricsBaseName song = T.unpack $ T.filter (`notElem` forbidden) (lyricsName song)
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

-- | Whether two songs are the same file, or the same part of a file, e.g.
-- in the queue and in the database.
sameSong :: Song -> Song -> Bool
sameSong a b = a.file == b.file && a.range == b.range
