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
  , missingLines
  , plainLyrics
  , timedLyrics
  , parseLrc
  , lyricsFileName
  , timedLyricsFileName
  , cleanTitle
  , byArtistAndTitle
  ) where

import Control.Monad
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Reprise.File
import Reprise.Format
import Reprise.Mpd.Protocol.Response qualified as Response
import Reprise.Mpd.Protocol.Types
import Reprise.Number

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
  = -- | From the file with the name in the directory of lyrics.
    Stored FilePath
  | -- | From a fetcher with the name.
    Fetched T.Text
  | -- | From the file with the name, as the fetchers that were asked
    -- again had none: each with why it failed, if it did.
    Kept FilePath [(T.Text, Maybe T.Text)]
  deriving stock (Eq, Show)

-- | Why the fetchers that were asked had no lyrics, from 'LyricsMissing' or
-- 'Kept': a line for those that didn't have them, and the line of each
-- failure, which names its fetcher.
missingLines :: [(T.Text, Maybe T.Text)] -> [T.Text]
missingLines asked =
  [ "No lyrics found on " <> alternatives notThere
  | let notThere = [f | (f, Nothing) <- asked]
  , not (null notThere)
  ]
    <> [reason | (_, Just reason) <- asked]
  where
    -- E.g. @A, B or C@.
    alternatives :: [T.Text] -> T.Text
    alternatives names = case reverse names of
      lastName : rest@(_ : _) -> T.intercalate ", " (reverse rest) <> " or " <> lastName
      _ -> T.concat names

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
-- @[ar:Artist]@, isn't a line. Spaces before a line's time don't count.
--
-- An @[offset:+500]@ tag moves every time by its milliseconds: a positive
-- offset shows the lines sooner, as the format defines it. A time doesn't
-- move before the start of the song.
parseLrc :: T.Text -> [(Seconds, T.Text)]
parseLrc lrc =
  L.sortOn fst [(max 0 (t - offset), text) | l <- stripped, (t, text) <- timedLine l]
  where
    stripped :: [T.Text]
    stripped = map T.stripStart (T.lines lrc)

    offset :: Seconds
    offset = fromMaybe 0 . listToMaybe $ mapMaybe offsetTag stripped

    offsetTag :: T.Text -> Maybe Seconds
    offsetTag l = do
      rest <- T.stripPrefix "[offset:" l
      let (value, end) = T.breakOn "]" rest
      guard . not $ T.null end
      ms <- case T.uncons (T.strip value) of
        Just ('+', digits) -> decimal digits
        Just ('-', digits) -> negate <$> decimal digits
        _ -> decimal (T.strip value)
      pure $ fromInteger ms / 1000

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
          minutes <- decimal m
          seconds <- Response.readSeconds (T.encodeUtf8 s)
          pure $ fromInteger (minutes * 60) + seconds

-- | The file of a song's lyrics in the directory of lyrics, named after the
-- song, as ncmpcpp names it.
lyricsFileName :: Song -> FilePath
lyricsFileName song = lyricsBaseName song <> textExtension

-- | The file of a song's timed lyrics, next to the file of its lyrics.
-- ncmpcpp doesn't read it.
timedLyricsFileName :: Song -> FilePath
timedLyricsFileName song = lyricsBaseName song <> timedExtension

textExtension :: FilePath
textExtension = ".txt"

timedExtension :: FilePath
timedExtension = ".lrc"

-- | The name of a song's files without their extension, with room for
-- either extension. ncmpcpp can't store the lyrics of a song whose name is
-- too long for a file at all.
lyricsBaseName :: Song -> FilePath
lyricsBaseName song = fileName room (songName song)
  where
    room :: Int
    room = nameLimit - maximum (map length [textExtension, timedExtension])

-- | Fetch the lyrics of a song by its artist and its title. A song without
-- both has none to fetch.
byArtistAndTitle :: Song -> (T.Text -> T.Text -> IO FetchResult) -> IO FetchResult
byArtistAndTitle song fetch = case (firstTag Artist song, firstTag Title song) of
  (Just artist, Just title) -> fetch artist title
  _ -> pure FetchedNothing

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
