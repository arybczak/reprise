{-# LANGUAGE DerivingVia #-}

-- | Lyrics from LRCLIB, lrclib.net: a free database of plain and timed
-- lyrics with a JSON API.
module Reprise.Lyrics.Lrclib
  ( lrclib
  , lrclibLyrics
  , LrclibTrack (..)
  , chooseTrack
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Text qualified as T
import GHC.Generics
import Yamlet

import Reprise.Lyrics
import Reprise.Lyrics.Http
import Reprise.Mpd.Protocol.Types

lrclib :: Get -> Fetcher
lrclib = Fetcher "LRCLIB" . lrclibLyrics

-- | The lyrics of a song from LRCLIB: by its artist, title, album and
-- length, else by a search of its artist and title, and if neither has it,
-- by its title without what follows in brackets.
lrclibLyrics :: Get -> Song -> IO FetchResult
lrclibLyrics get song = byArtistAndTitle song $ \artist title -> do
  found <- lookupTitle artist title
  case found of
    Right Nothing
      | cleanTitle title /= title -> fromFound <$> lookupTitle artist (cleanTitle title)
    _ -> pure $ fromFound found
  where
    lookupTitle :: T.Text -> T.Text -> IO (Either T.Text (Maybe LrclibTrack))
    lookupTitle artist title = do
      let named = [("artist_name", artist), ("track_name", title)]
          album = [("album_name", a) | Just a <- [firstTag Album song]]
          duration = [("duration", T.pack (show (round @_ @Int d))) | Just d <- [song.duration]]
      get "/api/get" (named <> album <> duration) >>= \case
        Right (200, body) -> pure $ Just <$> decoded body
        Right (404, _) ->
          get "/api/search" named >>= \case
            Right (200, body) -> pure $ chooseTrack song.duration <$> decoded body
            other -> pure $ failure other
        other -> pure $ failure other

    fromFound :: Either T.Text (Maybe LrclibTrack) -> FetchResult
    fromFound = \case
      Left reason -> FetchFailed reason
      Right Nothing -> FetchedNothing
      Right (Just track)
        | track.instrumental -> FetchedInstrumental
        | otherwise ->
            let timed = track.syncedLyrics >>= timedLyrics
                plain = mfilter (not . T.null . T.strip) track.plainLyrics
            in case (plain, timed) of
                 (Just text, _) -> FetchedLyrics $ Lyrics text ((.timed) =<< timed)
                 (Nothing, Just lyrics) -> FetchedLyrics lyrics
                 (Nothing, Nothing) -> FetchedNothing

    -- A busy server answers 503 with a message, e.g. "The server is busy,
    -- please retry in a moment".
    failure :: Either T.Text (Int, BS.ByteString) -> Either T.Text a
    failure = \case
      Left reason -> Left reason
      Right (status, body) -> Left $ case decode @LrclibError body of
        Right e -> "LRCLIB: " <> e.message
        Left _ -> answeredWith "LRCLIB" status

    decoded :: FromYaml a => BS.ByteString -> Either T.Text a
    decoded = either (const (Left "LRCLIB's answer can't be read")) Right . decode

-- | A track of LRCLIB, of the keys that reprise reads. Any other key may be
-- null, e.g. the name of the album.
data LrclibTrack = LrclibTrack
  { duration :: Double
  , instrumental :: Bool
  , plainLyrics :: Maybe T.Text
  , syncedLyrics :: Maybe T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml LrclibTrack

-- | LRCLIB adds keys, e.g. @lyricsfile@, which reprise doesn't read.
instance GenericYamlOptions LrclibTrack where
  yamlOptions = defaultYamlOptions {rejectUnknownFields = False}

-- | What LRCLIB answers instead of a track, e.g. @TrackNotFound@ with 404,
-- or @ServerOverloaded@ with 503.
data LrclibError = LrclibError
  { statusCode :: Int
  , name :: T.Text
  , message :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml LrclibError

instance GenericYamlOptions LrclibError where
  yamlOptions = defaultYamlOptions {rejectUnknownFields = False}

-- | The result of a search for a song: one within LRCLIB's tolerance of the
-- song's length, with lyrics rather than without, the first such. A song
-- without a length takes any.
chooseTrack :: Maybe Seconds -> [LrclibTrack] -> Maybe LrclibTrack
chooseTrack songLength tracks = listToMaybe $ filter hasLyrics fitting <> fitting
  where
    fitting :: [LrclibTrack]
    fitting = filter (\t -> maybe True (fits t) songLength) tracks

    fits :: LrclibTrack -> Seconds -> Bool
    fits t d = abs (realToFrac d - t.duration) <= durationTolerance

    hasLyrics :: LrclibTrack -> Bool
    hasLyrics t = isJust t.plainLyrics

-- | LRCLIB's @/api/get@ takes a track whose length is within 2 seconds of
-- the one asked for, as its documentation says.
durationTolerance :: Double
durationTolerance = 2
