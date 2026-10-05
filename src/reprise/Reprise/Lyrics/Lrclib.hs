{-# LANGUAGE DerivingVia #-}

-- | Lyrics from LRCLIB, lrclib.net: a free database of plain and timed
-- lyrics with a JSON API.
module Reprise.Lyrics.Lrclib
  ( LrclibGet
  , lrclibGet
  , lrclibLyrics
  , LrclibTrack (..)
  , chooseTrack
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types
import Yamlet

import Reprise.Lyrics
import Reprise.Mpd.Protocol.Types

-- | A GET of a path of LRCLIB's API with the parameters of its query: the
-- status and the body of the reply, or why there is none.
type LrclibGet =
  BS.ByteString -> [(T.Text, T.Text)] -> IO (Either T.Text (Int, BS.ByteString))

-- | A GET with a manager that speaks TLS, as LRCLIB is HTTPS only, and with
-- the user agent, by which LRCLIB asks clients to name themselves.
lrclibGet :: Manager -> T.Text -> LrclibGet
lrclibGet manager userAgent apiPath params = do
  let request =
        setQueryString [(T.encodeUtf8 k, Just (T.encodeUtf8 v)) | (k, v) <- params] $
          lrclibRequest
            { path = apiPath
            , requestHeaders = [(hUserAgent, T.encodeUtf8 userAgent)]
            , responseTimeout = responseTimeoutMicro lrclibTimeout
            }
  try (httpLbs request manager) >>= \case
    Right response ->
      pure $ Right (statusCode (responseStatus response), BL.toStrict (responseBody response))
    Left err -> pure . Left $ case err of
      HttpExceptionRequest _ ResponseTimeout -> "LRCLIB didn't answer in time"
      HttpExceptionRequest _ ConnectionTimeout -> "LRCLIB didn't answer in time"
      HttpExceptionRequest _ content -> "LRCLIB can't be reached: " <> T.pack (show content)
      InvalidUrlException url why -> "LRCLIB's URL " <> T.pack url <> " is invalid: " <> T.pack why
  where
    lrclibRequest :: Request
    lrclibRequest = defaultRequest {host = "lrclib.net", port = 443, secure = True}

-- | How long LRCLIB has to answer, in microseconds. It answered 30 requests
-- in 0.86 s at most on 2026-10-05. The worker serves one request at a time,
-- so a request that hangs holds up the next ones, and it gives up after
-- about ten times that.
lrclibTimeout :: Int
lrclibTimeout = 10 * 1000000

-- | The lyrics of a song from LRCLIB: by its artist, title, album and
-- length, else by a search of its artist and title, and if neither has it,
-- by its title without what follows in brackets.
lrclibLyrics :: LrclibGet -> Song -> IO LyricsResult
lrclibLyrics get song = case (firstTag Artist song, firstTag Title song) of
  (Just artist, Just title) -> do
    found <- lookupTitle artist title
    case found of
      Right Nothing
        | cleanTitle title /= title -> fromFound <$> lookupTitle artist (cleanTitle title)
      _ -> pure $ fromFound found
  _ -> pure LyricsMissing
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

    fromFound :: Either T.Text (Maybe LrclibTrack) -> LyricsResult
    fromFound = \case
      Left reason -> LyricsFailed reason
      Right Nothing -> LyricsMissing
      Right (Just track)
        | track.instrumental -> LyricsInstrumental
        | Just plain <- track.plainLyrics
        , not (T.null (T.strip plain)) ->
            LyricsFound (Fetched "LRCLIB") plain
        | otherwise -> LyricsMissing

    -- A busy server answers 503 with a message, e.g. "The server is busy,
    -- please retry in a moment".
    failure :: Either T.Text (Int, BS.ByteString) -> Either T.Text a
    failure = \case
      Left reason -> Left reason
      Right (status, body) -> Left $ case decode @LrclibError body of
        Right e -> "LRCLIB: " <> e.message
        Left _ -> "LRCLIB answered with the status " <> T.pack (show status)

    decoded :: FromYaml a => BS.ByteString -> Either T.Text a
    decoded = either (const (Left "LRCLIB's answer can't be read")) Right . decode

-- | A track of LRCLIB, of the keys that reprise reads.
data LrclibTrack = LrclibTrack
  { trackName :: T.Text
  , artistName :: T.Text
  , albumName :: T.Text
  , duration :: Double
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
