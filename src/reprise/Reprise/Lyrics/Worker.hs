-- | The thread that loads the lyrics that the lyrics screen asks for: the
-- stored ones, else the ones that the fetchers find, which it stores.
module Reprise.Lyrics.Worker
  ( LyricsSource (..)
  , lyricsWorker
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Functor
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory
import System.FilePath
import System.IO.Error

import Reprise.Event
import Reprise.Exception
import Reprise.File
import Reprise.Format
import Reprise.Lyrics
import Reprise.Mpd.Protocol.Types

data LyricsSource = LyricsSource
  { directory :: FilePath
  , fetchers :: [Fetcher]
  -- ^ Asked in order, until one has the lyrics.
  , requested :: TVar (Maybe (Int, LyricsRequest))
  -- ^ The newest request, with its token. The worker takes only the
  -- newest, so the songs that the screen passed by aren't loaded.
  , background :: TVar (Maybe Song)
  -- ^ The song whose lyrics to fetch and store without showing them, e.g.
  -- the one that plays. A request of the screen goes first.
  , emit :: AppEvent -> IO ()
  , logLine :: T.Text -> IO ()
  }

-- | Load the lyrics of each new request, and fetch the lyrics of each new
-- song in the background.
lyricsWorker :: LyricsSource -> IO ()
lyricsWorker src = go Nothing Nothing
  where
    -- The token of the request served last, and the song fetched in the
    -- background last.
    go :: Maybe Int -> Maybe Song -> IO ()
    go served fetched = do
      next <-
        atomically $
          ( readTVar src.requested >>= \case
              Just (token, request) | Just token /= served -> pure $ Left (token, request)
              _ -> retry
          )
            `orElse` ( readTVar src.background >>= \case
                         Just song | Just song /= fetched -> pure $ Right song
                         _ -> retry
                     )
      case next of
        Left (token, request) -> do
          result <- load (src.emit . LyricsFetching token) request.refetch request.song
          src.emit $ LyricsLoaded token result
          go (Just token) fetched
        Right song -> do
          result <- load (\_ -> pure ()) False song
          case result of
            LyricsMissing asked ->
              forM_ [(n, r) | (n, Just r) <- asked] $ \(fetcher, reason) ->
                src.logLine $
                  "The lyrics of "
                    <> songName song
                    <> " can't be fetched from "
                    <> fetcher
                    <> ": "
                    <> reason
            _ -> pure ()
          go served (Just song)

    -- The lyrics of a song: stored, or else fetched and stored, after an
    -- action. What the fetchers didn't have isn't remembered, as they may
    -- have it later, or the config may name other fetchers. A refetch that
    -- finds nothing keeps the stored lyrics.
    load :: (T.Text -> IO ()) -> Bool -> Song -> IO LyricsResult
    load fetching refetch song
      | refetch =
          fetchAndStore >>= \case
            LyricsMissing asked ->
              storedLyrics src.directory song <&> \case
                LyricsFound (Stored file) lyrics -> LyricsFound (Kept file asked) lyrics
                _ -> LyricsMissing asked
            other -> pure other
      | otherwise =
          storedLyrics src.directory song >>= \case
            LyricsMissing _ -> fetchAndStore
            other -> pure other
      where
        fetchAndStore :: IO LyricsResult
        fetchAndStore = do
          fetchedLyrics <- fetchFrom fetching [] src.fetchers song
          case fetchedLyrics of
            LyricsFound _ lyrics -> store song lyrics
            _ -> pure ()
          pure fetchedLyrics

    -- The lyrics show even if they can't be stored.
    store :: Song -> Lyrics -> IO ()
    store song lyrics = do
      stored <- try @IOException $ do
        createDirectoryIfMissing True src.directory
        writeLyrics (lyricsFileName song) lyrics.plain
        case lyrics.timed of
          Just timed -> writeLyrics (timedLyricsFileName song) timed.lrc
          -- The times stored before are of other lyrics.
          Nothing ->
            removeFile (src.directory </> timedLyricsFileName song) `catch` \err ->
              unless (isDoesNotExistError err) (throwIO err)
      either
        (src.logLine . ("The lyrics can't be stored: " <>) . exceptionText)
        pure
        stored

    writeLyrics :: FilePath -> T.Text -> IO ()
    writeLyrics file text =
      writeFileAtomically (src.directory </> file) (T.encodeUtf8 (T.stripEnd text <> "\n"))

-- | The stored lyrics of a song in the directory: timed if the times are
-- stored, else plain. LRC without times is plain too, e.g. lyrics pasted
-- into a new file of synced ones. A file that isn't UTF-8 shows, with its
-- bytes that aren't as replacement characters.
storedLyrics :: FilePath -> Song -> IO LyricsResult
storedLyrics dir song =
  readText timedFile >>= \case
    Right (Just lrc) | Just lyrics <- timedLyrics lrc -> pure $ LyricsFound (Stored timedFile) lyrics
    timedRead ->
      readText textFile <&> \case
        Right (Just text) -> LyricsFound (Stored textFile) (plainLyrics text)
        Right Nothing -> case timedRead of
          Right (Just lrc) -> LyricsFound (Stored timedFile) (plainLyrics lrc)
          Right Nothing -> LyricsMissing []
          Left err -> unreadable err
        Left err -> unreadable err
  where
    timedFile :: FilePath
    timedFile = timedLyricsFileName song

    textFile :: FilePath
    textFile = lyricsFileName song

    unreadable :: IOException -> LyricsResult
    unreadable err = LyricsFailed $ "The lyrics can't be read: " <> exceptionText err

    -- The text of a file, or Nothing without the file.
    readText :: FilePath -> IO (Either IOException (Maybe T.Text))
    readText file =
      try (BS.readFile (dir </> file)) <&> \case
        Right bytes ->
          Right . Just . T.stripEnd . T.replace "\r\n" "\n" $ T.decodeUtf8Lenient bytes
        Left err
          | isDoesNotExistError err -> Right Nothing
          | otherwise -> Left err

-- | The lyrics of the first fetcher that has them, after an action with the
-- name of each fetcher that is asked. Else the fetchers that were asked
-- before, in reverse, with why each that failed did.
fetchFrom
  :: (T.Text -> IO ())
  -> [(T.Text, Maybe T.Text)]
  -> [Fetcher]
  -> Song
  -> IO LyricsResult
fetchFrom fetching asked fetchers song = case fetchers of
  [] -> pure . LyricsMissing $ reverse asked
  fetcher : rest -> do
    fetching fetcher.name
    fetcher.fetch song >>= \case
      FetchedLyrics lyrics -> pure $ LyricsFound (Fetched fetcher.name) lyrics
      FetchedInstrumental -> pure LyricsInstrumental
      FetchedNothing -> fetchFrom fetching ((fetcher.name, Nothing) : asked) rest song
      FetchFailed reason -> fetchFrom fetching ((fetcher.name, Just reason) : asked) rest song
