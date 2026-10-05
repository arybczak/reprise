-- | The thread that loads the lyrics that the lyrics screen asks for: the
-- stored ones, else the ones that the fetchers find, which it stores.
module Reprise.Lyrics.Worker
  ( LyricsSource (..)
  , lyricsWorker
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Functor
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error

import Reprise.Event
import Reprise.Lyrics
import Reprise.Mpd.Protocol.Types

data LyricsSource = LyricsSource
  { directory :: FilePath
  , fetchers :: [Song -> IO LyricsResult]
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
lyricsWorker src = go Nothing Nothing M.empty
  where
    -- The token of the request served last, the song fetched in the
    -- background last, and what the fetchers had for the songs whose
    -- lyrics they didn't find, by their files. A failure isn't remembered,
    -- so that a request can try again.
    go :: Maybe Int -> Maybe Song -> M.Map FilePath LyricsResult -> IO ()
    go served fetched known = do
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
          (result, known') <-
            load (src.emit (LyricsFetching token)) request.refetch request.song known
          src.emit $ LyricsLoaded token result
          go (Just token) fetched known'
        Right song -> do
          (result, known') <- load (pure ()) False song known
          case result of
            LyricsFailed reason ->
              src.logLine $ "The lyrics of " <> lyricsName song <> " can't be fetched: " <> reason
            _ -> pure ()
          go served (Just song) known'

    -- The lyrics of a song: stored, or what the fetchers had before, or
    -- else fetched and stored, after an action. Also what is remembered
    -- after.
    load
      :: IO ()
      -> Bool
      -> Song
      -> M.Map FilePath LyricsResult
      -> IO (LyricsResult, M.Map FilePath LyricsResult)
    load fetching refetch song known = do
      let file = lyricsFileName song
          known' = if refetch then M.delete file known else known
      stored <- if refetch then pure LyricsMissing else storedLyrics src.directory song
      result <- case stored of
        LyricsMissing
          | Just r <- M.lookup file known' -> pure r
          | not (null src.fetchers) -> do
              fetching
              fetchedLyrics <- fetchFrom Nothing src.fetchers song
              case fetchedLyrics of
                LyricsFound _ lyrics -> store song lyrics
                _ -> pure ()
              pure fetchedLyrics
        other -> pure other
      pure . (result,) $ case result of
        LyricsMissing -> M.insert file result known'
        LyricsInstrumental -> M.insert file result known'
        _ -> known'

    -- The lyrics of the first fetcher that has them, or else the first
    -- failure, or else that none has them.
    fetchFrom :: Maybe T.Text -> [Song -> IO LyricsResult] -> Song -> IO LyricsResult
    fetchFrom failed fetchers song = case fetchers of
      [] -> pure $ maybe LyricsMissing LyricsFailed failed
      fetcher : rest ->
        fetcher song >>= \case
          LyricsMissing -> fetchFrom failed rest song
          LyricsFailed reason -> fetchFrom (failed <|> Just reason) rest song
          found -> pure found

    -- The lyrics show even if they can't be stored.
    store :: Song -> Lyrics -> IO ()
    store song lyrics = do
      stored <- try @IOException $ do
        createDirectoryIfMissing True src.directory
        writeWhole (lyricsFileName song) lyrics.plain
        case lyrics.timed of
          Just timed -> writeWhole (timedLyricsFileName song) timed.lrc
          -- The times stored before are of other lyrics.
          Nothing ->
            removeFile (src.directory </> timedLyricsFileName song) `catch` \err ->
              unless (isDoesNotExistError err) (throwIO err)
      either
        (src.logLine . ("The lyrics can't be stored: " <>) . T.pack . displayException)
        pure
        stored

    -- Through a temporary file, so that a file is never half written.
    writeWhole :: FilePath -> T.Text -> IO ()
    writeWhole file text = do
      (temporary, h) <- openBinaryTempFileWithDefaultPermissions src.directory (file <.> "part")
      BS.hPut h (T.encodeUtf8 (T.stripEnd text <> "\n")) `finally` hClose h
      renameFile temporary (src.directory </> file)

-- | The stored lyrics of a song in the directory: timed if the times are
-- stored, else plain. A file that isn't UTF-8 shows, with its bytes that
-- aren't as replacement characters.
storedLyrics :: FilePath -> Song -> IO LyricsResult
storedLyrics dir song =
  readText (timedLyricsFileName song) >>= \case
    Right (Just lrc) | Just lyrics <- timedLyrics lrc -> pure $ LyricsFound Stored lyrics
    _ ->
      readText (lyricsFileName song) <&> \case
        Right (Just text) -> LyricsFound Stored (plainLyrics text)
        Right Nothing -> LyricsMissing
        Left err -> LyricsFailed $ "The lyrics can't be read: " <> T.pack (displayException err)
  where
    -- The text of a file, or Nothing without the file.
    readText :: FilePath -> IO (Either IOException (Maybe T.Text))
    readText file =
      try (BS.readFile (dir </> file)) <&> \case
        Right bytes ->
          Right . Just . T.stripEnd . T.replace "\r\n" "\n" $ T.decodeUtf8Lenient bytes
        Left err
          | isDoesNotExistError err -> Right Nothing
          | otherwise -> Left err
