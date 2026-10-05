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
  , emit :: AppEvent -> IO ()
  , logLine :: T.Text -> IO ()
  }

-- | Load the lyrics of each new request.
lyricsWorker :: LyricsSource -> IO ()
lyricsWorker src = go Nothing M.empty
  where
    -- The token of the request served last, and what the fetchers had for
    -- the songs whose lyrics they didn't find, by their files. A failure
    -- isn't remembered, so that a request can try again.
    go :: Maybe Int -> M.Map FilePath LyricsResult -> IO ()
    go served known = do
      (token, request) <-
        atomically $
          readTVar src.requested >>= \case
            Just (token, request) | Just token /= served -> pure (token, request)
            _ -> retry
      let file = lyricsFileName request.song
          known' = if request.refetch then M.delete file known else known
      stored <-
        if request.refetch then pure LyricsMissing else storedLyrics src.directory request.song
      result <- case stored of
        LyricsMissing
          | Just r <- M.lookup file known' -> pure r
          | not (null src.fetchers) -> do
              src.emit $ LyricsFetching token
              fetched <- fetchFrom Nothing src.fetchers request.song
              case fetched of
                LyricsFound _ lyrics -> store request.song lyrics
                _ -> pure ()
              pure fetched
        other -> pure other
      src.emit $ LyricsLoaded token result
      go (Just token) $ case result of
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
