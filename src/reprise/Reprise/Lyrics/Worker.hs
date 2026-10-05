-- | The thread that loads the lyrics that the lyrics screen asks for.
module Reprise.Lyrics.Worker
  ( LyricsSource (..)
  , lyricsWorker
  ) where

import Control.Concurrent.STM
import Control.Exception
import Data.ByteString qualified as BS
import Data.Functor
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.FilePath
import System.IO.Error

import Reprise.Event
import Reprise.Lyrics
import Reprise.Mpd.Protocol.Types

data LyricsSource = LyricsSource
  { directory :: FilePath
  , requested :: TVar (Maybe (Int, Song))
  -- ^ The newest request, with its token. The worker takes only the
  -- newest, so the songs that the screen passed by aren't loaded.
  , emit :: AppEvent -> IO ()
  }

-- | Load the lyrics of each new request.
lyricsWorker :: LyricsSource -> IO ()
lyricsWorker src = go Nothing
  where
    go :: Maybe Int -> IO ()
    go served = do
      (token, song) <-
        atomically $
          readTVar src.requested >>= \case
            Just (token, song) | Just token /= served -> pure (token, song)
            _ -> retry
      src.emit . LyricsLoaded token =<< storedLyrics src.directory song
      go (Just token)

-- | The lyrics of a song in the directory. A file that isn't UTF-8 shows,
-- with its bytes that aren't as replacement characters.
storedLyrics :: FilePath -> Song -> IO LyricsResult
storedLyrics dir song =
  try @IOException (BS.readFile (dir </> lyricsFileName song)) <&> \case
    Right bytes -> LyricsFound . T.stripEnd . T.replace "\r\n" "\n" $ T.decodeUtf8Lenient bytes
    Left err
      | isDoesNotExistError err -> LyricsMissing
      | otherwise ->
          LyricsFailed $ "The lyrics can't be read: " <> T.pack (displayException err)
