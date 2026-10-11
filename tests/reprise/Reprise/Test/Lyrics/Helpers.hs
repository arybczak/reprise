-- | Helpers that several lyrics test modules use.
module Reprise.Test.Lyrics.Helpers
  ( stored
  ) where

import Data.Text qualified as T

import Reprise.Lyrics

-- | Plain lyrics stored for the song A - One.
stored :: T.Text -> LyricsResult
stored = LyricsFound (Stored "A - One.txt") . plainLyrics
