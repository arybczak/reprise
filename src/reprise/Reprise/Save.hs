-- | What a save puts in a stored playlist, from the queue or the browser.
module Reprise.Save
  ( SaveSource (..)
  , SaveItem (..)
  , songToSave
  , savedItems
  , partsLeftOut
  , withoutParts
  ) where

import Data.Maybe
import Data.Text qualified as T

import Reprise.Mpd.Protocol.Types

data SaveSource
  = -- | The whole queue, which MPD saves itself.
    SaveQueue
  | SaveItems [SaveItem]
  deriving stock (Eq, Show)

data SaveItem
  = SaveSong T.Text
  | -- | The songs of a directory of the database, with those of the
    -- directories in it.
    SaveDirectory T.Text
  | -- | The songs of a stored playlist, which the save fetches first.
    SavePlaylist T.Text
  | -- | A song that is a part of a file, e.g. a track of a cue sheet. A
    -- stored playlist would get the whole file, so the save leaves it out.
    SavePart
  deriving stock (Eq, Show)

-- | A song to save, or a part of a file, which can't be saved. MPD's
-- @playlistadd@ looks up a path without a scheme in its database, which
-- has no local file, so that goes as a @file://@ URI.
songToSave :: Song -> SaveItem
songToSave song
  | isJust song.range = SavePart
  | "/" `T.isPrefixOf` song.file = SaveSong ("file://" <> song.file)
  | otherwise = SaveSong song.file

-- | The items that a save saves: all but the parts of files.
savedItems :: [SaveItem] -> [SaveItem]
savedItems = filter (/= SavePart)

-- | The parts of files that a save leaves out.
partsLeftOut :: SaveSource -> Int
partsLeftOut = \case
  SaveQueue -> 0
  SaveItems items -> length (filter (== SavePart) items)

-- | The end of a message about songs that a number of parts of files were
-- left out of, e.g. as a stored playlist or an add would get the whole
-- file.
withoutParts :: Int -> T.Text
withoutParts = \case
  0 -> ""
  1 -> ", without 1 part of a file"
  n -> ", without " <> T.pack (show n) <> " parts of files"
