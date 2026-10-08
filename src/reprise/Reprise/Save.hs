-- | What a save puts in a stored playlist, from the queue or the browser.
module Reprise.Save
  ( SaveSource (..)
  , SaveItem (..)
  , songToSave
  , savedItems
  , partsLeftOut
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

-- | A song to save, or a part of a file, which can't be saved.
songToSave :: Song -> SaveItem
songToSave song
  | isJust song.range = SavePart
  | otherwise = SaveSong song.file

-- | The items that a save saves: all but the parts of files.
savedItems :: [SaveItem] -> [SaveItem]
savedItems = filter (/= SavePart)

-- | The parts of files that a save leaves out.
partsLeftOut :: SaveSource -> Int
partsLeftOut = \case
  SaveQueue -> 0
  SaveItems items -> length (filter (== SavePart) items)
