-- | Groups of neighbouring items: with the same key, e.g. the songs of an
-- album, which the moves to the previous and the next album go between, or
-- at consecutive positions, which one command can change.
module Reprise.Groups
  ( -- * Keys
    artistKey
  , albumKey

    -- * Moves
  , nextGroup
  , previousGroup

    -- * Positions
  , runs
  , runRange
  ) where

import Control.Applicative
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T

import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Types

-- | What tells artists apart: the album artist, or the artist without one,
-- so that a compilation whose songs have different artists is one artist.
artistKey :: Song -> Maybe [T.Text]
artistKey song = M.lookup AlbumArtist song.tags <|> M.lookup Artist song.tags

-- | What tells albums apart: the artist and the album. The album alone
-- would join albums of different artists with the same name, e.g. two
-- greatest hits next to each other.
albumKey :: Song -> (Maybe [T.Text], Maybe [T.Text])
albumKey song = (artistKey song, M.lookup Album song.tags)

-- | The first item after the group of the item at the index, in a list of
-- items of a number, by the keys of their indices.
nextGroup :: Eq k => (Int -> k) -> Int -> Int -> Int
nextGroup key n c
  | c < 0 || c >= n = c
  | otherwise = fromMaybe (n - 1) $ L.find ((/= key c) . key) [c + 1 .. n - 1]

-- | The first item of the group of the item at the index, or of the group
-- before it if the item is already the first.
previousGroup :: Eq k => (Int -> k) -> Int -> Int -> Int
previousGroup key n c
  | c <= 0 = 0
  | otherwise =
      let s = start c
      in if s < c then s else start (c - 1)
  where
    start :: Int -> Int
    start i
      | i >= n = i
      | otherwise = maybe 0 (+ 1) $ L.find ((/= key i) . key) [i - 1, i - 2 .. 0]

-- | The runs of consecutive positions, each as its first and its last
-- position, in order. The positions must be ascending and distinct.
runs :: [Int] -> [(Int, Int)]
runs = foldr extend []
  where
    extend :: Int -> [(Int, Int)] -> [(Int, Int)]
    extend p = \case
      (a, b) : rest | p + 1 == a -> (p, b) : rest
      acc -> (p, p) : acc

-- | MPD's range of a run, which ends past the run's last position.
runRange :: (Int, Int) -> Range
runRange (a, b) = Range (SongPos a) (Just (SongPos (b + 1)))
