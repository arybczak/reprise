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
  ) where

import Control.Applicative
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Text qualified as T

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

-- | The first item after the group of the item at the index.
nextGroup :: Eq k => (a -> k) -> Seq.Seq a -> Int -> Int
nextGroup key items c = case Seq.lookup c items of
  Nothing -> c
  Just item ->
    maybe (Seq.length items - 1) (+ (c + 1)) $
      Seq.findIndexL ((/= key item) . key) (Seq.drop (c + 1) items)

-- | The first item of the group of the item at the index, or of the group
-- before it if the item is already the first.
previousGroup :: Eq k => (a -> k) -> Seq.Seq a -> Int -> Int
previousGroup key items c
  | c <= 0 = 0
  | otherwise =
      let start i = case Seq.lookup i items of
            Nothing -> i
            Just item -> maybe 0 (+ 1) $ Seq.findIndexR ((/= key item) . key) (Seq.take i items)
          s = start c
      in if s < c then s else start (c - 1)

-- | The runs of consecutive positions, each as its first and its last
-- position, in order. The positions must be ascending and distinct.
runs :: [Int] -> [(Int, Int)]
runs = foldr extend []
  where
    extend :: Int -> [(Int, Int)] -> [(Int, Int)]
    extend p = \case
      (a, b) : rest | p + 1 == a -> (p, b) : rest
      acc -> (p, p) : acc
