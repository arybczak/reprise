-- | MPD commands that act on several songs of the queue, planned from their
-- positions. Each is one command list.
module Reprise.Screen.Queue.Edits
  ( runs
  , deletePositions
  , moveUp
  , moveDown
  , moveBefore
  ) where

import Control.Monad
import Data.Foldable
import Data.List.NonEmpty qualified as NE

import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Types

-- | The runs of consecutive positions, each as its first and its last
-- position, in order. The positions must be ascending and distinct.
runs :: [Int] -> [(Int, Int)]
runs = foldr extend []
  where
    extend :: Int -> [(Int, Int)] -> [(Int, Int)]
    extend p = \case
      (a, b) : rest | p + 1 == a -> (p, b) : rest
      acc -> (p, p) : acc

-- | Delete the songs at the positions. The ranges go from the last, so that
-- each one's positions are still right when it runs.
deletePositions :: [Int] -> Command ()
deletePositions ps = for_ (reverse (runs ps)) $ \(a, b) ->
  delete $ Range (SongPos a) (Just (SongPos (b + 1)))

-- | Move the songs at the positions up by one. The song above each run moves
-- below it, so each run costs one command, and the runs don't affect each
-- other. A run at the top stays.
moveUp :: [Int] -> Command ()
moveUp ps = for_ (runs ps) $ \(a, b) ->
  when (a > 0) $ move (onePosition (SongPos (a - 1))) (At (SongPos b))

-- | Move the songs at the positions down by one, in a queue of the given
-- length. A run at the bottom stays.
moveDown :: Int -> [Int] -> Command ()
moveDown len ps = for_ (runs ps) $ \(a, b) ->
  when (b < len - 1) $ move (onePosition (SongPos (b + 1))) (At (SongPos a))

-- | Move the songs at the positions, in their order, to just before the song
-- at a position, or to the end for the length of the queue. 'Nothing' if
-- the position is among the songs, as ncmpcpp does.
--
-- Each run moves as one range. When the songs move down, the runs go from
-- the last; when they move up, from the first. Then each move leaves the
-- positions of the runs still to move as they were. A run that is in place
-- already doesn't move.
moveBefore :: [Int] -> Int -> Maybe (Command ())
moveBefore ps target = do
  songs <- NE.nonEmpty ps
  let k = length ps
      -- Each run with the number of songs before it.
      indexed = zip (scanl (+) 0 [b - a + 1 | (a, b) <- runs ps]) (runs ps)
  if
    | target > NE.last songs ->
        Just $ for_ (reverse indexed) $ \(i, r) -> moveRun r (target - k + i)
    | target < NE.head songs ->
        Just $ for_ indexed $ \(i, r) -> moveRun r (target + i)
    | otherwise -> Nothing
  where
    moveRun :: (Int, Int) -> Int -> Command ()
    moveRun (a, b) to =
      when (a /= to) $ move (Range (SongPos a) (Just (SongPos (b + 1)))) (At (SongPos to))
