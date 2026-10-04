-- | MPD commands that act on several songs of the queue, planned from their
-- positions. Each is one command list.
module Reprise.Screen.Queue
  ( runs
  , deletePositions
  , moveUp
  , moveDown
  , moveBefore
  ) where

import Data.Foldable
import Data.List.NonEmpty qualified as NE
import MPD.Command
import MPD.Types

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
  if a > 0 then move (onePosition (SongPos (a - 1))) (At (SongPos b)) else pure ()

-- | Move the songs at the positions down by one, in a queue of the given
-- length. A run at the bottom stays.
moveDown :: Int -> [Int] -> Command ()
moveDown len ps = for_ (runs ps) $ \(a, b) ->
  if b < len - 1 then move (onePosition (SongPos (b + 1))) (At (SongPos a)) else pure ()

-- | Move the songs at the positions, in their order, to just before the song
-- at a position, or to the end for the length of the queue. 'Nothing' if
-- the position is among the songs, as ncmpcpp does.
--
-- When the songs move down, they go from the last; when they move up, from
-- the first. Then each move leaves the positions of the songs still to
-- move as they were.
moveBefore :: [Int] -> Int -> Maybe (Command ())
moveBefore ps target = do
  songs <- NE.nonEmpty ps
  let k = length ps
  if
    | target > NE.last songs ->
        Just $ for_ (reverse (zip [0 ..] ps)) $ \(i, p) ->
          move (onePosition (SongPos p)) (At (SongPos (target - k + i)))
    | target < NE.head songs ->
        Just $ for_ (zip [0 ..] ps) $ \(i, p) ->
          move (onePosition (SongPos p)) (At (SongPos (target + i)))
    | otherwise -> Nothing
