-- | Edits that act on several songs of the queue, planned from their
-- positions. Each is one command list.
module Reprise.Screen.Queue.Edits
  ( deletePositions
  , moveUp
  , runsMovingUp
  , moveDown
  , runsMovingDown
  , moveBefore
  , moveToStart
  , moveAfter
  ) where

import Data.Foldable
import Data.List.NonEmpty qualified as NE

import Reprise.Groups
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Types

-- | Delete the songs at the positions. The ranges go from the last, so that
-- each one's positions are still right when it runs.
deletePositions :: [Int] -> [QueueEdit]
deletePositions ps = map (DeleteRange . runRange) (reverse (runs ps))

-- | Move the songs at the positions up by one. The song above each run moves
-- below it, so each run costs one command, and the runs don't affect each
-- other.
moveUp :: [Int] -> [QueueEdit]
moveUp ps =
  [MoveRange (onePosition (SongPos (a - 1))) (SongPos b) | (a, b) <- runsMovingUp ps]

-- | The runs of the positions that 'moveUp' moves: a run at the top stays.
runsMovingUp :: [Int] -> [(Int, Int)]
runsMovingUp ps = [r | r@(a, _) <- runs ps, a > 0]

-- | Move the songs at the positions down by one, in a queue of the given
-- length.
moveDown :: Int -> [Int] -> [QueueEdit]
moveDown len ps =
  [MoveRange (onePosition (SongPos (b + 1))) (SongPos a) | (a, b) <- runsMovingDown len ps]

-- | The runs of the positions that 'moveDown' moves in a queue of the given
-- length: a run at the bottom stays.
runsMovingDown :: Int -> [Int] -> [(Int, Int)]
runsMovingDown len ps = [r | r@(_, b) <- runs ps, b < len - 1]

-- | Move the songs at the positions, in their order, to just before the song
-- at a position, or to the end for the length of the queue. 'Nothing' if
-- the position is among the songs, as ncmpcpp does.
--
-- Each run moves as one range. When the songs move down, the runs go from
-- the last; when they move up, from the first. Then each move leaves the
-- positions of the runs still to move as they were. A run that is in place
-- already doesn't move.
moveBefore :: [Int] -> Int -> Maybe [QueueEdit]
moveBefore ps target = do
  songs <- NE.nonEmpty ps
  if
    | target > NE.last songs ->
        Just $
          concatMap (\(i, r) -> moveRun r (target - length ps + i)) (reverse (indexedRuns ps))
    | target < NE.head songs -> Just (moveUpTo target ps)
    | otherwise -> Nothing

-- | Move the songs at the positions, in their order, to the start of the
-- queue. Unlike 'moveBefore' with the first position, the songs may be
-- there already, and the others join them.
moveToStart :: [Int] -> [QueueEdit]
moveToStart = moveUpTo 0

-- | Move the songs at the positions, in their order, to just after the song
-- at a position, which isn't one of them. The songs below it move up first,
-- which leaves the positions above it as they were, and then the songs
-- above it move down.
moveAfter :: Int -> [Int] -> [QueueEdit]
moveAfter c ps =
  let (above, below) = span (< c) ps
  in moveUpTo (c + 1) below <> fold (moveBefore above (c + 1))

-- | Move the songs at the positions, in their order, up to a position
-- before them.
moveUpTo :: Int -> [Int] -> [QueueEdit]
moveUpTo target ps = concatMap (\(i, r) -> moveRun r (target + i)) (indexedRuns ps)

-- | Each run with the number of songs before it.
indexedRuns :: [Int] -> [(Int, (Int, Int))]
indexedRuns ps = zip (scanl (+) 0 [b - a + 1 | (a, b) <- runs ps]) (runs ps)

-- | Move a run to a position, unless it is there.
moveRun :: (Int, Int) -> Int -> [QueueEdit]
moveRun run@(a, _) to = [MoveRange (runRange run) (SongPos to) | a /= to]
