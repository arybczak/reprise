-- | The selection of a list. It holds the items by their keys, e.g. the ids
-- of songs in the queue, so that it follows the items when the list
-- changes.
module Reprise.Selection
  ( Selection (..)
  , noSelection
  , isSelected
  , toggleKey
  , selectRange
  , invert
  , deselectAll
  , addKeys
  , restrictTo
  , selectedPositions
  ) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S

data Selection k = Selection
  { keys :: S.Set k
  , lastSelected :: [k]
  -- ^ The items that the user selected last, the latest first: the ends of
  -- the next range.
  }
  deriving stock (Eq, Show)

noSelection :: Selection k
noSelection = Selection S.empty []

isSelected :: Ord k => k -> Selection k -> Bool
isSelected k sel = k `S.member` sel.keys

-- | Select an item, or deselect it if it is selected.
toggleKey :: forall k. Ord k => k -> Selection k -> Selection k
toggleKey k sel
  | isSelected k sel = Selection (S.delete k sel.keys) others
  | otherwise = Selection (S.insert k sel.keys) (take rangeEnds (k : others))
  where
    others :: [k]
    others = filter (/= k) sel.lastSelected

-- | A range has a first and a last item.
rangeEnds :: Int
rangeEnds = 2

-- | Fill the selection between the last two items that the user selected,
-- so that a range doesn't swallow the items between it and an earlier
-- selection. Without them, between the first and the last selected item,
-- as in ncmpcpp. Nothing without a selected item. The keys are those of the
-- list's items, and an item without one can't be selected.
selectRange :: Ord k => Seq.Seq (Maybe k) -> Selection k -> Maybe (Selection k)
selectRange items sel =
  let positionOf k = Seq.findIndexL (== Just k) items
      ends = mapMaybe positionOf $ filter (`isSelected` sel) sel.lastSelected
  in case if length ends == rangeEnds then ends else selectedPositions items sel of
       [] -> Nothing
       ps ->
         Just $
           addKeys (catMaybes [join (Seq.lookup i items) | i <- [minimum ps .. maximum ps]]) sel

-- | Select the items that aren't selected, and deselect the others.
invert :: Ord k => Seq.Seq (Maybe k) -> Selection k -> Selection k
invert items sel = sel {keys = S.fromList (catMaybes (toList items)) S.\\ sel.keys}

-- | Deselect every item. The ends of the next range stay, so that selecting
-- them again makes a range.
deselectAll :: Selection k -> Selection k
deselectAll sel = sel {keys = S.empty}

addKeys :: Ord k => [k] -> Selection k -> Selection k
addKeys ks sel = sel {keys = S.union (S.fromList ks) sel.keys}

-- | Keep only the keys of items that are still there. Without a selection,
-- the keys aren't needed, so a lazy set of them isn't made, e.g. of every
-- song after a change of a long queue.
restrictTo :: Ord k => S.Set k -> Selection k -> Selection k
restrictTo ks sel
  | S.null sel.keys = sel
  | otherwise = sel {keys = S.intersection sel.keys ks}

-- | The positions of the selected items, in order.
selectedPositions :: Ord k => Seq.Seq (Maybe k) -> Selection k -> [Int]
selectedPositions items sel =
  [i | (i, Just k) <- zip [0 ..] (toList items), isSelected k sel]
