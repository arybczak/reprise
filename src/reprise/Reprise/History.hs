-- | The history of the line prompts, which all of them share.
module Reprise.History
  ( Recall (..)
  , historySize
  , remember
  , recallOlder
  , recallOldest
  , recallNewer
  ) where

import Data.Text qualified as T

import Reprise.LineEdit

-- | A line of the history that a prompt shows in place of what the user
-- typed.
data Recall = Recall
  { typed :: LineEdit
  -- ^ What the user typed. Only the lines that start with it are recalled.
  , index :: Int
  -- ^ The line in the history, from the newest.
  }
  deriving stock (Eq, Show)

-- | How many lines the history keeps. Every distinct line would make it
-- grow without end over a long session. The author chose the number, as
-- enough for the lines a user types again.
historySize :: Int
historySize = 100

-- | Add a line to a history, newest first. A line that is in it already
-- moves to the front, so each is in it once, and the oldest line goes when
-- the history is full. A blank line isn't added.
remember :: T.Text -> [T.Text] -> [T.Text]
remember line history
  | T.null (T.strip line) = history
  | otherwise = take historySize $ line : filter (/= line) history

-- | The next older line that starts with what the user typed, with the
-- cursor at its end.
recallOlder :: [T.Text] -> LineEdit -> Maybe Recall -> Maybe (LineEdit, Recall)
recallOlder history edit recall =
  case filter (matches typed . snd) . drop start $ zip [0 ..] history of
    (i, line) : _ -> Just (LineEdit line T.empty, Recall typed i)
    [] -> Nothing
  where
    typed :: LineEdit
    typed = maybe edit (.typed) recall

    start :: Int
    start = maybe 0 ((+ 1) . (.index)) recall

-- | The oldest line that starts with what the user typed, as bash's
-- @beginning-of-history@.
recallOldest :: [T.Text] -> LineEdit -> Maybe Recall -> Maybe (LineEdit, Recall)
recallOldest history edit recall =
  case reverse . filter (matches typed . snd) $ zip [0 ..] history of
    (i, line) : _ -> Just (LineEdit line T.empty, Recall typed i)
    [] -> Nothing
  where
    typed :: LineEdit
    typed = maybe edit (.typed) recall

-- | The next newer line that starts with what the user typed, or what the
-- user typed after the newest one.
recallNewer :: [T.Text] -> Recall -> (LineEdit, Maybe Recall)
recallNewer history recall =
  case filter (matches recall.typed . snd) . reverse . take recall.index $ zip [0 ..] history of
    (i, line) : _ -> (LineEdit line T.empty, Just recall {index = i})
    [] -> (recall.typed, Nothing)

-- | Whether a line can stand for what the user typed. The typed line itself
-- would change nothing.
matches :: LineEdit -> T.Text -> Bool
matches typed line = prefix `T.isPrefixOf` line && line /= prefix
  where
    prefix :: T.Text
    prefix = lineEditText typed
