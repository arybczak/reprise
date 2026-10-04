-- | A line of text being edited, with Emacs-style keys.
module Reprise.LineEdit
  ( LineEdit (..)
  , emptyLineEdit
  , lineEditText
  , editLine
  , visibleLine
  ) where

import Data.Char hiding (Space)
import Data.Set qualified as S
import Data.Text qualified as T

import Reprise.Format
import Reprise.Keys

-- | The text before and after the cursor.
data LineEdit = LineEdit
  { before :: T.Text
  , after :: T.Text
  }
  deriving stock (Eq, Show)

emptyLineEdit :: LineEdit
emptyLineEdit = LineEdit T.empty T.empty

lineEditText :: LineEdit -> T.Text
lineEditText e = e.before <> e.after

-- | Apply an editing key, or 'Nothing' for a key that doesn't edit.
--
-- @ctrl-w@ deletes to the previous space, as in a shell. The @alt@ keys
-- work on words of letters and digits, as in readline.
editLine :: KeySpec -> LineEdit -> Maybe LineEdit
editLine k e = case (S.toList k.modifiers, k.key) of
  ([], CharKey c) -> Just $ e {before = T.snoc e.before c}
  ([], Space) -> Just $ e {before = T.snoc e.before ' '}
  ([], Backspace) -> Just $ e {before = T.dropEnd 1 e.before}
  ([], DeleteKey) -> Just deleteNext
  ([Ctrl], CharKey 'd') -> Just deleteNext
  ([], ArrowLeft) -> Just left
  ([Ctrl], CharKey 'b') -> Just left
  ([], ArrowRight) -> Just right
  ([Ctrl], CharKey 'f') -> Just right
  ([], Home) -> Just home
  ([Ctrl], CharKey 'a') -> Just home
  ([], End) -> Just end
  ([Ctrl], CharKey 'e') -> Just end
  ([Ctrl], CharKey 'k') -> Just $ e {after = T.empty}
  ([Ctrl], CharKey 'u') -> Just $ e {before = T.empty}
  ([Ctrl], CharKey 'w') -> Just $ e {before = T.dropWhileEnd (not . isSpace) (T.stripEnd e.before)}
  ([Alt], CharKey 'b') -> Just $ let (rest, word) = wordBefore in LineEdit rest (word <> e.after)
  ([Alt], CharKey 'f') -> Just $ let (word, rest) = wordAfter in LineEdit (e.before <> word) rest
  ([Alt], CharKey 'd') -> Just $ e {after = snd wordAfter}
  ([Alt], Backspace) -> Just $ e {before = fst wordBefore}
  _ -> Nothing
  where
    deleteNext :: LineEdit
    deleteNext = e {after = T.drop 1 e.after}

    left :: LineEdit
    left = case T.unsnoc e.before of
      Just (rest, c) -> LineEdit rest (T.cons c e.after)
      Nothing -> e

    right :: LineEdit
    right = case T.uncons e.after of
      Just (c, rest) -> LineEdit (T.snoc e.before c) rest
      Nothing -> e

    home :: LineEdit
    home = LineEdit T.empty (lineEditText e)

    end :: LineEdit
    end = LineEdit (lineEditText e) T.empty

    -- The text before the word that ends at the cursor, and the word with
    -- what separates it from the cursor.
    wordBefore :: (T.Text, T.Text)
    wordBefore =
      let gap = T.takeWhileEnd (not . isAlphaNum) e.before
          rest = T.dropWhileEnd (not . isAlphaNum) e.before
      in (T.dropWhileEnd isAlphaNum rest, T.takeWhileEnd isAlphaNum rest <> gap)

    -- The word that starts at the cursor with what separates it from the
    -- cursor, and the text after it.
    wordAfter :: (T.Text, T.Text)
    wordAfter =
      let (gap, rest) = T.span (not . isAlphaNum) e.after
          (word, rest') = T.span isAlphaNum rest
      in (gap <> word, rest')

-- | What of the line fits in the given number of columns, with the column of
-- the cursor. The line scrolls so that the cursor stays in view, with a
-- column for the cursor after the text.
visibleLine :: Int -> LineEdit -> (T.Text, Int)
visibleLine room e
  | beforeWidth < room = (takeWidth room (lineEditText e), beforeWidth)
  | otherwise =
      let shown = takeWidthEnd (room - 1) e.before
      in (takeWidth room (shown <> e.after), textWidth shown)
  where
    beforeWidth :: Int
    beforeWidth = textWidth e.before

    takeWidthEnd :: Int -> T.Text -> T.Text
    takeWidthEnd n = T.reverse . takeWidth n . T.reverse
