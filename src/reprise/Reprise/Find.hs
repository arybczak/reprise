-- | Finding items by a pattern: an ICU regular expression, which follows
-- Perl's syntax, matched without regard to case or diacritics.
module Reprise.Find
  ( Pattern
  , compilePattern
  , matches
  , matchAll
  , Direction (..)
  , Found (..)
  , search
  ) where

import Control.Exception
import Data.Char
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.ICU qualified as ICU
import Data.Text.ICU.Error qualified as ICU
import Data.Text.ICU.Regex qualified as Regex
import System.IO.Unsafe

-- | A valid pattern, with its diacritics folded.
newtype Pattern = Pattern T.Text

-- | A pattern, or why the text isn't one. While the user types, a pattern
-- is often unfinished for a moment, e.g. right after @(@.
compilePattern :: T.Text -> Either T.Text Pattern
compilePattern t
  -- ICU rejects an empty pattern with an exception rather than a parse
  -- error. Folding can empty a pattern of combining marks.
  | T.null folded = Left "empty pattern"
  | otherwise = case ICU.regex' matchOptions folded of
      Right _ -> Right (Pattern folded)
      Left _ -> Left "incomplete pattern"
  where
    folded :: T.Text
    folded = foldDiacritics t

-- | Whether the pattern matches somewhere in the text, or why the match
-- stopped.
matches :: Pattern -> T.Text -> Either T.Text Bool
matches p t = withMatcher p ($ t)

-- | 'matches' for each text.
matchAll :: Pattern -> Seq.Seq T.Text -> Either T.Text (Seq.Seq Bool)
matchAll p ts = withMatcher p (`traverse` ts)

data Direction = Forward | Backward
  deriving stock (Eq, Show)

data Found = Found
  { index :: Int
  , wrapped :: Bool
  -- ^ Whether the search went past the end of the list to its start, or
  -- the other way.
  }
  deriving stock (Eq, Show)

-- | The first item after the given index that matches, in the direction,
-- going around the end of the list. The item at the index comes last.
search :: Pattern -> Direction -> Int -> Seq.Seq T.Text -> Either T.Text (Maybe Found)
search p direction start items = withMatcher p $ \match -> go match order
  where
    n :: Int
    n = Seq.length items

    order :: [(Int, Bool)]
    order = case direction of
      Forward ->
        [(i, False) | i <- [start + 1 .. n - 1]] <> [(i, True) | i <- [0 .. min start (n - 1)]]
      Backward ->
        [(i, False) | i <- [start - 1, start - 2 .. 0]]
          <> [(i, True) | i <- [n - 1, n - 2 .. max 0 start]]

    go :: (T.Text -> IO Bool) -> [(Int, Bool)] -> IO (Maybe Found)
    go match = \case
      [] -> pure Nothing
      (i, w) : rest -> do
        found <- match (Seq.index items i)
        if found then pure (Just (Found i w)) else go match rest

-- | Run matches with one matcher, which holds the work limit.
--
-- text-icu's pure matching clones the matcher for every match, and a clone
-- loses the limit, so this uses its IO interface the way the pure one
-- does. ICU reports a match over the limit with an exception.
withMatcher :: Pattern -> ((T.Text -> IO Bool) -> IO a) -> Either T.Text a
withMatcher (Pattern p) act = unsafePerformIO $ do
  let run = do
        matcher <- Regex.regex matchOptions p
        act $ \t -> Regex.setText matcher (foldDiacritics t) >> Regex.find matcher 0
  try run >>= \case
    Right a -> pure (Right a)
    Left err
      | err == ICU.u_REGEX_TIME_OUT -> pure (Left "the pattern is too slow")
      | otherwise -> pure (Left (T.pack (ICU.errorName err)))

matchOptions :: [ICU.MatchOption]
matchOptions = [ICU.CaseInsensitive, ICU.WorkLimit workLimit]
  where
    -- ICU counts the limit in steps of its match engine, which take "on the
    -- order of milliseconds" according to its documentation. A find runs
    -- on every key, and a reply within 100 ms feels instant, so a match
    -- that takes longer stops instead of freezing the screen. In the test
    -- of a pattern that backtracks exponentially, the limit stops the match
    -- after about 10 ms.
    workLimit :: Int
    workLimit = 100

-- | Decompose the characters and drop the combining marks, e.g. @ó@ becomes
-- @o@. A letter of its own, such as @ł@, stays.
foldDiacritics :: T.Text -> T.Text
foldDiacritics = T.filter ((/= NonSpacingMark) . generalCategory) . ICU.nfd
