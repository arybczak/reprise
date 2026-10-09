-- | Finding items by a pattern: an ICU regular expression, which follows
-- Perl's syntax, matched without regard to case or diacritics.
module Reprise.Find
  ( Pattern
  , compilePattern
  , Folded
  , foldText
  , matches
  , matchAll
  , matchRanges
  , Direction (..)
  , Found (..)
  , search
  ) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Data.Text.ICU qualified as ICU
import Data.Text.ICU.Error qualified as ICU
import Data.Text.ICU.Regex qualified as Regex
import GHC.Clock
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
    folded = foldPattern t

-- | Fold the diacritics of a pattern, but not the character after a
-- backslash, as its escape could become another: @\\ñ@ folded as text is
-- @\\n@, a line break. An escaped character outside ASCII is a literal, so
-- it is folded and loses its backslash.
foldPattern :: T.Text -> T.Text
foldPattern t = case T.breakOn "\\" t of
  (plain, escaped) ->
    foldDiacritics plain <> case T.uncons (T.drop 1 escaped) of
      Just (c, rest)
        | isAscii c -> T.pack ['\\', c] <> foldPattern rest
        | otherwise -> foldDiacritics (T.singleton c) <> foldPattern rest
      -- Nothing escaped, or a backslash at the end, which ICU rejects.
      Nothing -> escaped

-- | Text that patterns match, with its diacritics folded, so that text
-- matched more than once is folded once.
newtype Folded = Folded T.Text
  deriving stock (Eq, Show)

foldText :: T.Text -> Folded
foldText = Folded . foldDiacritics

-- | Whether the pattern matches somewhere in the text, or why the match
-- stopped.
matches :: Pattern -> Folded -> Either T.Text Bool
matches p t = withMatcher p ($ t)

-- | 'matches' for each text.
matchAll :: Pattern -> Seq.Seq Folded -> Either T.Text (Seq.Seq Bool)
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
-- going around the end of the list. The item at the index comes last. An
-- index past the end, e.g. of a list that shrank, is the end.
search :: Pattern -> Direction -> Int -> Seq.Seq Folded -> Either T.Text (Maybe Found)
search p direction from items = withMatcher p $ \match -> go match order
  where
    n :: Int
    n = Seq.length items

    start :: Int
    start = min from n

    order :: [(Int, Bool)]
    order = case direction of
      Forward ->
        [(i, False) | i <- [start + 1 .. n - 1]] <> [(i, True) | i <- [0 .. min start (n - 1)]]
      Backward ->
        [(i, False) | i <- [start - 1, start - 2 .. 0]]
          <> [(i, True) | i <- [n - 1, n - 2 .. max 0 start]]

    go :: (Folded -> IO Bool) -> [(Int, Bool)] -> IO (Maybe Found)
    go match = \case
      [] -> pure Nothing
      (i, w) : rest -> do
        found <- match (Seq.index items i)
        if found then pure (Just (Found i w)) else go match rest

-- | The characters of a text that the pattern matches, as the start and the
-- length of each match. A match covers a character whose folded text it
-- covers, and the combining marks after its last one, which folding drops.
matchRanges :: Pattern -> T.Text -> Either T.Text [(Int, Int)]
matchRanges p t = withRegex p $ \matcher -> do
  Regex.setText matcher (T.concat [piece | (_, piece, _) <- pieces])
  let matchesFrom :: IO [(Int, Int)]
      matchesFrom =
        Regex.findNext matcher >>= \case
          False -> pure []
          True -> do
            s <- fromIntegral <$> Regex.start_ matcher 0
            e <- fromIntegral <$> Regex.end_ matcher 0
            ((s, e) :) <$> matchesFrom
  bytes <- matchesFrom
  pure
    [ (first, lastChar - first + 1)
    | (s, e) <- bytes
    , e > s
    , let covered =
            [ i
            | (i, piece, at) <- pieces
            , let n = T.lengthWord8 piece
            , if n > 0 then at < e && at + n > s else at > s && at <= e
            ]
    , (first, lastChar) <- [(minimum covered, maximum covered) | not (null covered)]
    ]
  where
    -- Each character with its folded text and where that starts in the
    -- folded text, in bytes, as ICU counts it.
    pieces :: [(Int, T.Text, Int)]
    pieces =
      let folded = [foldChar c | c <- T.unpack t]
          starts = scanl (+) 0 (map T.lengthWord8 folded)
      in zip3 [0 ..] folded starts

    foldChar :: Char -> T.Text
    foldChar c
      | isAscii c = T.singleton c
      | otherwise = foldDiacritics (T.singleton c)

-- | Run matches with one matcher, which holds the work limit of each match.
-- The matches together stop too once they took as long, as a pattern can
-- stay within the limit on each text of a long list but not on all of them.
withMatcher :: Pattern -> ((Folded -> IO Bool) -> IO a) -> Either T.Text a
withMatcher p act =
  withRegex p $ \matcher -> do
    deadline <- (+ searchTime) <$> getMonotonicTime
    act $ \(Folded t) -> do
      now <- getMonotonicTime
      when (now > deadline) $ throwIO ICU.u_REGEX_TIME_OUT
      Regex.setText matcher t >> Regex.find matcher 0
  where
    -- In seconds, as a reply within 100 ms feels instant, see 'matchOptions'.
    searchTime :: Double
    searchTime = 0.1

-- | Run an action with a matcher of the pattern.
--
-- text-icu's pure matching clones the matcher for every match, and a clone
-- loses the limit, so this uses its IO interface the way the pure one
-- does. ICU reports a match over the limit with an exception.
withRegex :: Pattern -> (Regex.Regex -> IO a) -> Either T.Text a
withRegex (Pattern p) act = unsafePerformIO $ do
  let run = act =<< Regex.regex matchOptions p
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
-- @o@. A letter of its own, such as @ł@, stays. What remains is composed
-- again, so that a syllable of Hangul, which decomposes into its letters,
-- stays one character.
foldDiacritics :: T.Text -> T.Text
foldDiacritics = ICU.nfc . T.filter ((/= NonSpacingMark) . generalCategory) . ICU.nfd
