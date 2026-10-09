module FindTests (findTests) where

import Control.Exception
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Find

findTests :: TestTree
findTests =
  testGroup
    "Find"
    [ testCase "matching" test_matching
    , testCase "an incomplete pattern" test_incomplete
    , testCase "a pattern that is too slow" test_tooSlow
    , testCase "a search that is too slow" test_searchTooSlow
    , testCase "searching" test_search
    , testCase "where a pattern matches" test_matchRanges
    ]

test_matchRanges :: Assertion
test_matchRanges = do
  assertEqual "each match" (Right [(1, 1), (3, 1), (5, 1)]) (ranges "a" "banana")
  assertEqual "case" (Right [(0, 4)]) (ranges "beta" "BETA")
  assertEqual "diacritics" (Right [(0, 5)]) (ranges "pokoj" "Pokój")
  assertEqual "a mark after the match" (Right [(0, 2)]) (ranges "o" "o\x301x")
  assertEqual "after a wide character" (Right [(2, 1)]) (ranges "b" "日ab")
  assertEqual "no empty match" (Right []) (ranges "x*" "ab")
  where
    ranges :: T.Text -> T.Text -> Either T.Text [(Int, Int)]
    ranges p t = either (Left . ("compile: " <>)) (`matchRanges` t) (compilePattern p)

test_matching :: Assertion
test_matching = do
  assertEqual "a regular expression" (Right True) (match "^b.t" "beta")
  assertEqual "anywhere in the text" (Right True) (match "et" "beta")
  assertEqual "no match" (Right False) (match "x" "beta")
  assertEqual "case" (Right True) (match "BETA" "beta")
  assertEqual "diacritics in the text" (Right True) (match "pokoj" "Pokój")
  assertEqual "diacritics in the pattern" (Right True) (match "pokój" "Pokoj")
  -- ł is a letter of its own, not l with a mark.
  assertEqual "a letter of its own" (Right False) (match "zolw" "żółw")
  -- Folded as text, \ñ would be \n, a line break, and \ü an incomplete
  -- escape.
  assertEqual "an escaped letter with a diacritic" (Right True) (match "a\\ñ" "año")
  assertEqual "another" (Right True) (match "\\über" "uber")
  assertEqual "an escape" (Right True) (match "a\\d" "a1")
  -- A syllable of Hangul stays whole, so it doesn't match another syllable
  -- with some of its letters.
  assertEqual "a syllable of Hangul" (Right False) (match "[가각]" "나")
  assertEqual "a class of syllables" (Right True) (match "[가각]" "각")
  where
    match :: T.Text -> T.Text -> Either T.Text Bool
    match p t = either (Left . ("compile: " <>)) (`matches` foldText t) (compilePattern p)

test_incomplete :: Assertion
test_incomplete = do
  assertEqual "incomplete" (Just "incomplete pattern") (failure "ab(")
  -- ICU throws on an empty pattern.
  assertEqual "empty" (Just "empty pattern") (failure "")
  assertEqual "empty after folding" (Just "empty pattern") (failure "\x301")
  where
    failure :: T.Text -> Maybe T.Text
    failure = either Just (const Nothing) . compilePattern

-- | A pattern that backtracks exponentially stops instead of hanging.
test_tooSlow :: Assertion
test_tooSlow = case compilePattern "(a+)+$" of
  Left err -> assertFailure (T.unpack err)
  Right p ->
    assertEqual
      "error"
      (Left "the pattern is too slow")
      (matches p (foldText (T.replicate 40 "a" <> "b")))

-- | A pattern that is fast enough on each row, but slow on all of them, as
-- each row of 16 letters takes it a few milliseconds: the search stops
-- instead of taking seconds.
test_searchTooSlow :: Assertion
test_searchTooSlow = case compilePattern "(a+)+b" of
  Left err -> assertFailure (T.unpack err)
  Right p -> do
    let rows = Seq.replicate 4000 (foldText (T.replicate 16 "a"))
    r <- timeout (5 * 1000000) . evaluate $ search p Forward 0 rows
    assertEqual "error" (Just (Left "the pattern is too slow")) r

test_search :: Assertion
test_search = case compilePattern "a" of
  Left err -> assertFailure (T.unpack err)
  Right p -> do
    let items = folded ["a", "b", "a", "b"]
    assertEqual "forward" (Right (Just (Found 2 False))) (search p Forward 0 items)
    assertEqual "forward around" (Right (Just (Found 0 True))) (search p Forward 2 items)
    assertEqual "backward" (Right (Just (Found 0 False))) (search p Backward 2 items)
    assertEqual "backward around" (Right (Just (Found 2 True))) (search p Backward 0 items)
    assertEqual
      "only the start"
      (Right (Just (Found 0 True)))
      (search p Forward 0 (folded ["a", "b"]))
    assertEqual "nothing" (Right Nothing) (search p Forward 0 (folded ["b", "c"]))
    assertEqual "empty" (Right Nothing) (search p Backward 0 Seq.empty)
    -- The list shrank since the search's start was the cursor.
    assertEqual
      "backward from past the end"
      (Right (Just (Found 2 False)))
      (search p Backward 6 items)
    assertEqual
      "forward from past the end"
      (Right (Just (Found 0 True)))
      (search p Forward 6 items)
    assertEqual "backward in an empty list" (Right Nothing) (search p Backward 5 Seq.empty)
  where
    folded :: [T.Text] -> Seq.Seq Folded
    folded = Seq.fromList . map foldText
