module HistoryTests (historyTests) where

import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.History
import Reprise.LineEdit

historyTests :: TestTree
historyTests =
  testGroup
    "History"
    [ testCase "remember a line" test_remember
    , testCase "recall older and newer lines" test_recall
    , testCase "recall the lines that start with what was typed" test_recallPrefix
    ]

test_remember :: Assertion
test_remember = do
  assertEqual "newest first" ["b", "a"] (remember "b" ["a"])
  assertEqual "once" ["a", "b"] (remember "a" ["b", "a"])
  assertEqual "not a blank line" ["a"] (remember "  " ["a"])
  let full = map (T.pack . show) [1 .. historySize]
  assertEqual
    "the oldest goes"
    ("new" : take (historySize - 1) full)
    (remember "new" full)

test_recall :: Assertion
test_recall = do
  let history = ["c", "b", "a"]
      typing = LineEdit "" ""
  (c, r1) <- older history typing Nothing
  (b, r2) <- older history c (Just r1)
  (a, r3) <- older history b (Just r2)
  assertEqual "the newest first" (LineEdit "c" "") c
  assertEqual "then older" (LineEdit "b" "") b
  assertEqual "the oldest" (LineEdit "a" "") a
  assertEqual "nothing older" Nothing (recallOlder history a (Just r3))
  assertEqual "newer" (LineEdit "b" "", Just r2) (recallNewer history r3)
  assertEqual "back to the typed line" (typing, Nothing) (recallNewer history r1)

test_recallPrefix :: Assertion
test_recallPrefix = do
  let history = ["volume 40", "seek 1:30", "seek ", "seek 0:10"]
  (first, r1) <- older history (LineEdit "seek " "") Nothing
  (second, r2) <- older history first (Just r1)
  assertEqual "a match" (LineEdit "seek 1:30" "") first
  assertEqual "skips the typed line itself" (LineEdit "seek 0:10" "") second
  assertEqual "no more matches" Nothing (recallOlder history second (Just r2))
  assertEqual
    "back past the lines that don't match"
    (LineEdit "seek 1:30" "", Just r1)
    (recallNewer history r2)
  assertEqual "nothing matches" Nothing (recallOlder history (LineEdit "x" "") Nothing)

-- | The next older line, which the test expects.
older :: [T.Text] -> LineEdit -> Maybe Recall -> IO (LineEdit, Recall)
older history edit recall =
  maybe (assertFailure "no older line") pure $ recallOlder history edit recall
