module CollationTests (collationTests) where

import Data.List qualified as L
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Collation

collationTests :: TestTree
collationTests =
  testGroup
    "Collation"
    [ testCase "case doesn't come first" test_case
    , testCase "a leading the" test_leadingThe
    ]

-- | In the order of bytes, every capital comes before every small letter.
test_case :: Assertion
test_case =
  assertEqual
    "order"
    ["abba", "Beatles", "cream"]
    (sortBy False ["cream", "Beatles", "abba"])

test_leadingThe :: Assertion
test_leadingThe = do
  let names = ["The Beatles", "Abba", "Cream", "Theatre"]
  assertEqual "ignored" ["Abba", "The Beatles", "Cream", "Theatre"] (sortBy True names)
  assertEqual "kept" ["Abba", "Cream", "The Beatles", "Theatre"] (sortBy False names)

sortBy :: Bool -> [T.Text] -> [T.Text]
sortBy ignoreThe = L.sortOn (collationKey rootCollator ignoreThe)
