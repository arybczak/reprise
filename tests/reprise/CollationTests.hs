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
    , testCase "the locale of the order" test_locale
    , testCase "numbers by their values" test_numbers
    ]

-- | A number sorts by its value, e.g. of a track without a leading 0 in an
-- album of more than 9.
test_numbers :: Assertion
test_numbers = do
  assertEqual
    "tracks"
    ["1 - a.flac", "2 - b.flac", "10 - c.flac"]
    (sortBy False ["10 - c.flac", "2 - b.flac", "1 - a.flac"])
  assertEqual
    "leading zeros"
    ["01 a", "02 b", "10 c"]
    (sortBy False ["10 c", "02 b", "01 a"])

-- | Swedish puts ä after z. The locale is of LC_ALL, else LC_COLLATE, else
-- LANG, without its codeset.
test_locale :: Assertion
test_locale = do
  let swedish = ["apple", "zeta", "äpple"]
      english = ["apple", "äpple", "zeta"]
      order values = L.sortOn (collationKey (localeCollator values) False) ["zeta", "äpple", "apple"]
  assertEqual "LC_COLLATE" swedish (order [Nothing, Just "sv_SE.UTF-8", Just "en_US.UTF-8"])
  assertEqual
    "LC_ALL first"
    english
    (order [Just "en_US.UTF-8", Just "sv_SE.UTF-8", Nothing])
  assertEqual "an empty one is unset" swedish (order [Just "", Just "sv_SE", Nothing])
  assertEqual "LANG" swedish (order [Nothing, Nothing, Just "sv_SE.UTF-8@euro"])
  assertEqual "C" english (order [Just "C.UTF-8", Just "sv_SE.UTF-8", Nothing])
  assertEqual "none" english (order [Nothing, Nothing, Nothing])

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
