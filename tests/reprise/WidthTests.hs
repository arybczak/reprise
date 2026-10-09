module WidthTests (widthTests) where

import Control.Monad
import Data.Char
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.UnicodeWidthTable.Query qualified as V
import Graphics.Vty.UnicodeWidthTable.Types qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Mpd.Protocol.Types
import Reprise.UI.Layout
import Reprise.Width
import Utils

widthTests :: TestTree
widthTests =
  testGroup
    "Width"
    [ testCase "truncation" test_truncate
    , testCase "wrapping" test_wrap
    , testCase "an emoji is wide" test_emoji
    , testCase "a row with an emoji fits the terminal" test_emojiRowFits
    , testCase "a row with control characters fits the terminal" test_controlRowFits
    , testCase "the ranges are vty's" test_sameRanges
    ]

test_truncate :: Assertion
test_truncate = do
  assertEqual "short enough" "abc" (truncateToWidth 3 "abc")
  assertEqual "ellipsis" "ab…" (truncateToWidth 3 "abcd")
  assertEqual "wide characters" "日…" (truncateToWidth 4 "日本語")
  assertEqual "wide character width" 6 (textWidth "日本語")

test_wrap :: Assertion
test_wrap = do
  assertEqual "short enough" ["one two"] (wrapText 7 "one two")
  assertEqual "at spaces" ["one two", "three"] (wrapText 7 "one two three")
  assertEqual "a long word" ["abcd", "efgh", "ij"] (wrapText 4 "abcdefghij")
  assertEqual "a long word after a short one" ["a", "bcde", "f g"] (wrapText 4 "a bcdef g")
  assertEqual "wide characters" ["日本", "語"] (wrapText 5 "日本語")
  assertEqual "a character wider than the width" ["日", "本"] (wrapText 1 "日本")
  assertEqual "an empty line" [""] (wrapText 4 "")
  assertEqual "no width" ["abc"] (wrapText 0 "abc")

test_sameRanges :: Assertion
test_sameRanges = do
  width <- systemWidth >>= maybe (assertFailure "no UTF-8 locale") pure
  ours <- widthRanges width V.defaultUnicodeTableUpperBound
  vtys <- V.buildUnicodeWidthTable width V.defaultUnicodeTableUpperBound
  assertEqual "ranges" (V.unicodeWidthTableRanges vtys) ours

test_emoji :: Assertion
test_emoji = assertEqual "width" 2 (textWidth "🌍")

-- | The C library is the oracle for the terminal's widths.
test_emojiRowFits :: Assertion
test_emojiRowFits = do
  width <- systemWidth >>= maybe (assertFailure "no UTF-8 locale") pure
  let title = "Hello 🌍 world, with a title long enough to fill its column"
  s <-
    testState
      (80, 6)
      (statusOf Playing (Just 0) 1)
      [song 0 [(Artist, ["A"]), (Title, [title])] 60]
  forM_ (imageLines (renderScreen testAppEnv s)) $ \line -> do
    w <- sum <$> mapM width (T.unpack line)
    assertBool ("wider than the terminal: " <> T.unpack line) (w <= 80)

-- | A control character shows as a space, which takes a column.
test_controlRowFits :: Assertion
test_controlRowFits = do
  let title = "a\tb\DELc\x85\&d " <> T.replicate 20 "\t"
  s <-
    testState
      (80, 6)
      (statusOf Playing (Just 0) 1)
      [song 0 [(Artist, ["A"]), (Title, [title])] 60]
  forM_ (imageLines (renderScreen testAppEnv s)) $ \line -> do
    assertBool ("a control character: " <> show line) (not (T.any isControl line))
    assertBool ("wider than the terminal: " <> show line) (textWidth line <= 80)
  assertEqual "the width of the screen" 80 (V.imageWidth (renderScreen testAppEnv s))
