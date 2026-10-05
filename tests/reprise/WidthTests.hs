module WidthTests (widthTests) where

import Control.Monad
import Data.Text qualified as T
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
    , testCase "an emoji is wide" test_emoji
    , testCase "a row with an emoji fits the terminal" test_emojiRowFits
    , testCase "the ranges are vty's" test_sameRanges
    ]

test_truncate :: Assertion
test_truncate = do
  assertEqual "short enough" "abc" (truncateToWidth 3 "abc")
  assertEqual "ellipsis" "ab…" (truncateToWidth 3 "abcd")
  assertEqual "wide characters" "日…" (truncateToWidth 4 "日本語")
  assertEqual "wide character width" 6 (textWidth "日本語")

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
