module Reprise.Test.Width (widthTests) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Text qualified as T
import Graphics.Text.Width qualified as W
import Graphics.Vty qualified as V
import Graphics.Vty.Image.Internal qualified as VI
import Graphics.Vty.UnicodeWidthTable.Query qualified as V
import Graphics.Vty.UnicodeWidthTable.Types qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Test.Helpers
import Reprise.UI.Layout
import Reprise.Width

widthTests :: TestTree
widthTests =
  testGroup
    "width"
    [ testCase "truncation" test_truncate
    , testCase "wrapping" test_wrap
    , testCase "scrolling" test_scroll
    , testCase "an emoji is wide" test_emoji
    , testCase "a row with an emoji fits the terminal" test_emojiRowFits
    , testCase "a row with control characters fits the terminal" test_controlRowFits
    , testCase "sequences that terminals draw differently" test_sequences
    , testCase "the ranges are vty's" test_sameRanges
    , testCase "a Unicode newer than GHC's" test_newerUnicode
    ]

test_truncate :: Assertion
test_truncate = do
  assertEqual "short enough" "abc" (truncateToWidth 3 "abc")
  assertEqual "ellipsis" "ab…" (truncateToWidth 3 "abcd")
  assertEqual "no space before the ellipsis" "ab…" (truncateToWidth 4 "ab cd")
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

test_scroll :: Assertion
test_scroll = do
  assertEqual "fits" "abc" (scrollText 3 1 "abc")
  assertEqual "the start" "abc" (scrollText 3 0 "abcd")
  assertEqual "a step" "bcd" (scrollText 3 1 "abcd")
  assertEqual "round" "d *" (scrollText 3 3 "abcd")
  assertEqual "again" "abc" (scrollText 3 8 "abcd")
  -- An é of a letter and its accent, as in a name from macOS, is one step.
  assertEqual "a letter with its accent" "bcd" (scrollText 3 1 "e\x301\&bcd")
  assertEqual "the steps of a round" "e\x301\&bc" (scrollText 3 8 "e\x301\&bcd")

-- | vty takes only the characters that GHC's Unicode has, and the built-in
-- width of the ones that the C library doesn't know.
test_sameRanges :: Assertion
test_sameRanges = do
  width <- systemWidth >>= maybe (assertFailure "no UTF-8 locale") pure
  let anyWidth c = fromMaybe (W.safeWcwidth c) <$> width c
      known c
        | generalCategory c == NotAssigned = pure Nothing
        | otherwise = Just <$> anyWidth c
  ours <- widthRanges known V.defaultUnicodeTableUpperBound
  vtys <- V.buildUnicodeWidthTable anyWidth V.defaultUnicodeTableUpperBound
  assertEqual "ranges" (V.unicodeWidthTableRanges vtys) ours

-- | The C library may know a newer Unicode than GHC.
test_newerUnicode :: Assertion
test_newerUnicode = do
  -- Unassigned in every Unicode so far.
  let newer = '\xE0080'
      width c = pure $ if c == newer then Just 2 else Nothing
  assertEqual "GHC doesn't have it" NotAssigned (generalCategory newer)
  assertEqual "the ranges" [V.WidthTableRange 0xE0080 1 2] =<< widthRanges width '\xE00FF'

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
  forM_ (imageLines (uncropped s)) $ \line -> do
    w <- sum <$> mapM (\c -> fromMaybe (W.safeWcwidth c) <$> width c) (T.unpack line)
    assertBool ("wider than the terminal: " <> T.unpack line) (w <= 80)

-- | A terminal draws an emoji that a selector asks for, and emoji that a
-- joiner joins, wider or narrower than they measure, and a control of the
-- direction of text can reverse a row. They are left out.
test_sequences :: Assertion
test_sequences = do
  assertEqual "the heart of text" "❤" (sanitize "❤\xFE0F")
  assertEqual "emoji each on their own" "👨👩👧" (sanitize "👨\x200D👩\x200D👧")
  assertEqual "no change of direction" "ab" (sanitize "a\x202E\&b")
  forM_ ["❤\xFE0F", "👨\x200D👩\x200D👧", "a\x202E\&b"] $ \t ->
    assertEqual ("the width of " <> show t) (textWidth t) (textWidth (sanitize t))

-- | A control character shows as a space, which takes a column.
test_controlRowFits :: Assertion
test_controlRowFits = do
  let title = "a\tb\DELc\x85\&d " <> T.replicate 20 "\t"
  s <-
    testState
      (80, 6)
      (statusOf Playing (Just 0) 1)
      [song 0 [(Artist, ["A"]), (Title, [title])] 60]
  forM_ (imageLines (uncropped s)) $ \line -> do
    assertBool ("a control character: " <> show line) (not (T.any isControl line))
    assertBool ("wider than the terminal: " <> show line) (textWidth line <= 80)
  assertEqual "the width of the screen" 80 (V.imageWidth (uncropped s))

-- | The screen before its crop to the terminal's size, which would hide a
-- row that came out too wide.
uncropped :: AppState -> V.Image
uncropped s = case renderScreen testAppEnv s of
  VI.Crop i _ _ _ _ -> i
  i -> i
