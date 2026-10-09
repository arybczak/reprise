module LineEditTests (lineEditTests) where

import Data.Foldable
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Keys
import Reprise.LineEdit

lineEditTests :: TestTree
lineEditTests =
  testGroup
    "LineEdit"
    [ testCase "typing and deleting" test_typing
    , testCase "moving the cursor" test_moving
    , testCase "killing" test_killing
    , testCase "words" test_words
    , testCase "keys that don't edit" test_notEditing
    , testCase "the visible part" test_visible
    , testCase "characters as people see them" test_graphemes
    ]

test_typing :: Assertion
test_typing = do
  assertEqual "typed" (LineEdit "a b" "") (edits ["a", "space", "b"] emptyLineEdit)
  assertEqual
    "backspace"
    (LineEdit "a" "")
    (edits ["backspace", "backspace"] (LineEdit "ab " ""))
  assertEqual
    "backspace at the start"
    (LineEdit "" "ab")
    (edits ["backspace"] (LineEdit "" "ab"))
  assertEqual "delete" (LineEdit "a" "c") (edits ["delete"] (LineEdit "a" "bc"))
  assertEqual "ctrl-d" (LineEdit "a" "") (edits ["ctrl-d", "ctrl-d"] (LineEdit "a" "b"))
  assertEqual
    "inserted at the cursor"
    (LineEdit "axb" "c")
    (edits ["x", "b"] (LineEdit "a" "c"))

test_moving :: Assertion
test_moving = do
  let e = LineEdit "ab" "cd"
  assertEqual "left" (LineEdit "a" "bcd") (edits ["left"] e)
  assertEqual "ctrl-b" (LineEdit "a" "bcd") (edits ["ctrl-b"] e)
  assertEqual "right" (LineEdit "abc" "d") (edits ["right"] e)
  assertEqual "ctrl-f" (LineEdit "abc" "d") (edits ["ctrl-f"] e)
  assertEqual "home" (LineEdit "" "abcd") (edits ["home"] e)
  assertEqual "ctrl-a" (LineEdit "" "abcd") (edits ["ctrl-a"] e)
  assertEqual "end" (LineEdit "abcd" "") (edits ["end"] e)
  assertEqual "ctrl-e" (LineEdit "abcd" "") (edits ["ctrl-e"] e)
  assertEqual
    "left stops at the start"
    (LineEdit "" "abcd")
    (edits ["left", "left", "left"] e)

test_killing :: Assertion
test_killing = do
  let e = LineEdit "one two  " "three"
  assertEqual "ctrl-k" (LineEdit "one two  " "") (edits ["ctrl-k"] e)
  assertEqual "ctrl-u" (LineEdit "" "three") (edits ["ctrl-u"] e)
  assertEqual "ctrl-w" (LineEdit "one " "three") (edits ["ctrl-w"] e)
  assertEqual "ctrl-w twice" (LineEdit "" "three") (edits ["ctrl-w", "ctrl-w"] e)

test_words :: Assertion
test_words = do
  let e = LineEdit "a/b-cd " "ef gh"
  assertEqual "alt-b" (LineEdit "a/b-" "cd ef gh") (edits ["alt-b"] e)
  assertEqual "alt-b twice" (LineEdit "a/" "b-cd ef gh") (edits ["alt-b", "alt-b"] e)
  assertEqual "alt-f" (LineEdit "a/b-cd ef" " gh") (edits ["alt-f"] e)
  assertEqual "alt-d" (LineEdit "a/b-cd " " gh") (edits ["alt-d"] e)
  assertEqual "alt-backspace" (LineEdit "a/b-" "ef gh") (edits ["alt-backspace"] e)

test_notEditing :: Assertion
test_notEditing =
  forM_ ["enter", "escape", "ctrl-x", "up", "tab"] $ \k ->
    assertEqual (T.unpack k) Nothing (editLine (key k) (LineEdit "a" "b"))

test_visible :: Assertion
test_visible = do
  assertEqual "fits" ("abc", 1) (visibleLine 10 (LineEdit "a" "bc"))
  assertEqual "the cursor at the end" ("abc", 3) (visibleLine 10 (LineEdit "abc" ""))
  -- The cursor needs the last column after the text.
  assertEqual "scrolled" ("efg", 3) (visibleLine 4 (LineEdit "abcdefg" ""))
  assertEqual "scrolled in the middle" ("cdef", 3) (visibleLine 4 (LineEdit "abcde" "fgh"))
  assertEqual "wide characters" ("日本", 4) (visibleLine 5 (LineEdit "中日本" ""))
  assertEqual
    "a letter with its accent"
    ("e\x301\&e\x301\&e\x301\&e\x301", 4)
    (visibleLine 5 (LineEdit (T.replicate 10 "e\x301") ""))

-- | The keys move over and delete characters as people see them: a letter
-- with its accents, or emoji that a joiner joins.
test_graphemes :: Assertion
test_graphemes = do
  let accented = "e\x301"
      family = "👨\x200D👩\x200D👧"
  assertEqual
    "backspace"
    (LineEdit "a" "")
    (edits ["backspace"] (LineEdit ("a" <> accented) ""))
  assertEqual
    "backspace on emoji"
    (LineEdit "a" "")
    (edits ["backspace"] (LineEdit ("a" <> family) ""))
  assertEqual
    "delete"
    (LineEdit "a" "b")
    (edits ["delete"] (LineEdit "a" (accented <> "b")))
  assertEqual
    "left"
    (LineEdit "a" (accented <> "b"))
    (edits ["left"] (LineEdit ("a" <> accented) "b"))
  assertEqual
    "right"
    (LineEdit ("a" <> family) "b")
    (edits ["right"] (LineEdit "a" (family <> "b")))

----------------------------------------
-- Helpers

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec

-- | Apply keys that all edit.
edits :: [T.Text] -> LineEdit -> LineEdit
edits ks e0 =
  foldl'
    (\e k -> maybe (error ("doesn't edit: " <> T.unpack k)) id (editLine (key k) e))
    e0
    ks
