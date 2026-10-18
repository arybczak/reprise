module Reprise.Test.Find (findTests) where

import Control.Exception
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Optics.Core hiding (folded)
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Event
import Reprise.Find
import Reprise.Format
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Help
import Reprise.State
import Reprise.Test.Handler.Helpers
import Reprise.Test.Helpers
import Reprise.UI.Layout

findTests :: TestTree
findTests =
  testGroup
    "find"
    [ testCase "matching" test_matching
    , testCase "an incomplete pattern" test_incomplete
    , testCase "a pattern that is too slow" test_tooSlow
    , testCase "a search that is too slow" test_searchTooSlow
    , testCase "searching" test_search
    , testCase "where a pattern matches" test_matchRanges
    , testCase "find as you type" test_findAsYouType
    , testCase "find the next and the previous match" test_findAgain
    , testCase "find in text" test_findInText
    , testCase "find after the rows change" test_findRowsChange
    ]

test_matchRanges :: Assertion
test_matchRanges = do
  assertEqual "each match" (Right [(1, 1), (3, 1), (5, 1)]) (ranges "a" "banana")
  assertEqual "case" (Right [(0, 4)]) (ranges "beta" "BETA")
  assertEqual "diacritics" (Right [(0, 5)]) (ranges "pokoj" "Pokój")
  assertEqual "a mark after the match" (Right [(0, 2)]) (ranges "o" "o\x301x")
  assertEqual "a mark that isn't an accent" (Right []) (ranges "か" "か\x3099")
  -- The letters of a syllable compose into it, as in the match.
  assertEqual
    "a syllable of Hangul in letters"
    (Right [(0, 3)])
    (ranges "한" "\x1112\x1161\x11AB")
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
  -- The marks of Greek, Cyrillic, Hebrew and Arabic are accents, as of Latin.
  assertEqual "Greek" (Right True) (match "αθηνα" "Αθήνα")
  assertEqual "Cyrillic" (Right True) (match "елка" "ёлка")
  assertEqual "Hebrew" (Right True) (match "שלום" "שָׁלוֹם")
  -- In other scripts, a mark makes another letter or a vowel.
  assertEqual "Japanese" (Right False) (match "か" "が")
  assertEqual "Devanagari" (Right False) (match "हन" "हिन्")
  assertEqual "a decomposed mark after its letter" (Right False) (match "か" "か\x3099")
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
    let row = foldText (T.replicate 16 "a")
        rows = Seq.replicate 4000 row
    assertEqual "a row on its own" (Right False) (matches p row)
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

test_findAsYouType :: Assertion
test_findAsYouType = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  let cursorAfter ks = cursor <$> keys ks s
      note ks = do
        r <- keys ks s
        pure $ case r.state.prompt of
          Just (Prompt _ (Line _ (ForFind f) _)) -> f.note
          _ -> Just "no find"
  assertEqual "a match after the cursor" 3 =<< cursorAfter ("/" : typed "al")
  assertEqual "while typing" 2 =<< cursorAfter ("/" : typed "g")
  assertEqual "diacritics" 4 =<< cursorAfter ("/" : typed "pokoj")
  assertEqual "backward around the start" 3 =<< cursorAfter ("?" : typed "al")
  assertEqual "the note" (Just "wrapped around to the bottom") =<< note ("?" : typed "al")
  assertEqual "no match" (0, Just "no match")
    =<< ((,) <$> cursorAfter ("/" : typed "x") <*> note ("/" : typed "x"))
  assertEqual "an incomplete pattern" (Just "incomplete pattern") =<< note ("/" : typed "(")
  assertEqual "a cancel goes back" 0 =<< cursorAfter ("/" : typed "g" <> ["escape"])
  assertEqual "backspace goes back" 0 =<< cursorAfter ("/" : typed "g" <> ["backspace"])
  accepted <- keys ("/" : typed "al" <> ["enter"]) s
  assertEqual
    "kept"
    (3, Nothing)
    ((focusedView accepted.state).cursor, accepted.state.prompt)
  assertEqual "the pattern" (Just "al") accepted.state.findPattern
  assertEqual "on the visualizer" (Just "The visualizer screen has no find forward")
    . message
    =<< keys ["8", "/"] s

-- | The help of the default keys, on a view of 20 rows. A match goes to
-- the middle, as in a list.
test_findInText :: Assertion
test_findInText = do
  s <- testState (80, 24) (statusOf Stopped Nothing 1) (songs 1)
  help <- keys ["f1"] s
  let rows = map spansText (helpRows testAppEnv)
      withVolume = [i | (i, r) <- zip [0 ..] rows, "volume" `T.isInfixOf` T.toLower r]
      centered i = (i, max 0 (min (length rows - 20) (i - 10)))
      highlighted r =
        [ t
        | (a, t) <- imageSpans (renderScreen testAppEnv r.state)
        , T.toLower t == "volume"
        , V.attrStyle a /= V.Default
        ]
  (first, second, lastOne) <- case (withVolume, unsnoc withVolume) of
    (a : b : _ : _, Just (_, z)) -> pure (a, b, z)
    _ -> assertFailure $ "too few matches: " <> show withVolume
  typing <- keys ("/" : typed "volume") help.state
  assertEqual "while typing" (centered first) (position typing)
  assertBool "highlighted while typing" (not (null (highlighted typing)))
  found <- keys ["enter"] typing.state
  assertEqual "found" (centered first) (position found)
  assertBool "highlighted after" (not (null (highlighted found)))
  assertEqual "the next" (centered second) . position =<< keys ["."] found.state
  backward <- keys ("?" : typed "volume" <> ["enter"]) help.state
  assertEqual "backward around the start" (centered lastOne) (position backward)
  -- The first match leaves the view, so the next find starts at its top,
  -- not after the first match.
  let lastPage = length rows - 20
      afterTop = case [i | i <- withVolume, i >= lastPage] of
        i : _ -> i
        [] -> first
  assertBool "a find from the view differs" (afterTop /= second)
  scrolled <- keys ["end"] found.state
  assertEqual "from the view after a scroll" (centered afterTop) . position
    =<< keys ["."] scrolled.state
  queueFind <- keys ("/" : typed "volume" <> ["enter", "f1"]) s
  assertEqual "not after a find on another screen" [] (highlighted queueFind)
  assertEqual "not after an empty find" [] . highlighted =<< keys ["/", "enter"] found.state

test_findAgain :: Assertion
test_findAgain = do
  s <-
    keys ("/" : typed "al" <> ["enter"])
      =<< testState (80, 24) (statusOf Stopped Nothing 5) titled
  let findAfter ks = (\r -> (cursor r, message r)) <$> keys ks s.state
  assertEqual "next" (0, Just "Wrapped around to the top") =<< findAfter ["."]
  assertEqual "previous" (0, Nothing) =<< findAfter [","]
  assertEqual "previous twice" (3, Just "Wrapped around to the bottom")
    =<< findAfter [",", ","]
  cleared <- keys ["/", "enter"] s.state
  assertEqual
    "an empty find clears"
    (Nothing, Just "Find cleared")
    (cleared.state.findPattern, message cleared)
  assertEqual "nothing to find again" (Just "No find pattern") . message
    =<< keys ["."] cleared.state
  fresh <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  assertEqual "nothing yet" (Just "No find pattern") . message =<< keys ["."] fresh

-- | A find matches the rows of the queue and the display of now, not the
-- ones of an earlier find.
test_findRowsChange :: Assertion
test_findRowsChange = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  missed <- keys ("/" : typed "delta" <> ["enter"]) s
  let added = statusOf Stopped Nothing 6 & #playlistVersion .~ PlaylistVersion 2
  changed <-
    runEvents
      0
      [QueueFetched (added, titled <> [song 5 [(Title, ["delta"])] 60])]
      missed.state
  assertEqual "a song added since" 5 . cursor =<< keys ["."] changed.state
  -- Only the classic display joins the artist and the title with " - ".
  assertEqual "another display" 3 . cursor
    =<< keys ("/" : typed "A - alpha" <> ["enter", "t", "d", "."]) s
