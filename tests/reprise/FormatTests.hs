module FormatTests (formatTests) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Reprise.Format
import Reprise.Mpd.Protocol.Types
import Reprise.Style

formatTests :: TestTree
formatTests =
  testGroup
    "Format"
    [ testCase "parse" test_parse
    , testCase "parse errors" test_parseErrors
    , testCase "plain formats reject styles" test_plainRejectsStyles
    , testCase "missing tags" test_missingTags
    , testCase "alternatives" test_alternatives
    , testCase "status bar format" test_statusBarFormat
    , testCase "styles" test_styles
    , testCase "fields" test_fields
    , testCase "width" test_width
    , testProperty "print then parse" prop_printParse
    ]

test_parse :: Assertion
test_parse = do
  assertEqual
    "fields and literals"
    ( Right $
        Format [FieldItem ArtistField Nothing, Literal " - ", FieldItem TitleField (Just 30)]
    )
    (parseStyledFormat "%{artist} - %{title:30}")
  assertEqual
    "escapes"
    (Right $ Format [Literal "%[]|<"])
    (parseStyledFormat "%%%[%]%|%<")
  assertEqual
    "alternatives"
    ( Right $
        Format
          [ Alternatives
              [ [FieldItem ArtistField Nothing, Literal " - "]
              , []
              ]
          ]
    )
    (parseStyledFormat "[%{artist} - |]")
  assertEqual
    "styled span"
    (Right $ Format [Styled (style "red bold") [Literal "x"], Literal ">"])
    (parseStyledFormat "<red bold>x</>>")

test_parseErrors :: Assertion
test_parseErrors = do
  let err i msg = Left (FormatError i msg)
  assertEqual
    "old tag"
    (err 0 "%a is not a tag; tags are written %{artist}, %{title}, ...")
    (parseStyledFormat "%a")
  assertEqual "unknown tag" (Left 2) (position $ parseStyledFormat "%{foo}")
  assertEqual "unclosed [" (Left 3) (position $ parseStyledFormat "ab [x")
  assertEqual "] without [" (Left 1) (position $ parseStyledFormat "a]")
  assertEqual "| outside [" (Left 1) (position $ parseStyledFormat "a|b")
  assertEqual "unclosed span" (Left 0) (position $ parseStyledFormat "<red>x")
  assertEqual "</> without a span" (Left 1) (position $ parseStyledFormat "x</>")
  assertEqual "unbalanced span in [" (Left 7) (position $ parseStyledFormat "[<red>a|b</>]")
  assertEqual "bad style" (Left 1) (position $ parseStyledFormat "<purple>x</>")
  assertEqual "bad width" (Left 8) (position $ parseStyledFormat "%{title:x}")
  assertEqual "zero width" (Left 8) (position $ parseStyledFormat "%{title:0}")
  assertEqual "% at the end" (Left 1) (position $ parseStyledFormat "a%")
  where
    position :: Either FormatError a -> Either Int ()
    position = either (Left . (.position)) (const $ Right ())

test_plainRejectsStyles :: Assertion
test_plainRejectsStyles =
  assertEqual
    "error"
    (Left $ FormatError 1 "this format can't contain styles")
    (() <$ parsePlainFormat "<red>x</>")

test_missingTags :: Assertion
test_missingTags = do
  assertEqual "marker at the top level" "<empty> - t" (plain "%{artist} - %{title}" titled)
  assertEqual "nothing in brackets" "t" (plain "[%{artist} - ]%{title}" titled)
  assertEqual "only the bracket is missing" "x t" (plain "x [%{artist} ]%{title}" titled)

test_alternatives :: Assertion
test_alternatives = do
  assertEqual "first present" "t" (plain "[%{title}|%{file}]" titled)
  assertEqual "second" "a/b.flac" (plain "[%{artist}|%{file}]" titled)
  assertEqual "none" "" (plain "[%{artist}|%{album}]" titled)
  assertEqual "literal alternative" "?" (plain "[%{artist}|?]" titled)
  assertEqual "nested" "t" (plain "[[%{artist} - ]%{title}|%{file}]" titled)

-- | ncmpcpp printed @Artist "Album" - @ for a song without a title.
test_statusBarFormat :: Assertion
test_statusBarFormat = do
  let fmt = "[[%{artist}[ \"%{album}\"[ (%{year})]] - ]%{title}|%{filename}]"
      noTitle = song "a/b.flac" [(Artist, ["A"]), (Album, ["B"])]
      full =
        song "a/b.flac" [(Artist, ["A"]), (Album, ["B"]), (Title, ["T"]), (Date, ["2001-02-03"])]
  assertEqual "no title" "b.flac" (plain fmt noTitle)
  assertEqual "everything" "A \"B\" (2001) - T" (plain fmt full)

test_styles :: Assertion
test_styles = do
  let ctx = RenderContext " | " [Span (Just (style "cyan")) "?"]
  assertEqual
    "nested styles combine"
    [Span (Just (style "red")) "a", Span (Just (style "red bold")) "b"]
    (renderFormat ctx titled (styled "<red>a<bold>b</></>"))
  assertEqual
    "neighbours with the same style merge"
    [Span Nothing "a t"]
    (renderFormat ctx titled (styled "a %{title}"))
  assertEqual
    "the marker gets the surrounding style laid under its own"
    [Span (Just (style "cyan bold")) "?"]
    (renderFormat ctx titled (styled "<bold>%{artist}</>"))

test_fields :: Assertion
test_fields = do
  let s =
        ( song
            "dir/sub/file.flac"
            [(Artist, ["A", "B", "A"]), (Track, ["3/12"]), (Date, ["1999-05"])]
        )
          { duration = Just 3725
          , priority = 7
          }
      value f = fieldValue " | " s f
  assertEqual "multiple values without duplicates" (Just "A | B") (value ArtistField)
  assertEqual "track" (Just "03") (value TrackField)
  assertEqual "raw track" (Just "3/12") (value TrackRawField)
  assertEqual "year" (Just "1999") (value YearField)
  assertEqual "length" (Just "1:02:05") (value LengthField)
  assertEqual "priority" (Just "7") (value PriorityField)
  assertEqual "filename" (Just "file.flac") (value FilenameField)
  assertEqual "directory" (Just "dir/sub") (value DirectoryField)
  assertEqual "no directory" Nothing (fieldValue " | " (song "f.flac" []) DirectoryField)
  assertEqual "short length" "0:05" (formatDuration 5.9)

test_width :: Assertion
test_width =
  assertEqual "field width" "ab…" (plain "%{title:3}" (song "x" [(Title, ["abcdef"])]))

prop_printParse :: Property
prop_printParse = forAllShrink genFormat shrinkFormat $ \fmt ->
  let printed = printFormat renderStyle fmt
  in counterexample (T.unpack printed) $ parseStyledFormat printed === Right fmt

----------------------------------------
-- Helpers

style :: T.Text -> Style
style = either (error . T.unpack) id . parseStyle

styled :: T.Text -> Format Style
styled = either (error . show) id . parseStyledFormat

plain :: T.Text -> Song -> T.Text
plain fmt s = case parsePlainFormat fmt of
  Right f -> renderPlain (RenderContext " | " [Span Nothing "<empty>"]) s f
  Left err -> error (show err)

song :: T.Text -> [(Tag, [T.Text])] -> Song
song file tags =
  Song
    { file = file
    , tags = M.fromList tags
    , duration = Nothing
    , range = Nothing
    , lastModified = Nothing
    , format = Nothing
    , position = Nothing
    , songId = Nothing
    , priority = 0
    }

titled :: Song
titled = song "a/b.flac" [(Title, ["t"])]

-- | A format as the parser returns it: no empty literals, no neighbouring
-- literals.
genFormat :: Gen (Format Style)
genFormat = Format . normalize <$> sized genItems
  where
    genItems :: Int -> Gen [Item Style]
    genItems n = do
      k <- chooseInt (0, min 4 n)
      vectorOf k (genItem (n `div` 2))

    genItem :: Int -> Gen (Item Style)
    genItem n =
      oneof $
        [ Literal . T.pack <$> listOf1 (elements "ab %[]|<>-/:{}")
        , FieldItem <$> elements fields <*> oneof [pure Nothing, Just <$> chooseInt (1, 50)]
        ]
          <> if n > 0
            then
              [ Alternatives <$> (map normalize <$> listOf1' (genItems n))
              , Styled <$> genStyle <*> (normalize <$> genItems n)
              ]
            else []

    listOf1' :: Gen a -> Gen [a]
    listOf1' g = do
      k <- chooseInt (1, 3)
      vectorOf k g

    genStyle :: Gen Style
    genStyle =
      (\fg bg as -> Style fg bg (S.fromList as))
        <$> genColor
        <*> genColor
        <*> sublistOf [minBound .. maxBound]
          `suchThat` (/= mempty)

    genColor :: Gen (Maybe Color)
    genColor = oneof [pure Nothing, pure (Just DefaultColor), Just . Color <$> arbitrary]

normalize :: [Item Style] -> [Item Style]
normalize = \case
  Literal a : Literal b : rest -> normalize (Literal (a <> b) : rest)
  Literal "" : rest -> normalize rest
  item : rest -> item : normalize rest
  [] -> []

shrinkFormat :: Format Style -> [Format Style]
shrinkFormat (Format items) = [Format (normalize is) | is <- shrinkList shrinkItem items]
  where
    shrinkItem :: Item Style -> [Item Style]
    shrinkItem = \case
      Alternatives as -> concat as `seq` [Alternatives [normalize a] | a <- as]
      Styled _ is -> [Literal "x" | not (null is)]
      _ -> []
