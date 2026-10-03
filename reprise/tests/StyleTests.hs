module StyleTests (styleTests) where

import Data.Set qualified as S
import Graphics.Vty qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Style

styleTests :: TestTree
styleTests =
  testGroup
    "Style"
    [ testCase "parse" test_parse
    , testCase "parse errors" test_parseErrors
    , testCase "render" test_render
    , testCase "overlay" test_overlay
    , testCase "vty attributes" test_vtyAttributes
    ]

test_parse :: Assertion
test_parse = do
  assertEqual
    "foreground and background"
    (Right $ Style (Just (Color 3)) (Just (Color 24)) S.empty)
    (parseStyle "yellow on 24")
  assertEqual
    "attributes"
    (Right $ Style (Just (Color 0)) Nothing (S.fromList [Bold]))
    (parseStyle "black bold")
  assertEqual
    "default"
    (Right $ Style (Just DefaultColor) Nothing S.empty)
    (parseStyle "default")
  assertEqual "any order" (parseStyle "bold red on blue") (parseStyle "red on blue bold")
  assertEqual
    "only a background"
    (Right $ Style Nothing (Just (Color 237)) S.empty)
    (parseStyle "on 237")

test_parseErrors :: Assertion
test_parseErrors = do
  assertEqual "empty" (Left "a style can't be empty") (parseStyle "")
  assertEqual
    "out of range"
    (Left "a color number must be from 0 to 255, not 256")
    (parseStyle "256")
  assertEqual
    "two foregrounds"
    (Left "the foreground is set twice; write \"on blue\" for a background")
    (parseStyle "red blue")
  assertBool "unknown word" (either (const True) (const False) $ parseStyle "purple")
  assertEqual
    "on without a color"
    (Left "expected a color after \"on\"")
    (parseStyle "red on")

test_render :: Assertion
test_render =
  mapM_
    (\t -> assertEqual (show t) (Right t) (renderStyle <$> parseStyle t))
    [ "yellow on 24"
    , "black bold"
    , "default"
    , "221"
    , "bold underline"
    , "on 237"
    , "white italic reverse on default"
    ]

test_overlay :: Assertion
test_overlay = do
  let base = Style (Just (Color 3)) Nothing (S.fromList [Bold])
      cursor = Style Nothing (Just (Color 24)) S.empty
  assertEqual
    "the top changes only what it sets"
    (Style (Just (Color 3)) (Just (Color 24)) (S.fromList [Bold]))
    (base <> cursor)

test_vtyAttributes :: Assertion
test_vtyAttributes = do
  let s = Style (Just (Color 3)) (Just (Color 221)) (S.fromList [Bold])
  assertEqual "ISO color" (V.SetTo (V.ISOColor 3)) (V.attrForeColor (toAttr WithColors s))
  assertEqual
    "240 colors"
    (V.SetTo (V.Color240 205))
    (V.attrBackColor (toAttr WithColors s))
  assertEqual "no colors" V.Default (V.attrForeColor (toAttr NoColors s))
  assertEqual
    "attributes stay without colors"
    (V.SetTo V.bold)
    (V.attrStyle (toAttr NoColors s))
