module StyleTests (styleTests) where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Graphics.Vty qualified as V
import Optics.Core
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
    , testCase "the red, green and blue of the chart" test_colorRgb
    , testCase "gradient" test_gradient
    , testCase "the shades that the eye tells apart" test_distinctShades
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
  assertEqual
    "red, green and blue"
    (Right $ Style (Just (Rgb 0 255 128)) (Just (Rgb 0x1a 0x2b 0x3c)) S.empty)
    (parseStyle "#00ff80 on #1A2B3C")

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
  assertEqual
    "too few digits"
    (Left "a color of red, green and blue must be # and 6 hexadecimal digits, not #00ff8")
    (parseStyle "#00ff8")
  assertEqual
    "not hexadecimal"
    (Left "a color of red, green and blue must be # and 6 hexadecimal digits, not #00gg00")
    (parseStyle "#00gg00")

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
    , "#00ff80 bold on #1a2b3c"
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
  assertEqual
    "red, green and blue"
    (V.SetTo (V.RGBColor 0 255 128))
    (V.attrForeColor (toAttr WithColors (fg (Rgb 0 255 128))))

-- | xterm's chart.
test_colorRgb :: Assertion
test_colorRgb = do
  assertEqual "the cube's first" (Just (0, 0, 0)) (colorRgb (Color 16))
  assertEqual "red" (Just (255, 0, 0)) (colorRgb (Color 196))
  assertEqual "the cube's last" (Just (255, 255, 255)) (colorRgb (Color 231))
  assertEqual "the first gray" (Just (8, 8, 8)) (colorRgb (Color 232))
  assertEqual "the last gray" (Just (238, 238, 238)) (colorRgb (Color 255))
  assertEqual "an ISO color" Nothing (colorRgb (Color 1))
  assertEqual "the terminal's own" Nothing (colorRgb DefaultColor)

-- | Green and yellow are 0.193 apart in Oklab, so 10 steps of 0.02 at
-- most, and 2 such pairs need 20.
test_distinctShades :: Assertion
test_distinctShades = do
  let green = fg (Color 46)
      yellow = fg (Color 226)
  assertEqual "one stop" 1 (distinctShades (green NE.:| []))
  assertEqual "two stops" 11 (distinctShades (green NE.:| [yellow]))
  assertEqual "three stops" 21 (distinctShades (green NE.:| [yellow, green]))
  assertEqual
    "ISO colors don't blend"
    3
    (distinctShades (fg (Color 1) NE.:| [fg (Color 2), fg (Color 3)]))

test_gradient :: Assertion
test_gradient = do
  let black = fg (Rgb 0 0 0)
      white = fg (Rgb 255 255 255)
  assertEqual "one stop" [black, black] (gradient (black NE.:| []) 2)
  assertEqual "one step" [black] (gradient (black NE.:| [white]) 1)
  case gradient (black NE.:| [white]) 3 of
    [first, Style (Just (Rgb r g b)) Nothing _, final] -> do
      assertEqual "the ends" (black, white) (first, final)
      assertEqual "gray" (r, r) (g, b)
      -- Oklab's lightness of 0.5, which sRGB has darker than its middle.
      assertBool ("the middle: " <> show r) (r > 90 && r < 110)
    styles -> assertFailure $ "styles: " <> show styles
  let green = fg (Color 46)
      red = fg (Color 196)
  case gradient (green NE.:| [fg (Color 226), red]) 5 of
    [first, _, yellow, _, final] ->
      assertEqual
        "the stops stay colors of the chart"
        (green, fg (Color 226), red)
        (first, yellow, final)
    styles -> assertFailure $ "styles: " <> show styles
  assertEqual
    "ISO colors don't blend"
    [fg (Color 1), fg (Color 1), fg (Color 4), fg (Color 4)]
    (gradient (fg (Color 1) NE.:| [fg (Color 4)]) 4)
  let boldBlack = black & #attributes .~ S.fromList [Bold]
  case gradient (boldBlack NE.:| [white]) 5 of
    [_, nearBlack, _, nearWhite, _] -> do
      assertEqual "the nearer stop's attributes" (S.fromList [Bold]) nearBlack.attributes
      assertEqual "the other stop's attributes" S.empty nearWhite.attributes
    styles -> assertFailure $ "styles: " <> show styles

fg :: Color -> Style
fg c = Style (Just c) Nothing S.empty
