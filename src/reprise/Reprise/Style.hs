-- | Styles: colors and attributes, e.g. @yellow on 24@ or @black bold@.
module Reprise.Style
  ( -- * Styles
    Style (..)
  , Color (..)
  , StyleAttribute (..)
  , boldStyle
  , parseStyle
  , renderStyle

    -- * Conversion to vty
  , ColorMode (..)
  , toAttr

    -- * Gradients
  , gradient
  , distinctShades
  , colorRgb
  ) where

import Data.Bits
import Data.Char
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Word
import GHC.Generics
import Graphics.Vty qualified as V
import Optics.Core hiding (view)
import Yamlet

-- | A style changes only what it sets. A style laid over another with '<>'
-- keeps the colors that it doesn't set and adds its attributes.
data Style = Style
  { foreground :: Maybe Color
  , background :: Maybe Color
  , attributes :: S.Set StyleAttribute
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | The right style is laid over the left one.
instance Semigroup Style where
  a <> b =
    Style
      { foreground = maybe a.foreground Just b.foreground
      , background = maybe a.background Just b.background
      , attributes = a.attributes <> b.attributes
      }

instance Monoid Style where
  mempty = Style Nothing Nothing S.empty

-- | Bold, over any colors.
boldStyle :: Style
boldStyle = mempty & #attributes .~ S.singleton Bold

data Color
  = -- | The terminal's own color.
    DefaultColor
  | -- | A color of the standard 256 color chart. The first 8 have names.
    Color Word8
  | -- | A color by its red, green and blue, which a terminal without 24-bit
    -- colors shows as the nearest of the chart.
    Rgb Word8 Word8 Word8
  deriving stock (Eq, Ord, Show)

data StyleAttribute = Bold | Italic | Underline | Reverse
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Parse a style: an optional foreground color, attributes, and
-- @on \<color\>@ for a background, in any order.
parseStyle :: T.Text -> Either T.Text Style
parseStyle input = case T.words input of
  [] -> Left "a style can't be empty"
  ws -> go mempty ws
  where
    go :: Style -> [T.Text] -> Either T.Text Style
    go style = \case
      [] -> Right style
      "on" : rest -> case rest of
        [] -> Left "expected a color after \"on\""
        w : rest' -> case parseColor w of
          Right c
            | Just _ <- style.background -> Left "the background is set twice"
            | otherwise -> go (style & #background ?~ c) rest'
          Left err -> Left err
      w : rest
        | Just a <- lookup w attributeNames ->
            if a `S.member` style.attributes
              then Left $ "the attribute " <> w <> " is set twice"
              else go (style & #attributes %~ S.insert a) rest
        | otherwise -> case parseColor w of
            Right c
              | Just _ <- style.foreground ->
                  Left $ "the foreground is set twice; write \"on " <> w <> "\" for a background"
              | otherwise -> go (style & #foreground ?~ c) rest
            Left err -> Left err

    parseColor :: T.Text -> Either T.Text Color
    parseColor w
      | w == "default" = Right DefaultColor
      | Just i <- L.elemIndex w colorNames = Right . Color $ fromIntegral i
      | T.all isDigit w = case T.decimal @Integer w of
          Right (n, "")
            | n <= toInteger (maxBound @Word8) -> Right . Color $ fromInteger n
          _ -> Left $ "a color number must be from 0 to 255, not " <> w
      | Just hex <- T.stripPrefix "#" w =
          if T.length hex == 6 && T.all isHexDigit hex
            then Right $ Rgb (byte hex 0) (byte hex 2) (byte hex 4)
            else Left $ "a color of red, green and blue must be # and 6 hexadecimal digits, not " <> w
      | otherwise =
          Left $
            "unknown color or attribute "
              <> w
              <> ", expected a number from 0 to 255, #rrggbb, default, one of: "
              <> T.intercalate ", " colorNames
              <> ", or one of: "
              <> T.intercalate ", " (map fst attributeNames)

    -- The byte of the two hexadecimal digits at an index.
    byte :: T.Text -> Int -> Word8
    byte hex i = fromIntegral (digitToInt (T.index hex i) * 16 + digitToInt (T.index hex (i + 1)))

-- | The text of a style, which 'parseStyle' reads back.
renderStyle :: Style -> T.Text
renderStyle style =
  T.unwords $
    maybe [] (pure . colorName) style.foreground
      <> [name | (name, a) <- attributeNames, a `S.member` style.attributes]
      <> maybe [] (\c -> ["on", colorName c]) style.background
  where
    colorName :: Color -> T.Text
    colorName = \case
      DefaultColor -> "default"
      Color n
        | fromIntegral n < length colorNames -> colorNames !! fromIntegral n
        | otherwise -> T.pack (show n)
      Rgb r g b -> "#" <> foldMap hex [r, g, b]

    hex :: Word8 -> T.Text
    hex b = T.pack [intToDigit (fromIntegral (b `div` 16)), intToDigit (fromIntegral (b `mod` 16))]

colorNames :: [T.Text]
colorNames = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]

attributeNames :: [(T.Text, StyleAttribute)]
attributeNames =
  [ ("bold", Bold)
  , ("italic", Italic)
  , ("underline", Underline)
  , ("reverse", Reverse)
  ]

-- | An unquoted color number is an integer, so it is accepted as text.
instance FromYaml Style where
  parseYaml n = case view n of
    StringView t -> parse t
    IntView i -> parse . T.pack $ show i
    _ -> typeMismatch "a style, e.g. yellow on 24" n
    where
      parse :: T.Text -> Parser Style
      parse = either (failAt n . T.unpack) pure . parseStyle

----------------------------------------
-- Conversion to vty

data ColorMode
  = WithColors
  | -- | The @NO_COLOR@ environment variable is set. Attributes stay.
    NoColors
  deriving stock (Eq, Show)

-- | The vty attribute of a style. A color that the style doesn't set is the
-- terminal's own.
toAttr :: ColorMode -> Style -> V.Attr
toAttr mode style =
  V.Attr
    { V.attrStyle = case S.toList style.attributes of
        [] -> V.Default
        as -> V.SetTo $ foldr ((.|.) . vtyStyle) 0 as
    , V.attrForeColor = color style.foreground
    , V.attrBackColor = color style.background
    , V.attrURL = V.Default
    }
  where
    color :: Maybe Color -> V.MaybeDefault V.Color
    color = \case
      Just (Color n) | mode == WithColors -> V.SetTo (vtyColor n)
      Just (Rgb r g b) | mode == WithColors -> V.SetTo (V.RGBColor r g b)
      _ -> V.Default

    -- vty numbers the colors after the ISO ones from 0.
    vtyColor :: Word8 -> V.Color
    vtyColor n
      | n < isoColors = V.ISOColor n
      | otherwise = V.Color240 (n - isoColors)

    vtyStyle :: StyleAttribute -> V.Style
    vtyStyle = \case
      Bold -> V.bold
      Italic -> V.italic
      Underline -> V.underline
      Reverse -> V.reverseVideo

-- | The first 16 colors of the chart are the ISO colors, which terminals
-- theme.
isoColors :: Word8
isoColors = 16

----------------------------------------
-- Gradients

-- | A number of styles evenly from the first of some to the last, through
-- them all. Between two of them, the foreground blends in Oklab, so that
-- the steps look even, and the rest is the nearer one's. A foreground
-- whose red, green and blue the chart doesn't fix, e.g. an ISO color,
-- doesn't blend.
gradient :: NE.NonEmpty Style -> Int -> [Style]
gradient stops n = map styleAt [0 .. n - 1]
  where
    -- At an exact position of a stop, the stop itself, so that a color of
    -- the chart stays one.
    styleAt :: Int -> Style
    styleAt k
      | m == 1 || n == 1 = NE.head stops
      | otherwise =
          let (i, r) = (k * (m - 1)) `divMod` (n - 1)
          in if r == 0
               then stopAt i
               else blend (stopAt i) (stopAt (i + 1)) (fromIntegral r / fromIntegral (n - 1))

    blend :: Style -> Style -> Double -> Style
    blend a b f =
      let nearer = if f < 0.5 then a else b
      in case (colorRgb =<< a.foreground, colorRgb =<< b.foreground) of
           (Just x, Just y) -> nearer & #foreground ?~ mix x y f
           _ -> nearer

    stopAt :: Int -> Style
    stopAt i = stops NE.!! i

    m :: Int
    m = NE.length stops

-- | The fewest styles of a 'gradient' through some in which neighbours
-- differ by a just noticeable difference at most, so that more would look
-- the same. The stops are among them.
distinctShades :: NE.NonEmpty Style -> Int
distinctShades stops =
  1 + (NE.length stops - 1) * maximum (1 : zipWith steps (NE.toList stops) (NE.tail stops))
  where
    steps :: Style -> Style -> Int
    steps a b = case (colorRgb =<< a.foreground, colorRgb =<< b.foreground) of
      (Just x, Just y) -> ceiling (distance (toOklab x) (toOklab y) / justNoticeable)
      _ -> 1

    distance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
    distance (l1, a1, b1) (l2, a2, b2) = sqrt ((l1 - l2) ^ (2 :: Int) + (a1 - a2) ^ (2 :: Int) + (b1 - b2) ^ (2 :: Int))

    -- The difference in Oklab that CSS Color 4's gamut mapping takes as
    -- just noticeable.
    justNoticeable :: Double
    justNoticeable = 0.02

-- | The red, green and blue of a color, if the chart fixes them, as xterm's
-- chart does: a cube of 6 levels of each from 16, and 24 grays from 232.
colorRgb :: Color -> Maybe (Word8, Word8, Word8)
colorRgb = \case
  DefaultColor -> Nothing
  Rgb r g b -> Just (r, g, b)
  Color n
    | n < isoColors -> Nothing
    | n < grays ->
        let i = n - isoColors
        in Just (level (i `div` 36), level (i `div` 6 `mod` 6), level (i `mod` 6))
    | otherwise -> let v = 8 + 10 * (n - grays) in Just (v, v, v)
  where
    level :: Word8 -> Word8
    level k = if k == 0 then 0 else 55 + 40 * k

    grays :: Word8
    grays = isoColors + 6 * 6 * 6

-- | The color a part of the way from one to another, in Oklab.
mix :: (Word8, Word8, Word8) -> (Word8, Word8, Word8) -> Double -> Color
mix x y f =
  let (l1, a1, b1) = toOklab x
      (l2, a2, b2) = toOklab y
      between u v = u + f * (v - u)
  in fromOklab (between l1 l2, between a1 a2, between b1 b2)

-- | Björn Ottosson's Oklab of an sRGB color.
toOklab :: (Word8, Word8, Word8) -> (Double, Double, Double)
toOklab (r8, g8, b8) =
  let r = linear r8
      g = linear g8
      b = linear b8
      l = cbrt (0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)
      m = cbrt (0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)
      s = cbrt (0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)
  in ( 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s
     , 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s
     , 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
     )
  where
    linear :: Word8 -> Double
    linear c =
      let v = fromIntegral c / 255
      in if v <= 0.04045 then v / 12.92 else ((v + 0.055) / 1.055) ** 2.4

    cbrt :: Double -> Double
    cbrt v = v ** (1 / 3)

-- | The sRGB color of an Oklab one, at the nearest in the sRGB gamut.
fromOklab :: (Double, Double, Double) -> Color
fromOklab (lightness, a, b) =
  let l = (lightness + 0.3963377774 * a + 0.2158037573 * b) ^ (3 :: Int)
      m = (lightness - 0.1055613458 * a - 0.0638541728 * b) ^ (3 :: Int)
      s = (lightness - 0.0894841775 * a - 1.2914855480 * b) ^ (3 :: Int)
  in Rgb
       (gammaEncoded (4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s))
       (gammaEncoded (-1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s))
       (gammaEncoded (-0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s))
  where
    gammaEncoded :: Double -> Word8
    gammaEncoded c =
      let v = max 0 (min 1 c)
          gamma = if v <= 0.0031308 then 12.92 * v else 1.055 * v ** (1 / 2.4) - 0.055
      in round (255 * gamma)
