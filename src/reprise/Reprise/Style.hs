-- | Styles: colors and attributes, e.g. @yellow on 24@ or @black bold@.
module Reprise.Style
  ( -- * Styles
    Style (..)
  , Color (..)
  , StyleAttribute (..)
  , parseStyle
  , renderStyle

    -- * Conversion to vty
  , ColorMode (..)
  , toAttr
  ) where

import Data.Bits
import Data.Char
import Data.List qualified as L
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
