-- | Character widths that match the terminal, and text measured and cut by
-- them.
--
-- vty computes every width, also the ones here, with its built-in table
-- from Unicode 5.0, unless a custom table is installed. In that table an
-- emoji is narrow, but terminals draw it wide, so a row with one doesn't
-- fit.
module Reprise.Width
  ( -- * Text width
    textWidth
  , takeWidth
  , truncateToWidth
  , ellipsis
  , scrollText

    -- * The width table
  , installWidthTable
  , systemWidth
  , widthRanges
  ) where

import Control.Monad
import Data.Char
import Data.Text qualified as T
import Data.Word
import Foreign.C.Types
import Graphics.Text.Width qualified as W
import Graphics.Vty.UnicodeWidthTable.Install qualified as V
import Graphics.Vty.UnicodeWidthTable.Query qualified as V
import Graphics.Vty.UnicodeWidthTable.Types qualified as V

-- | The width of text in terminal columns. Wide characters count as 2.
textWidth :: T.Text -> Int
textWidth = T.foldl' (\w c -> w + W.safeWcwidth c) 0

-- | The longest prefix of text that fits in the width.
takeWidth :: Int -> T.Text -> T.Text
takeWidth w = T.pack . go w . T.unpack
  where
    go :: Int -> String -> String
    go _ [] = []
    go n (c : cs)
      | W.safeWcwidth c <= n = c : go (n - W.safeWcwidth c) cs
      | otherwise = []

-- | Shorten text to at most the given width, with an ellipsis at the end if
-- it was too wide.
truncateToWidth :: Int -> T.Text -> T.Text
truncateToWidth width t
  | textWidth t <= width = t
  | otherwise = takeWidth (width - textWidth ellipsis) t <> ellipsis

ellipsis :: T.Text
ellipsis = "…"

-- | Text that doesn't fit in the width, scrolled by a character for each
-- step, e.g. each second. It goes round with a separator.
scrollText :: Int -> Int -> T.Text -> T.Text
scrollText width steps t =
  let looped = t <> separator
      start = steps `mod` max 1 (T.length looped)
  in takeWidth width (T.drop start looped <> looped)
  where
    separator :: T.Text
    separator = " ** "

foreign import ccall unsafe "reprise_use_utf8_ctype"
  c_useUtf8Ctype :: IO CInt

foreign import ccall unsafe "reprise_wcwidth"
  c_wcwidth :: CInt -> IO CInt

-- | Install a width table for vty from the widths of the C library, which
-- knows a recent Unicode version. Without a UTF-8 locale, vty keeps its
-- built-in table.
installWidthTable :: IO ()
installWidthTable = do
  widths <- systemWidth
  forM_ widths $ \width ->
    V.installUnicodeWidthTable . V.UnicodeWidthTable
      =<< widthRanges width V.defaultUnicodeTableUpperBound

-- | The ranges of neighbouring characters with the same width, up to a
-- character, as vty's 'V.buildUnicodeWidthTable' makes them: control,
-- unassigned and surrogate characters are left out. vty's function keeps
-- every character with its width in a list first, after which reprise kept
-- 30 MB more memory (measured with an empty queue). This one keeps only the
-- ranges.
widthRanges :: (Char -> IO Int) -> Char -> IO [V.WidthTableRange]
widthRanges width upper = go 0 0 0 0 []
  where
    go :: Int -> Word32 -> Word32 -> Word8 -> [V.WidthTableRange] -> IO [V.WidthTableRange]
    go !i !start !size !columns done
      | i > fromEnum upper = pure . reverse $ close start size columns done
      | not (considered (toEnum i)) = go (i + 1) start size columns done
      | otherwise = do
          w <- fromIntegral <$> width (toEnum i)
          let code = fromIntegral i
          if size > 0 && start + size == code && columns == w
            then go (i + 1) start (size + 1) columns done
            else go (i + 1) code 1 w (close start size columns done)

    -- Finish the current range, if there is one.
    close :: Word32 -> Word32 -> Word8 -> [V.WidthTableRange] -> [V.WidthTableRange]
    close start size columns done
      | size > 0 = V.WidthTableRange start size columns : done
      | otherwise = done

    considered :: Char -> Bool
    considered c = generalCategory c `notElem` [Control, NotAssigned, Surrogate]

-- | The width of a character by the C library, if it has a UTF-8 locale.
-- A character that the C library doesn't know gets vty's built-in width.
systemWidth :: IO (Maybe (Char -> IO Int))
systemWidth = do
  ok <- c_useUtf8Ctype
  pure $
    if ok == 0
      then Nothing
      else Just $ \c -> do
        w <- c_wcwidth (fromIntegral (fromEnum c))
        pure $ if w >= 0 then fromIntegral w else W.safeWcwidth c
