-- | Character widths that match the terminal.
--
-- vty computes every width, also the ones in "Reprise.Format", with its
-- built-in table from Unicode 5.0, unless a custom table is installed. In
-- that table an emoji is narrow, but terminals draw it wide, so a row with
-- one doesn't fit.
module Reprise.Width
  ( installWidthTable
  , systemWidth
  ) where

import Control.Monad
import Foreign.C.Types
import Graphics.Text.Width qualified as W
import Graphics.Vty.UnicodeWidthTable.Install qualified as V
import Graphics.Vty.UnicodeWidthTable.Query qualified as V

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
    V.installUnicodeWidthTable
      =<< V.buildUnicodeWidthTable width V.defaultUnicodeTableUpperBound

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
