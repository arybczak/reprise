-- | Samples that several visualizer test modules use.
module Reprise.Test.Visualizer.Helpers
  ( samples
  , circle
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Int
import GHC.ByteOrder

-- | A frame of samples of the left and the right channel, as MPD writes
-- them.
samples :: [(Int16, Int16)] -> BS.ByteString
samples = BL.toStrict . BB.toLazyByteString . foldMap (\(l, r) -> sample l <> sample r)
  where
    sample :: Int16 -> BB.Builder
    sample = case targetByteOrder of
      LittleEndian -> BB.int16LE
      BigEndian -> BB.int16BE

-- | A frame with a period of a sine on the left and a cosine on the right,
-- which is a circle.
circle :: BS.ByteString
circle =
  samples
    [ (scaled (sin a), scaled (cos a))
    | i <- [0 .. n - 1]
    , let a = 2 * pi * fromIntegral i / fromIntegral n
    ]
  where
    n :: Int
    n = 44100 `div` 60

    scaled :: Double -> Int16
    scaled = round . (* 30000)
