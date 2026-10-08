-- | The samples that MPD's fifo output writes for the visualizer, in the
-- format @44100:16:2@: 44100 samples a second of 16 bits, of the left
-- channel and the right one in turn.
module Reprise.Visualizer.Samples
  ( sampleRate
  , bytesPerSample
  , channels
  , frameBytes
  , sampleAt
  , lowestFrequency
  , highestFrequency
  , pushOut
  ) where

import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Int
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word
import GHC.ByteOrder

sampleRate :: Int
sampleRate = 44100

bytesPerSample :: Int
bytesPerSample = 2

channels :: Int
channels = 2

-- | The bytes of a sample of every channel.
frameBytes :: Int
frameBytes = bytesPerSample * channels

-- | The sample at an index, from -1 to 1. MPD writes them in the byte order
-- of the machine. The index must be in the samples.
sampleAt :: BS.ByteString -> Int -> Double
sampleAt pcm i =
  let a = BS.unsafeIndex pcm (bytesPerSample * i)
      b = BS.unsafeIndex pcm (bytesPerSample * i + 1)
      (low, high) = case targetByteOrder of
        LittleEndian -> (a, b)
        BigEndian -> (b, a)
      word = fromIntegral @Word8 @Word16 low .|. (fromIntegral @Word8 @Word16 high `shiftL` 8)
  in fromIntegral (fromIntegral @Word16 @Int16 word) / fullScale
  where
    fullScale :: Double
    fullScale = fromIntegral (maxBound @Int16) + 1

-- | The range of human hearing, in Hz.
lowestFrequency, highestFrequency :: Double
lowestFrequency = 20
highestFrequency = 20000

-- | Move the elements of a buffer of the last samples to its start by a
-- number of elements, and return the end that they leave for new ones. The
-- buffer is moved in place, which a frame would otherwise copy.
pushOut :: VSM.Storable a => VSM.IOVector a -> Int -> IO (VSM.IOVector a)
pushOut v n = do
  let kept = VSM.length v - n
  VSM.move (VSM.slice 0 kept v) (VSM.slice n kept v)
  pure $ VSM.slice kept n v
