-- | The spectrum of the visualizer's samples, with pocketfft's real FFT in
-- C.
module Reprise.Visualizer.Spectrum
  ( Transform
  , newTransform
  , SampleWindow
  , newSampleWindow
  , pushSamples
  , pushSilence
  , spectrumOf
  , windowSamples
  , binFrequency
  ) where

import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import Reprise.Visualizer.Samples

-- | pocketfft's plan of a real FFT of 'transformLength' points, the window
-- of the samples, and the buffer of a transform, which a frame would
-- otherwise allocate anew. Because of the buffer, one thread at a time can
-- use it.
data Transform = Transform
  { plan :: ForeignPtr RfftPlan
  , window :: VS.Vector Double
  , work :: VSM.IOVector Double
  }

data RfftPlan

newTransform :: IO Transform
newTransform = do
  plan <- c_make_rfft_plan (fromIntegral transformLength)
  when (plan == nullPtr) . throwIO $ userError "pocketfft couldn't make the plan of its FFT"
  Transform
    <$> newForeignPtr c_destroy_rfft_plan plan
    <*> pure (VS.generate windowSamples blackman)
    <*> VSM.new transformLength
  where
    -- The Blackman window, as in ncmpcpp, for its low side lobes.
    blackman :: Int -> Double
    blackman i =
      let x = 2 * pi * fromIntegral i / fromIntegral (windowSamples - 1)
      in (1 - alpha) / 2 - cos x / 2 + alpha / 2 * cos (2 * x)

    alpha :: Double
    alpha = 0.16

-- | The last 'windowSamples' samples of every channel, as MPD writes them,
-- after silence. New samples push out the oldest in place, which a frame
-- would otherwise copy.
newtype SampleWindow = SampleWindow
  { bytes :: VSM.IOVector Word8
  }

-- | A window of silence.
newSampleWindow :: IO SampleWindow
newSampleWindow = SampleWindow <$> VSM.replicate (windowSamples * frameBytes) 0

-- | Push the samples of every channel into the window.
pushSamples :: SampleWindow -> BS.ByteString -> IO ()
pushSamples samples = pushBytes samples.bytes

-- | Push a number of bytes of silence into the window.
pushSilence :: SampleWindow -> Int -> IO ()
pushSilence samples = pushZeros samples.bytes

-- | The magnitudes of the spectrum of a channel of the window, one for each
-- bin.
spectrumOf :: Transform -> SampleWindow -> Int -> IO (VS.Vector Double)
spectrumOf transform samples channel = do
  out <- VSM.new (transformLength `div` 2 + 1)
  r <-
    withForeignPtr transform.plan $ \p ->
      VSM.unsafeWith samples.bytes $ \src ->
        VS.unsafeWith transform.window $ \win ->
          VSM.unsafeWith transform.work $ \w ->
            VSM.unsafeWith out $ \o ->
              c_spectrum
                p
                (castPtr src)
                (castPtr win)
                (fromIntegral windowSamples)
                (fromIntegral channels)
                (fromIntegral channel)
                (castPtr w)
                (castPtr o)
  when (r /= 0) . throwIO $ userError "pocketfft couldn't allocate the buffers of its FFT"
  VS.unsafeFreeze out

-- | The samples of a channel that a spectrum is of, as in ncmpcpp with the
-- author's @visualizer_spectrum_dft_size@ of 2: about a third of a second.
-- Fewer would blur the low frequencies, and more would make the spectrum
-- lag behind the sound.
windowSamples :: Int
windowSamples = 16384

-- | The points of the transform: the samples padded with silence to twice
-- as many, as in ncmpcpp, which gives bins half as wide.
transformLength :: Int
transformLength = 2 * windowSamples

-- | The frequency of a bin of a spectrum, in Hz.
binFrequency :: Int -> Double
binFrequency bin = fromIntegral bin * fromIntegral sampleRate / fromIntegral transformLength

foreign import ccall unsafe "make_rfft_plan"
  c_make_rfft_plan :: CSize -> IO (Ptr RfftPlan)

foreign import ccall unsafe "&destroy_rfft_plan"
  c_destroy_rfft_plan :: FunPtr (Ptr RfftPlan -> IO ())

foreign import ccall unsafe "reprise_spectrum"
  c_spectrum
    :: Ptr RfftPlan
    -> Ptr CUChar
    -> Ptr CDouble
    -> CSize
    -> CSize
    -> CSize
    -> Ptr CDouble
    -> Ptr CDouble
    -> IO CInt
