-- | The samples of the visualizer's wave: the last ones, of a fixed length,
-- from where the bass last rose through zero, as an oscilloscope's trigger
-- does. A steady sound then stands still. Without it, each frame starts at
-- another point of the sound's period, and the wave jumps sideways.
module Reprise.Visualizer.Wave
  ( WaveWindow
  , newWaveWindow
  , pushWave
  , pushWaveSilence
  , waveOf
  , waveSamples
  , historySamples
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word
import Foreign.Ptr

import Reprise.Config
import Reprise.Visualizer.Samples

-- | The last 'historySamples' samples of every channel, as MPD writes them,
-- and the bass of their mix, after silence. New samples push out the oldest
-- in place, as in the spectrum's window.
data WaveWindow = WaveWindow
  { bytes :: VSM.IOVector Word8
  , bass :: VSM.IOVector Double
  }

-- | Silence.
newWaveWindow :: IO WaveWindow
newWaveWindow =
  WaveWindow
    <$> VSM.replicate (historySamples * frameBytes) 0
    <*> VSM.replicate historySamples 0

-- | Push the samples of every channel into the wave.
pushWave :: WaveWindow -> BS.ByteString -> IO ()
pushWave w pcm = do
  let count = BS.length pcm `div` frameBytes
  pushBass w count $ \i ->
    sum [sampleAt pcm (i * channels + c) | c <- [0 .. channels - 1]] / fromIntegral channels
  pushBytes w.bytes (BS.take (count * frameBytes) pcm)

-- | Push a number of bytes of silence into the wave.
pushWaveSilence :: WaveWindow -> Int -> IO ()
pushWaveSilence w n = do
  let count = n `div` frameBytes
  pushBass w count (const 0)
  pushZeros w.bytes (count * frameBytes)

-- | Push the bass of a number of samples of the mix, by their index. The
-- filter goes through all of them, also those that don't fit.
pushBass :: WaveWindow -> Int -> (Int -> Double) -> IO ()
pushBass w count mix = do
  previous <- VSM.read w.bass (historySamples - 1)
  end <- pushOut w.bass (min count historySamples)
  let skipped = count - VSM.length end
      go :: Int -> Double -> IO ()
      go i y = when (i < count) $ do
        let y' = y + smoothing * (mix i - y)
        when (i >= skipped) $ VSM.write end (i - skipped) y'
        go (i + 1) y'
  go 0 previous

-- | The last 'waveSamples' samples of every channel from the latest point
-- where the bass rises through zero, or the last ones if it doesn't.
waveOf :: WaveWindow -> IO BS.ByteString
waveOf w = do
  start <- trigger latest
  VSM.unsafeWith (VSM.slice (start * frameBytes) (waveSamples * frameBytes) w.bytes) $ \p ->
    BS.packCStringLen (castPtr p, waveSamples * frameBytes)
  where
    trigger :: Int -> IO Int
    trigger i
      | i < 1 = pure latest
      | otherwise = do
          before <- VSM.read w.bass (i - 1)
          after <- VSM.read w.bass i
          if before < 0 && after >= 0 then pure i else trigger (i - 1)

    latest :: Int
    latest = historySamples - waveSamples

-- | The samples of every channel that the wave shows: those of a frame at
-- the default frame rate, about 17 ms, so that the wave looks the same at
-- any rate.
waveSamples :: Int
waveSamples =
  let FrameRate fps = defaultConfig.visualizer.fps
  in sampleRate `div` fps

-- | The samples before the wave where the trigger looks for the bass rising
-- through zero: a period of the lowest frequency that people hear, so that
-- it finds a rise of any bass.
searchSamples :: Int
searchSamples = ceiling (fromIntegral sampleRate / lowestFrequency)

-- | The samples of every channel that the wave keeps.
historySamples :: Int
historySamples = waveSamples + searchSamples

-- | The coefficient of the one-pole low-pass filter of the bass. Its cutoff
-- is the frequency whose period is the wave's length, about 60 Hz: a sound
-- of a shorter period repeats within the wave, so its jumps move it by a
-- smaller part of the wave.
smoothing :: Double
smoothing = 1 - exp (-2 * pi / fromIntegral waveSamples)
