module Reprise.Test.Visualizer.Spectrum (spectrumTests) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Int
import Data.Vector.Storable qualified as VS
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Test.Visualizer.Helpers
import Reprise.Visualizer.Spectrum

spectrumTests :: TestTree
spectrumTests =
  testGroup
    "spectrum"
    [ testCase "the spectrum of a sine" test_sineSpectrum
    , testCase "the window of the spectrum" test_window
    ]

-- | The Blackman window passes 0.42 of a sine, and a real sine is half in
-- the bin of its frequency and half in the bin of the negative one.
test_sineSpectrum :: Assertion
test_sineSpectrum = do
  transform <- newTransform
  let amplitude = 0.9
      frequency = 1000
      sine =
        samples
          [ (round (amplitude * fromIntegral (maxBound @Int16) * sin (2 * pi * frequency * t)), 0)
          | i <- [0 .. windowSamples - 1]
          , let t = fromIntegral i / 44100
          ]
  window <- newSampleWindow
  pushSamples window sine
  left <- spectrumOf transform window 0
  right <- spectrumOf transform window 1
  let peak = VS.maxIndex left
  assertBool
    ("the peak at " <> show (binFrequency peak) <> " Hz")
    (abs (binFrequency peak - frequency) <= binFrequency 1)
  assertBool
    ("its magnitude " <> show (left VS.! peak))
    (abs (left VS.! peak - amplitude * 0.42 / 2) < 0.01)
  assertEqual "the silent channel" 0 (VS.maximum right)

-- | The window holds the last samples, whether they came at once or in
-- pieces, and silence pushes them out.
test_window :: Assertion
test_window = do
  transform <- newTransform
  let count = 2 * windowSamples
      noise =
        samples
          [ (fromIntegral (i * 7919), fromIntegral (i * 104729))
          | i <- [0 .. count - 1]
          ]
      bytesOf n = 4 * n
      half = windowSamples `div` 2
      spectra w = forM [0, 1] $ spectrumOf transform w
      windowOf pcm = do
        w <- newSampleWindow
        pushSamples w pcm
        pure w
  whole <- spectra =<< windowOf (BS.takeEnd (bytesOf windowSamples) noise)
  pieces <- newSampleWindow
  let (first, rest) = BS.splitAt (bytesOf 1000) noise
      (middle, end) = BS.splitAt (BS.length rest - bytesOf 3) rest
  mapM_ (pushSamples pieces) [first, middle, end]
  assertEqual "in pieces" whole =<< spectra pieces
  pushSilence pieces (bytesOf half)
  silenced <-
    spectra
      =<< windowOf (BS.takeEnd (bytesOf half) noise <> BS.replicate (bytesOf half) 0)
  assertEqual "half silent" silenced =<< spectra pieces
  pushSilence pieces (bytesOf count)
  assertEqual "silent" [0, 0] . map VS.maximum =<< spectra pieces
