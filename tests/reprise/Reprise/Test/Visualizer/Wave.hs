module Reprise.Test.Visualizer.Wave (waveTests) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Int
import Data.List qualified as L
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Test.Visualizer.Helpers
import Reprise.Visualizer.Samples
import Reprise.Visualizer.Wave

waveTests :: TestTree
waveTests =
  testGroup
    "wave"
    [ testCase "a steady sound stands still in the wave" test_triggerStill
    , testCase "the wave starts at the latest rise" test_triggerLatest
    , testCase "the wave follows the bass" test_triggerBass
    , testCase "without a rise of the bass, the wave is of the last samples" test_triggerNone
    , testCase "the wave of silence" test_triggerSilence
    ]

-- | A tone of 100 Hz, with a period of 441 samples. Once more samples come,
-- the wave is the same, also of a tone in one channel only.
test_triggerStill :: Assertion
test_triggerStill =
  forM_ [("both", \s -> (s, s)), ("left", (,0)), ("right", (0,))] $ \(placement, placed) -> do
    w <- newWaveWindow
    let tone i = 20000 * sin (2 * pi * 100 * fromIntegral (i `mod` 441) / 44100)
    pushWave w (soundIn placed tone 0 44100)
    first <- waveOf w
    assertEqual "the length" (waveSamples * frameBytes) (BS.length first)
    pushWave w (soundIn placed tone 44100 44200)
    assertEqual ("the shift of the wave in " <> placement) (Just 0) . shiftFrom first
      =<< waveOf w
  where
    -- How many samples a wave moved on from an earlier one, by the
    -- smallest shift at which they overlap.
    shiftFrom :: BS.ByteString -> BS.ByteString -> Maybe Int
    shiftFrom earlier later =
      L.find
        (\k -> BS.drop (k * frameBytes) earlier `BS.isPrefixOf` later)
        [0 .. BS.length earlier `div` frameBytes - 1]

-- | A tone of 100 Hz that gets louder: each of its periods rises through
-- zero. The last rise is in the wave's samples or the period before them,
-- and the tone is loud a period before that, so the wave's first period is
-- loud. From the first rise, in the first period of the samples that the
-- wave keeps, it would be quiet.
test_triggerLatest :: Assertion
test_triggerLatest = do
  w <- newWaveWindow
  let tone loud i = loud * sin (2 * pi * 100 * fromIntegral (i `mod` period) / 44100)
      quiet = historySamples * 2
  pushWave w (sound (tone 5000) 0 quiet)
  pushWave w (sound (tone 20000) quiet (quiet + waveSamples + 2 * period))
  wave <- waveOf w
  let peak = maximum . map abs . take period $ left wave
  assertBool ("the peak of the first period: " <> show peak) (peak > 10000)
  where
    period :: Int
    period = 441

-- | A bass of 100 Hz and a treble of 2953 Hz as loud. The treble isn't a
-- harmonic of the bass, so it rises through zero at other points of the
-- bass's period in each one. As more samples come, in 20 chunks of more
-- than 2 periods, the waves differ from the first by the swing of the
-- treble and the bass's change in 10 samples at most: the treble that the
-- filter passes moves the rise by a few samples. Without the filter, the
-- bass is at any point of its period, and they differ by up to the swing
-- of both.
test_triggerBass :: Assertion
test_triggerBass = do
  w <- newWaveWindow
  let tone i =
        let t = fromIntegral i / 44100
        in bass * sin (2 * pi * 100 * t) + treble * sin (2 * pi * 2953 * t)
  pushWave w (sound tone 0 44100)
  first <- waveOf w
  differences <- forM [1 .. 20] $ \k -> do
    pushWave w (sound tone (44100 + (k - 1) * 1000) (44100 + k * 1000))
    later <- waveOf w
    pure . maximum $ zipWith (\a b -> abs (a - b)) (left first) (left later)
  assertBool
    ("the differences: " <> show differences)
    (all (<= 2 * treble + bass * 2 * pi * 100 * 10 / 44100) differences)
  where
    bass, treble :: Double
    bass = 10000
    treble = 10000

test_triggerNone :: Assertion
test_triggerNone = do
  w <- newWaveWindow
  let rising = samples [(fromIntegral (i `mod` 30000) + 1, 1000) | i <- [0 .. 4000 :: Int]]
  pushWave w rising
  assertEqual "the last samples" (BS.takeEnd (waveSamples * frameBytes) rising) =<< waveOf w

test_triggerSilence :: Assertion
test_triggerSilence = do
  w <- newWaveWindow
  let silence = BS.replicate (waveSamples * frameBytes) 0
  assertEqual "at first" silence =<< waveOf w
  pushWave w circle
  pushWaveSilence w (historySamples * frameBytes)
  assertEqual "after the samples" silence =<< waveOf w

----------------------------------------
-- Helpers

-- | The samples of the left channel.
left :: BS.ByteString -> [Double]
left pcm =
  [ sampleAt pcm (i * channels) * fromIntegral (maxBound @Int16)
  | i <- [0 .. BS.length pcm `div` frameBytes - 1]
  ]

-- | The samples of a sound from an index until an end, the same in both
-- channels.
sound :: (Int -> Double) -> Int -> Int -> BS.ByteString
sound = soundIn (\s -> (s, s))

-- | The samples of a sound, placed in the channels.
soundIn :: (Int16 -> (Int16, Int16)) -> (Int -> Double) -> Int -> Int -> BS.ByteString
soundIn placed f from end = samples [placed (round (f i)) | i <- [from .. end - 1]]
