-- | The thread that reads the samples of MPD's fifo output for the
-- visualizer, while the visualizer shows, and sends what each frame shows:
-- the samples of the ellipse, or the spectrum.
module Reprise.Visualizer.Worker
  ( VisualizerSource (..)
  , visualizerWorker
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Text qualified as T
import GHC.Clock
import System.IO

import Reprise.Config
import Reprise.Event
import Reprise.Visualizer.Samples
import Reprise.Visualizer.Spectrum

data VisualizerSource = VisualizerSource
  { path :: FilePath
  , channels :: Int
  , fps :: Int
  , reading :: TVar (Maybe Visualization)
  -- ^ What the visualizer wants the samples for.
  , emit :: AppEvent -> IO ()
  -- ^ Doesn't wait for the UI, so that a frame that it can't draw in time
  -- is dropped.
  }

-- | Wait until the visualizer wants the samples, and send what each frame
-- shows until it doesn't. The samples that the fifo held before are old,
-- so they are dropped.
visualizerWorker :: VisualizerSource -> IO ()
visualizerWorker src = do
  transform <- newTransform
  forever $ do
    atomically $ readTVar src.reading >>= check . isJust
    -- A fifo opens at once without a writer, as GHC opens it non-blocking.
    try @IOException (openBinaryFile src.path ReadMode) >>= \case
      Left err -> do
        src.emit . VisualizerFailed $
          "The visualizer can't read its data source: " <> T.pack (displayException err)
        atomically $ readTVar src.reading >>= check . isNothing
      Right h -> (`finally` hClose h) $ do
        void (readAvailable h)
        start <- getMonotonicTime
        frames transform h start 0 BS.empty BS.empty 0
  where
    -- The samples that came after the last frame, the window of the
    -- spectrum, and the number of frames without samples since the last.
    frames
      :: Transform -> Handle -> Double -> Int -> BS.ByteString -> BS.ByteString -> Int -> IO ()
    frames transform h start n buffered window quiet = do
      let deadline = start + fromIntegral n / fromIntegral src.fps
      now <- getMonotonicTime
      due <- registerDelay . max 0 $ ceiling ((deadline - now) * microsecondsPerSecond)
      -- The visualizer can stop while the worker waits for the frame.
      visualization <- atomically $ do
        v <- readTVar src.reading
        when (isJust v) $ readTVar due >>= check
        pure v
      forM_ visualization $ \v -> do
        new <- readAvailable h
        let available = buffered <> new
            whole = BS.length available - BS.length available `mod` frameBytes
            (frame, rest) = BS.splitAt (min samplesBytes whole) available
            -- A frame without samples is silence, which the spectrum falls
            -- through.
            window' =
              BS.takeEnd windowBytes $
                window <> if BS.null frame then BS.replicate samplesBytes 0 else frame
            quiet' = if BS.null frame then quiet + 1 else 0
        case v of
          Ellipse -> src.emit $ VisualizerSamples frame
          -- Once silence fills the window, the spectrum stays the same.
          Spectrum -> when (quiet' <= silentFrames) $ do
            spectra <- forM [0 .. src.channels - 1] $ \c -> spectrumOf transform src.channels c window'
            src.emit $ VisualizerSpectrum spectra
        -- A frame that came late doesn't make the next ones late, and the
        -- samples don't fall behind the sound by more than a frame.
        sent <- getMonotonicTime
        let next = max (n + 1) (ceiling ((sent - start) * fromIntegral src.fps))
            excess = max 0 (BS.length rest - samplesBytes)
        frames transform h start next (BS.drop (roundUp excess) rest) window' quiet'

    -- The bytes of a sample of every channel.
    frameBytes :: Int
    frameBytes = bytesPerSample * src.channels

    -- The bytes of the samples that a frame shows: as long as the frame.
    samplesBytes :: Int
    samplesBytes = frameBytes * samplesPerFrame

    samplesPerFrame :: Int
    samplesPerFrame = max 1 (sampleRate `div` src.fps)

    windowBytes :: Int
    windowBytes = frameBytes * windowSamples

    -- The frames of silence that fill the window.
    silentFrames :: Int
    silentFrames = (windowSamples + samplesPerFrame - 1) `div` samplesPerFrame

    -- To whole samples of every channel.
    roundUp :: Int -> Int
    roundUp n = (n + frameBytes - 1) `div` frameBytes * frameBytes

    microsecondsPerSecond :: Double
    microsecondsPerSecond = 1000000

-- | Read what the fifo holds, without waiting for more.
readAvailable :: Handle -> IO BS.ByteString
readAvailable h = BS.concat <$> go
  where
    go :: IO [BS.ByteString]
    go = do
      chunk <- BS.hGetNonBlocking h pipeCapacity
      if BS.length chunk < pipeCapacity then pure [chunk] else (chunk :) <$> go

-- | The capacity of a pipe on Linux, so that a read usually takes all that
-- the fifo holds.
pipeCapacity :: Int
pipeCapacity = 65536
