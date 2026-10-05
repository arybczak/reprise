-- | The thread that reads the samples of MPD's fifo output for the
-- visualizer, while the visualizer shows, and sends those of each frame.
module Reprise.Visualizer.Worker
  ( VisualizerSource (..)
  , visualizerWorker
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Text qualified as T
import GHC.Clock
import System.IO

import Reprise.Event
import Reprise.Visualizer.Samples

data VisualizerSource = VisualizerSource
  { path :: FilePath
  , channels :: Int
  , fps :: Int
  , reading :: TVar Bool
  -- ^ Whether the visualizer wants the samples.
  , emit :: AppEvent -> IO ()
  -- ^ Doesn't wait for the UI, so that a frame that it can't draw in time
  -- is dropped.
  }

-- | Wait until the visualizer wants the samples, and send those of each
-- frame until it doesn't. The samples that the fifo held before are old, so
-- they are dropped.
visualizerWorker :: VisualizerSource -> IO ()
visualizerWorker src = forever $ do
  atomically $ readTVar src.reading >>= check
  -- A fifo opens at once without a writer, as GHC opens it non-blocking.
  try @IOException (openBinaryFile src.path ReadMode) >>= \case
    Left err -> do
      src.emit . VisualizerFailed $
        "The visualizer can't read its data source: " <> T.pack (displayException err)
      atomically $ readTVar src.reading >>= check . not
    Right h -> (`finally` hClose h) $ do
      void (readAvailable h)
      start <- getMonotonicTime
      frames h start 0 BS.empty
  where
    frames :: Handle -> Double -> Int -> BS.ByteString -> IO ()
    frames h start n buffered = do
      let deadline = start + fromIntegral n / fromIntegral src.fps
      now <- getMonotonicTime
      due <- registerDelay . max 0 $ ceiling ((deadline - now) * microsecondsPerSecond)
      -- The visualizer can stop while the worker waits for the frame.
      reading <- atomically $ do
        r <- readTVar src.reading
        when r $ readTVar due >>= check
        pure r
      when reading $ do
        new <- readAvailable h
        let available = buffered <> new
            whole = BS.length available - BS.length available `mod` frameBytes
            (frame, rest) = BS.splitAt (min samplesBytes whole) available
        src.emit $ VisualizerSamples frame
        -- A frame that came late doesn't make the next ones late, and the
        -- samples don't fall behind the sound by more than a frame.
        sent <- getMonotonicTime
        let next = max (n + 1) (ceiling ((sent - start) * fromIntegral src.fps))
            excess = max 0 (BS.length rest - samplesBytes)
        frames h start next (BS.drop (roundUp excess) rest)

    -- The bytes of a sample of every channel.
    frameBytes :: Int
    frameBytes = bytesPerSample * src.channels

    -- The bytes of the samples that a frame shows: as long as the frame.
    samplesBytes :: Int
    samplesBytes = frameBytes * max 1 (sampleRate `div` src.fps)

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
