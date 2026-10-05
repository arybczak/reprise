{-# LANGUAGE InterruptibleFFI #-}

-- | The thread that reads the samples of MPD's fifo output for the
-- visualizer, while the visualizer shows, and sends what each frame shows:
-- the samples of the ellipse, or the spectrum.
module Reprise.Visualizer.Worker
  ( VisualizerSource (..)
  , visualizerWorker

    -- * The samples between MPD and the frames
  , Playout (..)
  , newPlayout
  , playout
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Text qualified as T
import Foreign.C.Types
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
        window <- newSampleWindow src.channels
        frames transform window h start 1 0 (newPlayout start) 0
  where
    -- The frames of silence since the last samples.
    frames
      :: Transform -> SampleWindow -> Handle -> Double -> Int -> Int -> Playout -> Int -> IO ()
    frames transform window h start n previous p quiet = do
      sleepUntil $ start + fromIntegral n / fromIntegral src.fps
      visualization <- readTVarIO src.reading
      forM_ visualization $ \v -> do
        new <- readAvailable h
        arrival <- getMonotonicTime
        let shown = sampleBytes * (samplesUntil n - samplesUntil previous)
            (frame, stopped, p') = playout sampleBytes shown arrival new p
            continue = frames' arrival p'
        case v of
          Ellipse -> do
            src.emit $ VisualizerSamples frame
            continue 0
          Spectrum
            | not (BS.null frame) -> do
                pushSamples window frame
                spectrum
                continue 0
            -- The spectrum falls through silence, and stays once the
            -- window is silent.
            | stopped -> do
                pushSilence window shown
                when (quiet < silentFrames) spectrum
                continue (quiet + 1)
            | otherwise -> continue quiet
      where
        -- A frame that came late doesn't make the next ones late.
        frames' :: Double -> Playout -> Int -> IO ()
        frames' sent =
          frames
            transform
            window
            h
            start
            (max (n + 1) (ceiling ((sent - start) * fromIntegral src.fps)))
            n

        spectrum :: IO ()
        spectrum = do
          spectra <- forM [0 .. src.channels - 1] $ spectrumOf transform window
          src.emit $ VisualizerSpectrum spectra

    -- The samples from the start until a frame.
    samplesUntil :: Int -> Int
    samplesUntil n = n * sampleRate `div` src.fps

    -- The bytes of a sample of every channel.
    sampleBytes :: Int
    sampleBytes = bytesPerSample * src.channels

    -- The frames of silence that fill the window.
    silentFrames :: Int
    silentFrames = (windowSamples * src.fps + sampleRate - 1) `div` sampleRate

-- | The samples between MPD's writes and the frames. MPD writes at the
-- speed of the sound, but in writes that can be longer than a frame, so the
-- frames show samples once the buffer holds a frame and 'writesAhead'
-- writes more.
data Playout = Playout
  { buffered :: BS.ByteString
  -- ^ The samples that came and that no frame showed yet.
  , flowing :: Bool
  -- ^ Whether the frames show samples: from when the buffer holds a frame
  -- and the writes ahead, until it runs out.
  , write :: Maybe Int
  -- ^ The bytes of a write of MPD: the fewest that a read gave, as a pipe
  -- gives a write that short whole.
  , lastWrite :: Double
  -- ^ When the last samples came.
  }
  deriving stock (Show)

-- | Before the first samples, which come after the time.
newPlayout :: Double -> Playout
newPlayout = Playout BS.empty False Nothing

-- | What a frame shows: the samples that its time takes, of the bytes of a
-- sample of every channel and a number of bytes, at the time that the new
-- samples came. Also whether MPD stopped writing, e.g. paused.
playout
  :: Int -> Int -> Double -> BS.ByteString -> Playout -> (BS.ByteString, Bool, Playout)
playout sampleBytes shown arrival new p =
  let arrived = not (BS.null new)
      write = if arrived then Just (maybe (BS.length new) (min (BS.length new)) p.write) else p.write
      lastWrite = if arrived then arrival else p.lastWrite
      margin = fromMaybe 0 write
      available = p.buffered <> new
      flowing = BS.length available >= shown + if p.flowing then 0 else writesAhead * margin
      (frame, rest) = if flowing then BS.splitAt shown available else (BS.empty, available)
      -- More than a frame and a write beyond the writes ahead is a lag,
      -- which the clocks of MPD and reprise drift into.
      excess = BS.length rest - (shown + (writesAhead + 1) * margin)
      buffered = if excess > 0 then BS.drop (roundUp excess) rest else rest
      -- Without a write for two, MPD stopped writing.
      stopped =
        isJust write
          && arrival - lastWrite > 2 * fromIntegral margin / fromIntegral (sampleBytes * sampleRate)
  in (frame, stopped, Playout buffered flowing write lastWrite)
  where
    -- To whole samples of every channel.
    roundUp :: Int -> Int
    roundUp n = (n + sampleBytes - 1) `div` sampleBytes * sampleBytes

-- | The writes that the buffer holds ahead of the frames, so that a write
-- that comes late by up to a write's time doesn't run it out. On the
-- author's MPD, a write of 21 ms came between 19.3 ms and 22.7 ms after
-- the last, and with a write ahead, the buffer ran out a few times a
-- second.
writesAhead :: Int
writesAhead = 2

-- | Sleep until a time of the monotonic clock. GHC's timers wake up to a
-- millisecond late, which shows next to a frame of 8 ms. Like 'threadDelay',
-- it lets an exception in, also in a thread that masks them, e.g. one that
-- 'bracket' forked.
sleepUntil :: Double -> IO ()
sleepUntil t = do
  now <- getMonotonicTime
  when (now < t) $ do
    c_sleepUntil (realToFrac t)
    allowInterrupt
    sleepUntil t

foreign import ccall interruptible "reprise_sleep_until"
  c_sleepUntil :: CDouble -> IO ()

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
