-- | The thread that reads the samples of MPD's fifo output for the
-- visualizer, while the visualizer shows, and sends what each frame shows:
-- the samples of the ellipse, the spectrum or the samples of the wave.
module Reprise.Visualizer.Worker
  ( VisualizerSource (..)
  , visualizerWorker

    -- * The samples between MPD and the frames
  , Playout (..)
  , newPlayout
  , playout
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.ByteString qualified as BS
import Data.IORef.Strict qualified as S
import Data.Maybe
import Effectful
import Effectful.Exception

import Reprise.Config
import Reprise.Effect.Clock
import Reprise.Effect.Fifo
import Reprise.Event
import Reprise.Exception
import Reprise.Visualizer.Samples
import Reprise.Visualizer.Spectrum
import Reprise.Visualizer.Wave

data VisualizerSource = VisualizerSource
  { fps :: Int
  , trail :: Int
  -- ^ The frames whose samples the ellipse shows.
  , reading :: TVar (Maybe Visualization)
  -- ^ What the visualizer wants the samples for.
  , emit :: AppEvent -> IO Bool
  -- ^ Doesn't wait for the UI, so that a frame that it can't draw in time
  -- is dropped. Returns whether the UI took the event.
  , report :: AppEvent -> IO ()
  -- ^ Waits for the UI, for an event that it mustn't miss, e.g. a failure,
  -- after which the worker waits for another wish.
  , debug :: Bool
  -- ^ Whether to send what happened to the frames each second.
  }

-- | Wait until the visualizer wants the samples, and send what each frame
-- shows until it doesn't. The samples that the fifo held before are old,
-- so they are dropped. A fifo whose writer went, e.g. as MPD restarted,
-- opens again.
visualizerWorker :: (Clock :> es, Fifo :> es, IOE :> es) => VisualizerSource -> Eff es ()
visualizerWorker src = do
  transform <- liftIO newTransform
  forever $ do
    wanted <- liftIO . atomically $ readTVar src.reading >>= maybe retry pure
    try @IOException openFifo >>= \case
      -- The next try comes with the next wish, e.g. another visualization.
      Left err -> liftIO $ do
        src.report . VisualizerFailed $
          "The visualizer can't read its data source: " <> exceptionText err
        atomically $ readTVar src.reading >>= check . (/= Just wanted)
      Right () -> (`finally` closeFifo) $ do
        void readFifo
        start <- monotonicTime
        window <- liftIO newSampleWindow
        wave <- liftIO newWaveWindow
        second <- liftIO $ S.newIORef (Second start noFrames)
        playing transform window wave second start
  where
    -- The frames from when the samples come until the picture is silent
    -- after the last ones, again and again. In between, e.g. while MPD is
    -- paused, the worker waits for the samples instead of waking for each
    -- frame. The frames keep their times from the start, and the first
    -- after a wait shows only its own samples.
    playing
      :: (Clock :> es, Fifo :> es, IOE :> es)
      => Transform
      -> SampleWindow
      -> WaveWindow
      -> S.IORef Second
      -> Double
      -> Eff es ()
    playing transform window wave second start =
      waitFifo (readTVar src.reading >>= check . isNothing) >>= \case
        SamplesCame -> do
          now <- monotonicTime
          let n = max 1 (ceiling ((now - start) * fromIntegral src.fps))
          silent <- frames transform window wave second start n (n - 1) (newPlayout now) 0
          when silent $ playing transform window wave second start
        Cancelled -> pure ()
        WriterGone -> pure ()

    -- The frames of silence since the last samples. True once the picture
    -- is silent after them, False once the visualizer doesn't want the
    -- samples.
    frames
      :: forall es
       . (Clock :> es, Fifo :> es, IOE :> es)
      => Transform
      -> SampleWindow
      -> WaveWindow
      -> S.IORef Second
      -> Double
      -> Int
      -> Int
      -> Playout
      -> Int
      -> Eff es Bool
    frames transform window wave second start n previous p quiet = do
      sleepUntil $ start + fromIntegral n / fromIntegral src.fps
      liftIO (readTVarIO src.reading) >>= \case
        Nothing -> pure False
        Just v -> do
          new <- readFifo
          arrival <- monotonicTime
          let shown = frameBytes * (samplesUntil n - samplesUntil previous)
              (frame, stopped, p') = playout shown arrival new p
              continue = frames' arrival p'

              -- What the last samples show falls through silence, for a
              -- number of frames, after which the picture is silent.
              lastSamples
                :: (BS.ByteString -> IO ()) -> (Int -> IO ()) -> Int -> IO () -> Eff es Bool
              lastSamples push pushSilent fading draw
                | not (BS.null frame) = do
                    liftIO $ push frame >> draw
                    continue 0
                | stopped && quiet >= fading = pure True
                | stopped = do
                    liftIO $ pushSilent shown >> draw
                    continue (quiet + 1)
                | otherwise = continue quiet
          liftIO . count arrival $ \st ->
            st
              { frames = st.frames + 1
              , late = st.late + n - previous - 1
              , empty = st.empty + fromEnum (BS.null frame && isJust (writeSize p') && not stopped)
              , bytes = st.bytes + BS.length new
              }
          case v of
            -- The ellipse keeps the samples of its trail, so it only sends
            -- each frame's.
            Ellipse ->
              lastSamples (const (pure ())) (const (pure ())) src.trail $
                send (VisualizerSamples frame)
            Spectrum ->
              lastSamples
                (pushSamples window)
                (pushSilence window)
                (silentFrames windowSamples)
                spectrum
            Wave ->
              lastSamples (pushWave wave) (pushWaveSilence wave) (silentFrames historySamples) $
                send . VisualizerWave =<< waveOf wave
      where
        -- A frame that came late doesn't make the next ones late.
        frames' :: Double -> Playout -> Int -> Eff es Bool
        frames' sent =
          frames
            transform
            window
            wave
            second
            start
            (max (n + 1) (ceiling ((sent - start) * fromIntegral src.fps)))
            n

        spectrum :: IO ()
        spectrum = do
          -- The left channel is the first of a frame's, the right one the second.
          left <- spectrumOf transform window 0
          right <- spectrumOf transform window 1
          send $ VisualizerSpectrum left right

        send :: AppEvent -> IO ()
        send e = do
          taken <- src.emit e
          unless taken . S.modifyIORef second $ \(Second since st) ->
            Second since st {dropped = st.dropped + 1}

        -- Count a frame. A frame a second after the start of a second
        -- starts the next one, once the frames of the last are sent, so that
        -- a frame that the UI drops counts in the second of the frame.
        count :: Double -> (FrameStats -> FrameStats) -> IO ()
        count now f = do
          Second since st <- S.readIORef second
          if now - since >= 1
            then do
              when src.debug . void . src.emit $ VisualizerStats st
              S.writeIORef second (Second now (f noFrames))
            else S.writeIORef second (Second since (f st))

    -- The samples from the start until a frame.
    samplesUntil :: Int -> Int
    samplesUntil n = n * sampleRate `div` src.fps

    -- The frames of silence that fill a number of samples.
    silentFrames :: Int -> Int
    silentFrames kept = (kept * src.fps + sampleRate - 1) `div` sampleRate

    noFrames :: FrameStats
    noFrames = FrameStats {frames = 0, late = 0, empty = 0, dropped = 0, bytes = 0}

-- | What happened to the frames since a time.
data Second = Second Double FrameStats

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
  , recentReads :: [Int]
  -- ^ The bytes of the last reads that gave samples, newest first, at most
  -- 'readsPerWrite' of them. The fewest are a write of MPD, see
  -- 'writeSize'.
  , lastWrite :: Double
  -- ^ When the last samples came.
  }
  deriving stock (Show)

-- | Before the first samples, which come after the time.
newPlayout :: Double -> Playout
newPlayout time =
  Playout {buffered = BS.empty, flowing = False, recentReads = [], lastWrite = time}

-- | The bytes of a write of MPD: the fewest that a recent read gave, as a
-- pipe gives a write that short whole. Only recent reads count, as MPD's
-- last write of a song is shorter, and so is the first after a seek.
-- 'Nothing' before the first samples.
writeSize :: Playout -> Maybe Int
writeSize p = if null p.recentReads then Nothing else Just (minimum p.recentReads)

-- | The reads that 'writeSize' takes the fewest bytes of. A read holds one
-- write, or more when a frame comes late, or when frames come slower than
-- MPD writes. While frames come at least half as often as MPD writes, one of
-- any two reads in a row holds a single write, so three find one even with a
-- frame that came late.
readsPerWrite :: Int
readsPerWrite = 3

-- | What a frame shows: the samples that its time takes, of a number of
-- bytes, at the time that the new samples came. Also whether MPD stopped
-- writing, e.g. paused.
playout
  :: Int -> Double -> BS.ByteString -> Playout -> (BS.ByteString, Bool, Playout)
playout shown arrival new p =
  let arrived = not (BS.null new)
      recentReads = if arrived then take readsPerWrite (BS.length new : p.recentReads) else p.recentReads
      write = writeSize p {recentReads = recentReads}
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
          && arrival - lastWrite > 2 * fromIntegral margin / fromIntegral (frameBytes * sampleRate)
  in ( frame
     , stopped
     , Playout
         { buffered = buffered
         , flowing = flowing
         , recentReads = recentReads
         , lastWrite = lastWrite
         }
     )
  where
    -- To whole samples of every channel.
    roundUp :: Int -> Int
    roundUp n = (n + frameBytes - 1) `div` frameBytes * frameBytes

-- | The writes that the buffer holds ahead of the frames, so that a write
-- that comes late by up to a write's time doesn't run it out. On the
-- author's MPD, a write of 21 ms came between 19.3 ms and 22.7 ms after
-- the last, and with a write ahead, the buffer ran out a few times a
-- second.
writesAhead :: Int
writesAhead = 2
