-- | The mirror: reprise's copy of MPD's status and queue, and its pure
-- updates from MPD's replies.
module Reprise.Mpd.Mirror
  ( -- * Mirror
    Mirror (..)
  , emptyMirror
  , setQueue
  , applyQueueChanges
  , setStatus

    -- * Queries
  , currentSong
  , currentPosition
  , elapsedAt
  , queueLength
  ) where

import Data.Foldable
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Generics
import MPD.Types
import Optics.Core

data Mirror = Mirror
  { status :: Maybe Status
  , statusTime :: Double
  -- ^ The monotonic time when the status arrived, for the elapsed time.
  , queue :: Seq.Seq Song
  , queueVersion :: Maybe PlaylistVersion
  -- ^ The version of the queue that 'queue' mirrors. The status may be
  -- newer, until the changes of the queue arrive.
  }
  deriving stock (Eq, Show, Generic)

emptyMirror :: Mirror
emptyMirror = Mirror Nothing 0 Seq.empty Nothing

-- | Replace the whole queue.
setQueue :: Double -> Status -> [Song] -> Mirror -> Mirror
setQueue now st songs m =
  setStatus now st $
    m
      & #queue .~ Seq.fromList songs
      & #queueVersion ?~ st.playlistVersion

-- | Apply the reply to @plchanges@, which MPD sent with the status in one
-- command list. The changed songs replace or extend the queue at their
-- positions, then the queue is cut to its new length.
applyQueueChanges :: Double -> Status -> [Song] -> Mirror -> Either T.Text Mirror
applyQueueChanges now st changes m = do
  q <- foldlM apply m.queue (L.sortOn (.position) changes)
  pure . setStatus now st $
    m
      & #queue .~ Seq.take st.playlistLength q
      & #queueVersion ?~ st.playlistVersion
  where
    apply :: Seq.Seq Song -> Song -> Either T.Text (Seq.Seq Song)
    apply q song = case song.position of
      Just (SongPos p)
        | p < Seq.length q -> Right $ Seq.update p song q
        | p == Seq.length q -> Right $ q Seq.|> song
        | otherwise ->
            Left $ "a changed song at position " <> T.pack (show p) <> " is past the end of the queue"
      Nothing -> Left "a changed song has no position"

setStatus :: Double -> Status -> Mirror -> Mirror
setStatus now st m =
  m
    & #status ?~ st
    & #statusTime .~ now

currentPosition :: Mirror -> Maybe Int
currentPosition m = do
  st <- m.status
  SongPos p <- st.currentPosition
  pure p

currentSong :: Mirror -> Maybe Song
currentSong m = currentPosition m >>= (`Seq.lookup` m.queue)

-- | The elapsed time of the current song at a monotonic time, interpolated
-- from the last status while it plays.
elapsedAt :: Double -> Mirror -> Maybe Seconds
elapsedAt now m = do
  st <- m.status
  e <- st.elapsed
  pure $ case st.state of
    Playing ->
      let interpolated = e + realToFrac (max 0 (now - m.statusTime))
      in maybe interpolated (min interpolated) st.duration
    _ -> e

queueLength :: Mirror -> Int
queueLength m = Seq.length m.queue
