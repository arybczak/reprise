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
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics
import Optics.Core

import Reprise.Mpd.Protocol.Types

data Mirror = Mirror
  { status :: Maybe Status
  , statusTime :: Double
  -- ^ The monotonic time when the status arrived, for the elapsed time.
  , queue :: Seq.Seq Song
  , queueVersion :: Maybe PlaylistVersion
  -- ^ The version of the queue that 'queue' mirrors. The status may be
  -- newer, until the changes of the queue arrive.
  , totalLength :: Seconds
  -- ^ The length of the songs of the queue. The header shows it on every
  -- redraw, so it is summed only when the queue changes.
  , queued :: ~(S.Set (T.Text, Maybe SongRange))
  -- ^ The files of the songs of the queue, with their parts, so that the
  -- other screens can mark them on every redraw. Made when a screen first
  -- needs it after a change of the queue.
  , lengthFromCurrent :: Seconds
  -- ^ The length of the current song and the songs after it, or of the
  -- whole queue without a current song. It is summed when the status
  -- arrives.
  }
  deriving stock (Eq, Show, Generic)

emptyMirror :: Mirror
emptyMirror = Mirror Nothing 0 Seq.empty Nothing 0 S.empty 0

-- | Replace the whole queue.
setQueue :: Double -> Status -> [Song] -> Mirror -> Mirror
setQueue now st songs m =
  setStatus now st $
    m
      & withQueue (Seq.fromList songs)
      & #queueVersion ?~ st.playlistVersion

-- | Apply the reply to @plchanges@, which MPD sent with the status in one
-- command list. The changed songs replace or extend the queue at their
-- positions, then the queue is cut to its new length.
applyQueueChanges :: Double -> Status -> [Song] -> Mirror -> Either T.Text Mirror
applyQueueChanges now st changes m = do
  q <- foldlM apply m.queue (L.sortOn (.position) changes)
  pure . setStatus now st $
    m
      & withQueue (Seq.take st.playlistLength q)
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
    & #lengthFromCurrent .~ case st.currentPosition of
      Just (SongPos p) -> songsLength (Seq.drop p m.queue)
      Nothing -> m.totalLength

withQueue :: Seq.Seq Song -> Mirror -> Mirror
withQueue q m =
  m
    & #queue .~ q
    & #totalLength .~ songsLength q
    & #queued .~ S.fromList [(song.file, song.range) | song <- toList q]

-- | Songs without a length count as 0.
songsLength :: Seq.Seq Song -> Seconds
songsLength = foldl' (\acc song -> acc + fromMaybe 0 song.duration) 0

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
