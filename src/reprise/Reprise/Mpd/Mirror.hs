-- | The mirror: reprise's copy of MPD's status and queue, and its pure
-- updates from MPD's replies and from reprise's own edits of the queue.
module Reprise.Mpd.Mirror
  ( -- * Mirror
    Mirror (..)
  , emptyMirror
  , setQueue
  , applyQueueChanges
  , setStatus
  , forgetStatus
  , forgetQueue

    -- * Edits
  , QueueEdit (..)
  , editCommands
  , applyQueueEdits

    -- * Queries
  , currentSong
  , currentPosition
  , elapsedAt
  , queueLength
  ) where

import Control.Monad
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Word
import GHC.Generics
import Optics.Core

import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types

data Mirror = Mirror
  { status :: Maybe Status
  , statusTime :: Double
  -- ^ The monotonic time when the status arrived, for the elapsed time.
  , queue :: Seq.Seq Song
  -- ^ The songs have no position, which an edit that the mirror makes
  -- itself would leave stale: the position of a song is its index.
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
emptyMirror =
  Mirror
    { status = Nothing
    , statusTime = 0
    , queue = Seq.empty
    , queueVersion = Nothing
    , totalLength = 0
    , queued = S.empty
    , lengthFromCurrent = 0
    }

-- | Replace the whole queue.
setQueue :: Double -> Status -> [Song] -> Mirror -> Mirror
setQueue now st songs m =
  setStatus now st $
    m
      & withQueue (Seq.fromList (map unplaced songs))
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
        | p < Seq.length q -> Right $ Seq.update p (unplaced song) q
        | p == Seq.length q -> Right $ q Seq.|> unplaced song
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

-- | Forget the status of the player, e.g. without a connection, in which it
-- would look as if it went on.
forgetStatus :: Mirror -> Mirror
forgetStatus = #status .~ Nothing

-- | No queue, until the next whole queue arrives.
forgetQueue :: Mirror -> Mirror
forgetQueue m =
  m
    & withQueue Seq.empty
    & #queueVersion .~ Nothing
    & #lengthFromCurrent .~ 0

-- | An edit of the queue at positions.
data QueueEdit
  = DeleteRange Range
  | -- | Move the songs of the range so that the first one is at the
    -- position.
    MoveRange Range SongPos
  | ClearQueue
  | ShuffleRange (Maybe Range)
  deriving stock (Eq, Show)

-- | The commands of the edits, in one command list.
editCommands :: [QueueEdit] -> Command ()
editCommands = traverse_ $ \case
  DeleteRange r -> delete r
  MoveRange r target -> move r (At target)
  ClearQueue -> clear
  ShuffleRange r -> shuffle r

-- | Make the edits in the mirror, with the status that MPD sent after them
-- in one command list. 'Nothing' if the status shows that the queue isn't
-- the mirror's after the edits: another client changed it too, or the
-- edits shuffle it.
--
-- Each command that changes the queue increments its version by one, also
-- in a command list, so a version as many changes past the mirror's one
-- means that no other change came between.
applyQueueEdits :: Double -> Status -> [QueueEdit] -> Mirror -> Maybe Mirror
applyQueueEdits now st edits m = do
  PlaylistVersion v <- m.queueVersion
  (q, changes) <- foldlM edit (m.queue, 0) edits
  guard $ st.playlistVersion == PlaylistVersion (v + changes)
  guard $ Seq.length q == st.playlistLength
  pure . setStatus now st $
    m
      & withQueue q
      & #queueVersion ?~ st.playlistVersion
  where
    -- The queue after an edit, and how many changes of the version it adds.
    -- For MPD, a delete of no songs and a move in place change nothing.
    edit :: (Seq.Seq Song, Word32) -> QueueEdit -> Maybe (Seq.Seq Song, Word32)
    edit (q, changes) = \case
      DeleteRange r
        | (a, b) <- bounds q r
        , a < b ->
            Just (Seq.take a q <> Seq.drop b q, changes + 1)
        | otherwise -> Just (q, changes)
      MoveRange r (SongPos target)
        | (a, b) <- bounds q r
        , a /= target ->
            let (moved, rest) = Seq.splitAt (b - a) (Seq.drop a q)
                (before, after) = Seq.splitAt target (Seq.take a q <> rest)
            in Just (before <> moved <> after, changes + 1)
        | otherwise -> Just (q, changes)
      ClearQueue -> Just (Seq.empty, changes + 1)
      ShuffleRange _ -> Nothing

    -- MPD cuts the end of a range to the length of the queue.
    bounds :: Seq.Seq Song -> Range -> (Int, Int)
    bounds q r =
      let SongPos a = r.start
      in (a, maybe (Seq.length q) (\(SongPos b) -> min b (Seq.length q)) r.end)

unplaced :: Song -> Song
unplaced = #position .~ Nothing

withQueue :: Seq.Seq Song -> Mirror -> Mirror
withQueue q m =
  m
    & #queue .~ q
    & #totalLength .~ songsLength q
    & #queued .~ S.fromList (map songKey (toList q))

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
