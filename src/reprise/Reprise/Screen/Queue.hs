-- | The queue screen: MPD's queue as a list, which the user moves around,
-- selects songs in, finds songs in and changes.
module Reprise.Screen.Queue
  ( -- * Drawing
    queueView

    -- * Verbs
  , queueVerb
  , shuffleQueue

    -- * Moving
  , jumpToPlaying

    -- * Finding
  , queueRows
  ) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Event
import Reprise.Find
import Reprise.Groups
import Reprise.Handler.Core
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Save
import Reprise.Screen.Queue.Edits
import Reprise.Selection
import Reprise.State
import Reprise.UI.SongList

----------------------------------------
-- Drawing

queueView :: AppEnv -> AppState -> View -> V.Image
queueView env s v =
  let ctx = rowContext env s.toggles.queueDisplay v.width
      titles = titleRow env s v ctx
      playingId = do
        st <- s.mirror.status
        guard $ st.state /= Stopped
        st.currentId
      visible = Seq.take (listHeight env s v) (Seq.drop v.offset s.mirror.queue)
      found =
        typedMatches s $
          foldText . rowText env.config.lists env.config.songs s.toggles.queueDisplay <$> visible
      row (i, isFound) song =
        renderRow
          ctx
          RowFlags
            { queued = False
            , playing = isJust playingId && song.songId == playingId
            , selected = maybe False (`isSelected` s.queueState.selection) song.songId
            , found = isFound
            , cursor = i == v.cursor && cursorVisible s
            }
          song
  in V.vertCat $ titles <> zipWith row (zip [v.offset ..] found) (toList visible)

----------------------------------------
-- Verbs

-- | How the queue does a verb, if it does it.
queueVerb :: App es => Action -> Maybe (Eff es ())
queueVerb = \case
  Move t -> Just $ modifyWithEnv (moveListCursor t)
  JumpToPlaying -> Just $ modifyWithEnv jumpToPlaying
  Activate -> Just activate
  Save -> Just $ getsS queueToSave >>= maybe (showMessage "The queue is empty") askSaveName
  Select t -> Just $ select t
  Delete -> Just deleteMarked
  Priority p -> Just $ prioritize p
  MoveSongs t -> Just $ moveSongs t
  Shuffle -> Just shuffleSelected
  Toggle ToggleDisplay -> Just $ toggleDisplay #queueDisplay
  Toggle ToggleFollowPlaying -> Just toggleFollowPlaying
  _ -> Nothing

-- | Shuffle the selected songs, which must be next to each other, else the
-- whole queue. Shuffling the selected songs loses nothing the user didn't
-- point at, so only shuffling the whole queue asks first.
shuffleSelected :: App es => Eff es ()
shuffleSelected =
  getsS (runs . selectedSongPositions) >>= \case
    [] -> shuffleQueue
    [(a, b)] -> do
      mutate . shuffle . Just $ Range (SongPos a) (Just (SongPos (b + 1)))
      showMessage $ "Shuffled " <> countSongs (b - a + 1)
    _ -> showError "Only selected songs next to each other can be shuffled"

-- | Ask whether to shuffle the whole queue.
shuffleQueue :: App es => Eff es ()
shuffleQueue = do
  n <- getsS (queueLength . (.mirror))
  if n < 2
    then showMessage "There is nothing to shuffle"
    else
      modifyS $
        confirm ("Shuffle " <> countSongs n <> " in the queue?") (Confirmed ConfirmShuffle)

toggleFollowPlaying :: App es => Eff es ()
toggleFollowPlaying = do
  toggleSetting "Follow playing" #followPlaying
  follow <- getsS (.toggles.followPlaying)
  when follow (modifyWithEnv jumpToPlaying)

----------------------------------------
-- Moving

-- | Move the queue's cursor to the playing song in the middle of the list,
-- also while the view shows another screen.
jumpToPlaying :: AppEnv -> AppState -> AppState
jumpToPlaying env s = case currentPosition s.mirror of
  Nothing -> s
  Just p -> jumpScreenTo QueueScreen p env s

----------------------------------------
-- Selection

-- | The positions of the selected songs of the queue, in order.
selectedSongPositions :: AppState -> [Int]
selectedSongPositions s = selectedPositions ((.songId) <$> s.mirror.queue) s.queueState.selection

-- | What a save saves: the selected songs, or the whole queue without a
-- selection. Nothing when the queue is empty.
queueToSave :: AppState -> Maybe SaveSource
queueToSave s = case selectedSongPositions s of
  []
    | null s.mirror.queue -> Nothing
    | otherwise -> Just SaveQueue
  ps ->
    Just $ SaveItems [songToSave song | p <- ps, Just song <- [Seq.lookup p s.mirror.queue]]

-- | The positions of the songs that an action applies to.
markedPositions :: AppState -> [Int]
markedPositions s =
  marked ((.songId) <$> s.mirror.queue) s.queueState.selection (focusedView s).cursor

select :: App es => SelectTarget -> Eff es ()
select t = do
  q <- getsS (.mirror.queue)
  selectInList
    (#queueState % #selection)
    ((\song -> (song.songId, Just song)) <$> q)
    queueRows
    countSongs
    t

----------------------------------------
-- Changes

-- | Play the song under the cursor.
activate :: App es => Eff es ()
activate = do
  song <- getsS songUnderCursor
  forM_ (song >>= (.songId)) (mutate . playId)

deleteMarked :: App es => Eff es ()
deleteMarked = do
  ps <- getsS markedPositions
  mutate $ deletePositions ps

prioritize :: App es => Int -> Eff es ()
prioritize p = do
  s <- getS
  case mapMaybe (\i -> Seq.lookup i s.mirror.queue >>= (.songId)) (markedPositions s) of
    [] -> showMessage "The queue is empty"
    ids -> do
      mutate $ prioId p ids
      showMessage $ "Priority " <> T.pack (show p) <> " set for " <> countSongs (length ids)

moveSongs :: App es => MoveSongsTarget -> Eff es ()
moveSongs t = do
  s <- getS
  let ps = markedPositions s
      n = queueLength s.mirror
      c = (focusedView s).cursor
      -- The cursor moves with its song if the song's run moves.
      follow :: App es => [(Int, Int)] -> Int -> Eff es ()
      follow moved delta =
        when (or [c >= a && c <= b | (a, b) <- moved]) $
          modifyWithEnv (setCursor (c + delta))
  case t of
    MoveSongsUp -> do
      mutate $ moveUp ps
      follow (runsMovingUp ps) (-1)
    MoveSongsDown -> do
      mutate $ moveDown n ps
      follow (runsMovingDown n ps) 1
    MoveSongsToCursor -> case selectedSongPositions s of
      [] -> showMessage "Select the songs to move first"
      selected -> case moveBefore selected c of
        Just cmd -> mutate cmd
        Nothing -> showMessage "The cursor is among the selected songs"
    MoveSongsToEnd -> forM_ (moveBefore ps n) mutate
    MoveSongsToBeginning -> mutate (moveToStart ps)
    MoveSongsToNext -> case currentPosition s.mirror of
      Nothing -> showMessage "No song is playing"
      Just current
        | current `elem` ps -> showMessage "The playing song is among the songs to move"
        | otherwise -> mutate (moveAfter current ps)

----------------------------------------
-- Finding

-- | The rows of the queue as finds match them: the ones of the last find,
-- unless the queue or its display changed since.
queueRows :: App es => Eff es (Seq.Seq Folded)
queueRows = do
  env <- getAppEnv
  s <- getS
  case s.queueState.findRows of
    Just r
      | r.version == s.mirror.queueVersion && r.display == s.toggles.queueDisplay -> pure r.rows
    _ -> do
      let display = s.toggles.queueDisplay
          rows = foldText . rowText env.config.lists env.config.songs display <$> s.mirror.queue
      modifyS $ #queueState % #findRows ?~ FindRows s.mirror.queueVersion display rows
      pure rows
