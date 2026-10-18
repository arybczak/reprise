-- | The queue screen: MPD's queue as a list, which the user moves around,
-- selects songs in, finds songs in and changes.
module Reprise.Screen.Queue
  ( -- * Drawing
    queueView

    -- * For the handlers
  , queueOps

    -- * Moving
  , jumpToPlaying
  ) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful
import Effectful.Input.Static
import Effectful.State.Static.Local
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
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
      visible = visibleItems env s v s.mirror.queue
      found = typedMatches s $ songFindText env s.toggles.queueDisplay <$> visible
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

queueOps
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => ScreenOps es
queueOps = ScreenOps {verb = queueVerb, rows = Just queueRows, shown = switchTo QueueScreen}

-- | How the queue does a verb, if it does it.
queueVerb
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Action
  -> Maybe (Eff es ())
queueVerb = \case
  Move t -> Just $ modifyWithEnv (moveListCursor t)
  JumpToPlaying -> Just $ do
    current <- getsS (currentPosition . (.mirror))
    if isJust current then modifyWithEnv jumpToPlaying else noCurrentSong
  Activate -> Just activate
  Add p -> Just $ addMarked (Just p)
  AddAndPlay -> Just $ addMarked Nothing
  Save -> Just $ getsS queueToSave >>= maybe (showMessage "The queue is empty") askSaveName
  Select t -> Just $ select t
  Delete -> Just deleteMarked
  Priority p -> Just $ prioritize p
  MoveSongs t -> Just $ moveSongs t
  Clear -> Just clearQueue
  Shuffle -> Just shuffleSelected
  Toggle ToggleDisplay -> Just $ toggleDisplay #queueDisplay
  Toggle ToggleFollowPlaying -> Just toggleFollowPlaying
  _ -> Nothing

-- | Ask whether to clear the queue.
clearQueue :: (State AppState :> es, UiRequest :> es) => Eff es ()
clearQueue = do
  n <- getsS (queueLength . (.mirror))
  if n == 0
    then showMessage "The queue is empty"
    else
      modifyS $
        confirm ("Clear " <> countSongs n <> " from the queue?") (Confirmed ConfirmClear)

-- | Shuffle the selected songs, which must be next to each other, else the
-- whole queue. Shuffling the selected songs loses nothing the user didn't
-- point at, so only shuffling the whole queue asks first.
shuffleSelected :: (State AppState :> es, MpdRequest :> es, UiRequest :> es) => Eff es ()
shuffleSelected =
  getsS (runs . selectedSongPositions) >>= \case
    [] -> shuffleQueue
    [run@(a, b)] -> do
      editQueue [ShuffleRange (Just (runRange run))]
      showMessage $ "Shuffled " <> countSongs (b - a + 1)
    _ -> showError "Only selected songs next to each other can be shuffled"

-- | Ask whether to shuffle the whole queue.
shuffleQueue :: (State AppState :> es, UiRequest :> es) => Eff es ()
shuffleQueue = do
  n <- getsS (queueLength . (.mirror))
  if n < 2
    then showMessage "There is nothing to shuffle"
    else
      modifyS $
        confirm ("Shuffle " <> countSongs n <> " in the queue?") (Confirmed ConfirmShuffle)

toggleFollowPlaying
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Eff es ()
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

select
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => SelectTarget
  -> Eff es ()
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

-- | Add copies of the marked songs, at a place, or without one at the end
-- to play the first, as the add keys add in the browser. A part of a file,
-- e.g. a track of a cue sheet, is left out, as an add takes the whole file.
addMarked
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Maybe AddPosition
  -> Eff es ()
addMarked place = do
  s <- getS
  env <- getAppEnv
  let songs = mapMaybe (`Seq.lookup` s.mirror.queue) (markedPositions s)
      whole = filter (isNothing . (.range)) songs
      commands = [add song.file | song <- whole]
      added =
        ( case whole of
            [song] -> "Added: " <> songText env song
            _ -> "Added " <> countSongs (length whole)
        )
          <> withoutParts (length songs - length whole)
  case (place, songs) of
    (_, []) -> showMessage "The queue is empty"
    -- A single song plays where it is.
    (Nothing, [song]) -> addAndPlayAt (Just song) commands added
    _ | null whole -> showMessage $ "Added nothing" <> withoutParts (length songs)
    (Just p, _) -> addAt p commands added
    (Nothing, _) -> addAndPlayAt Nothing commands added

-- | Play the song under the cursor.
activate :: (State AppState :> es, MpdRequest :> es) => Eff es ()
activate = do
  song <- getsS songUnderCursor
  forM_ (song >>= (.songId)) (mutate . playId)

deleteMarked :: (State AppState :> es, MpdRequest :> es) => Eff es ()
deleteMarked = do
  ps <- getsS markedPositions
  editQueue $ deletePositions ps

prioritize
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Int
  -> Eff es ()
prioritize p = do
  s <- getS
  case mapMaybe (\i -> Seq.lookup i s.mirror.queue >>= (.songId)) (markedPositions s) of
    [] -> showMessage "The queue is empty"
    ids -> do
      mutate $ prioId p ids
      showMessage $ "Priority " <> T.pack (show p) <> " set for " <> countSongs (length ids)

moveSongs
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => MoveSongsTarget
  -> Eff es ()
moveSongs t = do
  s <- getS
  let ps = markedPositions s
      n = queueLength s.mirror
      c = (focusedView s).cursor
      -- The cursor goes to the middle one of the songs, as in ncmpcpp, so
      -- that they show as they move. The song moves if its run moves.
      follow :: (State AppState :> es, Input AppEnv :> es) => [(Int, Int)] -> Int -> Eff es ()
      follow moved delta = case drop (length ps `div` 2) ps of
        middle : _
          | not (null moved) ->
              modifyWithEnv . setCursor $
                if or [middle >= a && middle <= b | (a, b) <- moved]
                  then middle + delta
                  else middle
        _ -> pure ()

      -- The songs end up together, in their order, from a position. The
      -- cursor jumps to the first one, as to an album, so that the songs
      -- before them show too.
      jumpToMoved :: (State AppState :> es, Input AppEnv :> es) => Int -> Eff es ()
      jumpToMoved = modifyWithEnv . jumpTo
  case t of
    MoveSongsUp -> do
      editQueue $ moveUp ps
      follow (runsMovingUp ps) (-1)
    MoveSongsDown -> do
      editQueue $ moveDown n ps
      follow (runsMovingDown n ps) 1
    MoveSongsToCursor -> case selectedSongPositions s of
      [] -> showMessage "Select the songs to move first"
      selected
        | c `elem` selected -> showMessage "The cursor is among the selected songs"
        | otherwise -> do
            editQueue (moveAfter c selected)
            -- The cursor stays on its song, as after an add there.
            modifyWithEnv . setCursor $ c - length (filter (< c) selected)
    MoveSongsToEnd -> unless (null ps) $ do
      editQueue (moveDownTo n ps)
      jumpToMoved (n - length ps)
    MoveSongsToBeginning -> do
      editQueue (moveToStart ps)
      jumpToMoved 0
    MoveSongsToNext -> case currentPosition s.mirror of
      Nothing -> noCurrentSong
      Just current
        | current `elem` ps -> showMessage "The current song is among the songs to move"
        | otherwise -> do
            editQueue (moveAfter current ps)
            -- The songs above the current song move below it.
            jumpToMoved (current - length (filter (< current) ps) + 1)

----------------------------------------
-- Finding

-- | The rows of the queue as finds match them: the ones of the last find,
-- unless the queue or its display changed since.
queueRows :: (State AppState :> es, Input AppEnv :> es) => Eff es (Seq.Seq Folded)
queueRows = do
  env <- getAppEnv
  s <- getS
  case s.queueState.findRows of
    Just r
      | r.version == s.mirror.queueVersion && r.display == s.toggles.queueDisplay -> pure r.rows
    _ -> do
      let display = s.toggles.queueDisplay
          rows = songFindText env display <$> s.mirror.queue
      modifyS $ #queueState % #findRows ?~ FindRows s.mirror.queueVersion display rows
      pure rows
