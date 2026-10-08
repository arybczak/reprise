-- | The queue screen: MPD's queue as a list, which the user moves around,
-- selects songs in, finds songs in and changes.
module Reprise.Screen.Queue
  ( -- * Drawing
    queueView

    -- * Moving
  , moveQueueCursor
  , jumpToPlaying

    -- * Selection
  , select
  , selectedSongPositions
  , queueToSave

    -- * Changes
  , activate
  , deleteMarked
  , prioritize
  , moveSelection

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
import Reprise.Find
import Reprise.Groups
import Reprise.Handler.Core
import Reprise.LineEdit
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
  let ctx =
        RowContext
          { colorMode = env.colorMode
          , lists = env.config.lists
          , songs = env.config.songs
          , display = s.toggles.queueDisplay
          , width = v.width
          }
      titles
        | s.toggles.queueDisplay == Columns && env.config.songs.columns.showTitles =
            [renderTitles ctx]
        | otherwise = []
      playingId = do
        st <- s.mirror.status
        guard $ st.state /= Stopped
        st.currentId
      visible = Seq.take (listHeight env s v) (Seq.drop v.offset s.mirror.queue)
      -- The matches of a find show while the user types it.
      found = case s.prompt of
        Just (Prompt _ (Line edit (ForFind _)))
          | Right p <- compilePattern (lineEditText edit)
          , Right matched <-
              matchAll
                p
                (foldText . rowText env.config.lists env.config.songs s.toggles.queueDisplay <$> visible) ->
              toList matched
        _ -> repeat False
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
-- Moving

moveQueueCursor :: MoveTarget -> AppEnv -> AppState -> AppState
moveQueueCursor t env s = moveListCursor Just s.mirror.queue t env s

-- | Move the queue's cursor to the playing song in the middle of the list,
-- also while the view shows another screen.
jumpToPlaying :: AppEnv -> AppState -> AppState
jumpToPlaying env s =
  let v = focusedView s
      h = listHeight env s (v & #screen .~ QueueScreen)
  in case currentPosition s.mirror of
       Nothing -> s
       Just p
         | v.screen == QueueScreen -> jumpTo p env s
         | otherwise -> s & #views % ix s.focus % #positions % at QueueScreen ?~ (p, p - h `div` 2)

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

-- | The positions of the songs that an action applies to: the selected
-- songs, or the song under the cursor without a selection.
markedPositions :: AppState -> [Int]
markedPositions s = case selectedSongPositions s of
  [] -> [c | let c = (focusedView s).cursor, c >= 0, c < queueLength s.mirror]
  ps -> ps

select :: App es => SelectTarget -> Eff es ()
select t = do
  q <- getsS (.mirror.queue)
  case t of
    SelectFound -> do
      rows <- queueRows
      selectFound (#queueState % #selection) ((.songId) <$> q) rows countSongs
    _ -> selectInList (#queueState % #selection) ((\song -> (song.songId, Just song)) <$> q) t
  case t of
    SelectItem (Just m) -> modifyWithEnv (moveQueueCursor m)
    _ -> pure ()

withSongUnderCursor :: App es => (Song -> Eff es ()) -> Eff es ()
withSongUnderCursor k = do
  s <- getS
  forM_ (Seq.lookup (focusedView s).cursor s.mirror.queue) k

----------------------------------------
-- Changes

-- | Play the song under the cursor.
activate :: App es => Eff es ()
activate = withSongUnderCursor $ \song -> forM_ song.songId (mutate . playId)

deleteMarked :: App es => Eff es ()
deleteMarked = do
  ps <- getsS markedPositions
  mutate $ deletePositions ps

prioritize :: App es => Int -> Eff es ()
prioritize p = do
  s <- getS
  let ids = mapMaybe (\i -> Seq.lookup i s.mirror.queue >>= (.songId)) (markedPositions s)
  mutate $ prioId p ids
  showMessage $ "Priority " <> T.pack (show p) <> " set for " <> countSongs (length ids)

moveSelection :: App es => MoveSelectionTarget -> Eff es ()
moveSelection t = do
  s <- getS
  let ps = markedPositions s
      n = queueLength s.mirror
      c = (focusedView s).cursor
      -- The cursor moves with its song if the song's run moves.
      follow :: App es => (Int -> Int -> Bool) -> Int -> Eff es ()
      follow moves delta =
        when (or [moves a b && c >= a && c <= b | (a, b) <- runs ps]) $
          modifyWithEnv (setCursor (c + delta))
  case t of
    MoveSelectionUp -> do
      mutate $ moveUp ps
      follow (\a _ -> a > 0) (-1)
    MoveSelectionDown -> do
      mutate $ moveDown n ps
      follow (\_ b -> b < n - 1) 1
    MoveSelectionToCursor -> case selectedSongPositions s of
      [] -> showMessage "Select the songs to move first"
      selected -> case moveBefore selected c of
        Just cmd -> mutate cmd
        Nothing -> showMessage "The cursor is among the selected songs"
    MoveSelectionToEnd -> forM_ (moveBefore ps n) mutate
    MoveSelectionToBeginning -> mutate (moveToStart ps)
    MoveSelectionToNext -> case currentPosition s.mirror of
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
