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
  , selectedPositions
  , modifySelection

    -- * Changes
  , activate
  , deleteMarked
  , prioritize
  , moveSelection

    -- * Finding
  , startFind
  , findAsYouType
  , acceptFind
  , findAgain
  ) where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
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
import Reprise.Screen.Queue.Edits
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
      playingId = s.mirror.status >>= (.currentId)
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
            { playing = isJust playingId && song.songId == playingId
            , selected = maybe False (`S.member` s.queueState.selection) song.songId
            , found = isFound
            , cursor = i == v.cursor && cursorVisible s
            }
          song
  in V.vertCat $ titles <> zipWith row (zip [v.offset ..] found) (toList visible)

----------------------------------------
-- Moving

moveQueueCursor :: MoveTarget -> AppEnv -> AppState -> AppState
moveQueueCursor t env s =
  let h = max 1 (listHeight env s (focusedView s))
      q = s.mirror.queue
      c = (focusedView s).cursor
  in case t of
       MoveUp -> setCursor (c - 1) env s
       MoveDown -> setCursor (c + 1) env s
       MovePageUp -> setCursor (c - h) env s
       MovePageDown -> setCursor (c + h) env s
       MoveFirst -> setCursor 0 env s
       MoveLast -> setCursor (Seq.length q - 1) env s
       MovePreviousAlbum -> jumpTo (previousGroup albumKey q c) env s
       MoveNextAlbum -> jumpTo (nextGroup albumKey q c) env s
       MovePreviousArtist -> jumpTo (previousGroup artistKey q c) env s
       MoveNextArtist -> jumpTo (nextGroup artistKey q c) env s

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
selectedPositions :: AppState -> [Int]
selectedPositions s =
  [ i
  | (i, song) <- zip [0 ..] (toList s.mirror.queue)
  , maybe False (`S.member` s.queueState.selection) song.songId
  ]

-- | The positions of the songs that an action applies to: the selected
-- songs, or the song under the cursor without a selection.
markedPositions :: AppState -> [Int]
markedPositions s = case selectedPositions s of
  [] -> [c | let c = (focusedView s).cursor, c >= 0, c < queueLength s.mirror]
  ps -> ps

select :: App es => SelectTarget -> Eff es ()
select = \case
  SelectItem andMove -> do
    withSongUnderCursor $ \song -> forM_ song.songId $ \i -> do
      selected <- getsS ((i `S.member`) . (.queueState.selection))
      modifyS . modifySelection $ if selected then S.delete i else S.insert i
      modifyS $
        #queueState % #lastSelected %~ \ends ->
          (if selected then id else take rangeEnds . (i :)) (filter (/= i) ends)
    forM_ andMove (modifyWithEnv . moveQueueCursor)
  -- Between the last two songs that the user selected, so that a range
  -- doesn't swallow the songs between it and an earlier selection. Without
  -- them, between the first and the last selected song, as in ncmpcpp.
  SelectRange -> do
    s <- getS
    let positionOf i = Seq.findIndexL ((== Just i) . (.songId)) s.mirror.queue
        ends =
          mapMaybe positionOf $
            filter (`S.member` s.queueState.selection) s.queueState.lastSelected
    case if length ends == rangeEnds then ends else selectedPositions s of
      [] -> showMessage "Select the first and the last song of the range first"
      ps -> do
        addToSelection [minimum ps .. maximum ps]
        showMessage "Range selected"
  SelectInvert -> do
    ids <- getsS (S.fromList . mapMaybe (.songId) . toList . (.mirror.queue))
    modifyS $ modifySelection (ids S.\\)
    showMessage "Selection inverted"
  SelectNone -> do
    modifyS $ modifySelection (const S.empty)
    showMessage "Selection cleared"
  SelectAlbum -> selectGroup albumKey "Album"
  SelectArtist -> selectGroup artistKey "Artist"
  SelectFound -> do
    rows <- queueRows
    s <- getS
    case compilePattern <$> s.queueState.findPattern of
      Nothing -> showMessage "Nothing was found yet"
      Just (Left err) -> showError (capitalize err)
      Just (Right p) -> case matchAll p rows of
        Left err -> showError (capitalize err)
        Right found -> do
          let ps = [i | (i, True) <- zip [0 ..] (toList found)]
          addToSelection ps
          showMessage $ countSongs (length ps) <> " found and selected"
  where
    -- The songs next to each other around the cursor with its song's key.
    selectGroup :: (App es, Eq k) => (Song -> k) -> T.Text -> Eff es ()
    selectGroup key name = do
      s <- getS
      let q = s.mirror.queue
          c = (focusedView s).cursor
      forM_ (Seq.lookup c q) $ \song -> do
        let same i = (key <$> Seq.lookup i q) == Just (key song)
            earlier = takeWhile same [c - 1, c - 2 .. 0]
            later = takeWhile same [c + 1 .. Seq.length q - 1]
        addToSelection (earlier <> [c] <> later)
        showMessage $ name <> " around the cursor selected"

    -- The first and the last song.
    rangeEnds :: Int
    rangeEnds = 2

    addToSelection :: App es => [Int] -> Eff es ()
    addToSelection ps = do
      q <- getsS (.mirror.queue)
      let ids = S.fromList $ mapMaybe (\i -> Seq.lookup i q >>= (.songId)) ps
      modifyS $ modifySelection (S.union ids)

modifySelection :: (S.Set SongId -> S.Set SongId) -> AppState -> AppState
modifySelection f = #queueState % #selection %~ f

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
    MoveSelectionToCursor -> case selectedPositions s of
      [] -> showMessage "Select the songs to move first"
      selected -> case moveBefore selected c of
        Just cmd -> mutate cmd
        Nothing -> showMessage "The cursor is among the selected songs"
    MoveSelectionToEnd -> forM_ (moveBefore ps n) mutate

----------------------------------------
-- Finding

startFind :: Direction -> AppState -> AppState
startFind direction s =
  let v = focusedView s
  in openLine
       question
       emptyLineEdit
       (ForFind $ Finding direction (v.cursor, v.offset) Nothing)
       s
  where
    question :: T.Text
    question = case direction of
      Forward -> "Find forward: "
      Backward -> "Find backward: "

-- | Move to the first match from where the find started, on every key, so
-- that the result doesn't depend on how the pattern was typed. Returns the
-- find with a note on what it found.
findAsYouType :: App es => Finding -> T.Text -> Eff es Finding
findAsYouType f text = do
  rows <- queueRows
  note <-
    if T.null text
      then modifyWithEnv (restoreView f.origin) >> pure Nothing
      else case compilePattern text of
        -- The cursor stays until the pattern is complete again.
        Left err -> pure (Just err)
        Right p -> case search p f.direction (fst f.origin) rows of
          Left err -> pure (Just err)
          Right Nothing -> modifyWithEnv (restoreView f.origin) >> pure (Just "no match")
          Right (Just found) -> do
            modifyWithEnv (jumpTo found.index)
            pure (wrapNote f.direction found)
  pure $ f & #note .~ note

-- | Keep the pattern for the next and the previous match. An empty pattern
-- finds the last pattern again, as in Vim.
acceptFind :: App es => Finding -> T.Text -> Eff es ()
acceptFind f text
  | T.null text = findAgain f.direction
  | otherwise = case compilePattern text of
      Left _ -> do
        modifyWithEnv (restoreView f.origin)
        showError $ "Invalid pattern: " <> text
      Right _ -> do
        modifyS $ #queueState % #findPattern ?~ text
        forM_ f.note (showMessage . capitalize)

-- | Move to the next or the previous match of the last pattern.
findAgain :: App es => Direction -> Eff es ()
findAgain direction = do
  rows <- queueRows
  s <- getS
  case s.queueState.findPattern of
    Nothing -> showMessage "Nothing was found yet"
    Just text -> case compilePattern text of
      Left err -> showError (capitalize err)
      Right p -> case search p direction (focusedView s).cursor rows of
        Left err -> showError (capitalize err)
        Right Nothing -> showMessage $ "No match for " <> text
        Right (Just found) -> do
          modifyWithEnv (jumpTo found.index)
          forM_ (wrapNote direction found) (showMessage . capitalize)

wrapNote :: Direction -> Found -> Maybe T.Text
wrapNote direction found
  | found.wrapped = Just $ case direction of
      Forward -> "wrapped around to the top"
      Backward -> "wrapped around to the bottom"
  | otherwise = Nothing

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

capitalize :: T.Text -> T.Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> t
