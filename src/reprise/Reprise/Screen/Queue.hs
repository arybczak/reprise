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

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Map.Strict qualified as M
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

queueView :: AppState -> View -> V.Image
queueView s v =
  let ctx =
        RowContext
          { colorMode = s.colorMode
          , lists = s.config.lists
          , songs = s.config.songs
          , display = s.toggles.queueDisplay
          , width = v.width
          }
      titles
        | s.toggles.queueDisplay == Columns && s.config.songs.columns.showTitles =
            [renderTitles ctx]
        | otherwise = []
      playingId = s.mirror.status >>= (.currentId)
      visible = Seq.take (listHeight s v) (Seq.drop v.offset s.mirror.queue)
      -- The matches of a find show while the user types it.
      found = case s.prompt of
        Just (Prompt _ (Line edit (ForFind _)))
          | Right p <- compilePattern (lineEditText edit)
          , Right matched <-
              matchAll
                p
                (foldText . rowText s.config.lists s.config.songs s.toggles.queueDisplay <$> visible) ->
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

moveQueueCursor :: App es => MoveTarget -> Eff es ()
moveQueueCursor t = do
  s <- getS
  let h = max 1 (listHeight s (focusedView s))
      q = s.mirror.queue
      c = (focusedView s).cursor
  case t of
    MoveUp -> setCursor (c - 1)
    MoveDown -> setCursor (c + 1)
    MovePageUp -> setCursor (c - h)
    MovePageDown -> setCursor (c + h)
    MoveFirst -> setCursor 0
    MoveLast -> setCursor (Seq.length q - 1)
    MovePreviousAlbum -> jumpTo $ previousGroup albumKey q c
    MoveNextAlbum -> jumpTo $ nextGroup albumKey q c
    MovePreviousArtist -> jumpTo $ previousGroup artistKey q c
    MoveNextArtist -> jumpTo $ nextGroup artistKey q c

-- | Move the queue's cursor to the playing song in the middle of the list,
-- also while the view shows another screen.
jumpToPlaying :: App es => Eff es ()
jumpToPlaying = do
  s <- getS
  let v = focusedView s
      h = listHeight s (v & #screen .~ QueueScreen)
  forM_ (currentPosition s.mirror) $ \p ->
    if v.screen == QueueScreen
      then jumpTo p
      else modifyS $ #views % ix s.focus % #positions % at QueueScreen ?~ (p, p - h `div` 2)

-- | What tells artists apart: the album artist, or the artist without one,
-- so that a compilation whose songs have different artists is one artist.
artistKey :: Song -> Maybe [T.Text]
artistKey song = M.lookup AlbumArtist song.tags <|> M.lookup Artist song.tags

-- | What tells albums apart: the artist and the album. The album alone
-- would join albums of different artists with the same name, e.g. two
-- greatest hits next to each other.
albumKey :: Song -> (Maybe [T.Text], Maybe [T.Text])
albumKey song = (artistKey song, M.lookup Album song.tags)

-- | The first item after the group of the item at the index.
nextGroup :: Eq k => (Song -> k) -> Seq.Seq Song -> Int -> Int
nextGroup key q c = case Seq.lookup c q of
  Nothing -> c
  Just song ->
    maybe (Seq.length q - 1) (+ (c + 1)) $
      Seq.findIndexL ((/= key song) . key) (Seq.drop (c + 1) q)

-- | The first item of the group of the item at the index, or of the group
-- before it if the item is already the first.
previousGroup :: Eq k => (Song -> k) -> Seq.Seq Song -> Int -> Int
previousGroup key q c
  | c <= 0 = 0
  | otherwise =
      let start i = case Seq.lookup i q of
            Nothing -> i
            Just song -> maybe 0 (+ 1) $ Seq.findIndexR ((/= key song) . key) (Seq.take i q)
          s = start c
      in if s < c then s else start (c - 1)

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
      modifySelection $ if selected then S.delete i else S.insert i
      modifyS $
        #queueState % #lastSelected %~ \ends ->
          (if selected then id else take rangeEnds . (i :)) (filter (/= i) ends)
    forM_ andMove moveQueueCursor
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
    modifySelection (ids S.\\)
    showMessage "Selection inverted"
  SelectNone -> do
    modifySelection (const S.empty)
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
      modifySelection (S.union ids)

modifySelection :: App es => (S.Set SongId -> S.Set SongId) -> Eff es ()
modifySelection f = modifyS $ #queueState % #selection %~ f

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
          setCursor (c + delta)
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

startFind :: App es => Direction -> Eff es ()
startFind direction = do
  v <- getsS focusedView
  openLine question emptyLineEdit . ForFind $ Finding direction (v.cursor, v.offset) Nothing
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
      then restoreView f.origin >> pure Nothing
      else case compilePattern text of
        -- The cursor stays until the pattern is complete again.
        Left err -> pure (Just err)
        Right p -> case search p f.direction (fst f.origin) rows of
          Left err -> pure (Just err)
          Right Nothing -> restoreView f.origin >> pure (Just "no match")
          Right (Just found) -> do
            jumpTo found.index
            pure (wrapNote f.direction found)
  pure $ f & #note .~ note

-- | Keep the pattern for the next and the previous match. An empty pattern
-- finds the last pattern again, as in Vim.
acceptFind :: App es => Finding -> T.Text -> Eff es ()
acceptFind f text
  | T.null text = findAgain f.direction
  | otherwise = case compilePattern text of
      Left _ -> do
        restoreView f.origin
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
          jumpTo found.index
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
  s <- getS
  case s.queueState.findRows of
    Just r
      | r.version == s.mirror.queueVersion && r.display == s.toggles.queueDisplay -> pure r.rows
    _ -> do
      let display = s.toggles.queueDisplay
          rows = foldText . rowText s.config.lists s.config.songs display <$> s.mirror.queue
      modifyS $ #queueState % #findRows ?~ FindRows s.mirror.queueVersion display rows
      pure rows

capitalize :: T.Text -> T.Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> t
