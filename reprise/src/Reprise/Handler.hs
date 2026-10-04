-- | The handlers of events and actions. They are 'Eff' code over the state,
-- and they only request MPD commands and UI operations, so the tests run
-- them with pure handlers.
module Reprise.Handler
  ( -- * Running
    App
  , runEvent
  , handleEvent
  , runAction

    -- * Queries
  , listHeight
  , displayedElapsed
  , cursorVisible
  , describeMpdError

    -- * Constants
  , seekCommitDelay
  , messageTimeout
  , cursorHideDelay
  ) where

import Control.Applicative
import Control.Monad
import Data.Char hiding (Space)
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Effectful
import Effectful.State.Static.Local
import MPD.Command hiding (currentSong)
import MPD.Types
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Find
import Reprise.Format
import Reprise.Keymap
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Screen.Help
import Reprise.Screen.Queue
import Reprise.State
import Reprise.UI.SongList

-- | The effects of the handlers.
type App es = (State AppState :> es, MpdRequest :> es, UiRequest :> es)

-- | Handle an event at a monotonic time, with pure handlers that collect the
-- requests.
runEvent :: Double -> AppEvent -> AppState -> (AppState, [PendingRequest], [UiCommand])
runEvent now event s =
  let ((((), s'), requests), commands) =
        runPureEff
          . collectUiRequests
          . collectMpdRequests
          . runState (s & #now .~ now)
          $ do
            handleEvent event
            afterEvent
  in (s', requests, commands)

----------------------------------------
-- Constants

-- | The pause after the last seek key press that sends the seek. It must be
-- longer than the terminal's key repeat delay, or the gap before the first
-- repeat would end the seek: X11's default delay is 660 ms, and a repeat
-- comes every 40 ms at its default rate of 25 per second.
seekCommitDelay :: Double
seekCommitDelay = 0.66 + 1 / 25

-- | The seek step grows by a second for each this many seconds that the key
-- is held, as in ncmpcpp's incremental seeking.
seekAccelerationPeriod :: Double
seekAccelerationPeriod = 2

-- | How long a status bar message stays. ncmpcpp's default
-- @message_delay_time@.
messageTimeout :: Double
messageTimeout = 5

-- | How long after the last key the queue hides its cursor. ncmpcpp's
-- default @playlist_disable_highlight_delay@.
cursorHideDelay :: Double
cursorHideDelay = 5

----------------------------------------
-- Events

handleEvent :: App es => AppEvent -> Eff es ()
handleEvent = \case
  KeyPressed k -> handleKey k
  Resized w h -> do
    modifyS $ layoutViews . (#terminalSize .~ (w, h))
    modifyView id
  MpdConnected v -> do
    modifyS $ #connection .~ Connected v
    fetchQueue
  -- The player's status would be stale, but the queue stays to look at
  -- until the connection is back.
  MpdDisconnected reason ->
    modifyS $
      (#connection .~ Disconnected reason)
        . (#mirror % #status .~ Nothing)
        . (#seek .~ Nothing)
  MpdChanged subsystems
    | PlaylistSubsystem `elem` subsystems -> fetchQueueChanges
    | any (`elem` statusSubsystems) subsystems -> request status StatusFetched
    | otherwise -> pure ()
  QueueFetched (st, songs) -> do
    now <- getsS (.now)
    updateMirror $ setQueue now st songs
  QueueChangesFetched (st, changes) -> do
    s <- getS
    case applyQueueChanges s.now st changes s.mirror of
      Right m -> updateMirror (const m)
      Left err -> do
        showError $ "The queue is out of sync, fetching it again: " <> err
        fetchQueue
  StatusFetched st -> do
    now <- getsS (.now)
    updateMirror $ setStatus now st
  ReplayGainFetched mode -> do
    let nextMode = case mode of
          ReplayGainOff -> ReplayGainTrack
          ReplayGainTrack -> ReplayGainAlbum
          ReplayGainAlbum -> ReplayGainAuto
          ReplayGainAuto -> ReplayGainOff
    mutate $ setReplayGainMode nextMode
    showMessage $
      "Replay gain: " <> case nextMode of
        ReplayGainOff -> "off"
        ReplayGainTrack -> "track"
        ReplayGainAlbum -> "album"
        ReplayGainAuto -> "auto"
  MpdDone -> pure ()
  MpdFailed _ err -> showError $ describeMpdError err
  Tick token ->
    modifyS $
      #tick %~ \case
        Just (t, _) | t == token -> Nothing
        other -> other
  SeekCommit token -> commitSeek token
  MessageExpired token ->
    modifyS $
      #message %~ \case
        Just m | m.token == token -> Nothing
        other -> other
  Redraw -> pure ()
  Confirmed action -> runConfirmed action
  where
    statusSubsystems :: [Subsystem]
    statusSubsystems = [PlayerSubsystem, MixerSubsystem, OptionsSubsystem, UpdateSubsystem, DatabaseSubsystem]

-- | Run after every event: schedule the next redraw of the elapsed time and
-- update the window title.
afterEvent :: App es => Eff es ()
afterEvent = do
  scheduleTick
  updateWindowTitle

describeMpdError :: MpdError -> T.Text
describeMpdError = \case
  AckError ack -> ack.command <> ": " <> ack.message
  ProtocolError err -> "Protocol error: " <> err
  ConnectionError err -> case err of
    ConnectFailed reason -> "Can't connect to MPD: " <> reason
    UnsupportedVersion (Version a b c) ->
      "MPD "
        <> T.intercalate "." (map (T.pack . show) [a, b, c])
        <> " is too old, reprise needs 0.23 or newer"
    Closed -> "MPD closed the connection"
    Broken reason -> "The connection to MPD broke: " <> reason
    TimedOut -> "MPD didn't reply in time"

fetchQueue :: App es => Eff es ()
fetchQueue = request ((,) <$> status <*> playlistInfo) QueueFetched

-- | The status and the changes of the queue come in one command list, so
-- the changes match the length in the status.
fetchQueueChanges :: App es => Eff es ()
fetchQueueChanges =
  getsS (.mirror.queueVersion) >>= \case
    Just v -> request ((,) <$> status <*> plChanges v) QueueChangesFetched
    Nothing -> fetchQueue

-- | Change the mirror and react to the changes: keep the cursors in the
-- list, follow the playing song, and report changed options.
updateMirror :: App es => (Mirror -> Mirror) -> Eff es ()
updateMirror f = do
  old <- getsS (.mirror)
  modifyS $ #mirror %~ f
  new <- getsS (.mirror)
  -- Songs that left the queue leave the selection.
  when (new.queueVersion /= old.queueVersion) $ do
    let ids = S.fromList . mapMaybe (.songId) $ toList new.queue
    modifySelection (`S.intersection` ids)
  modifyView id
  s <- getS
  let oldId = old.status >>= (.currentId)
      newId = new.status >>= (.currentId)
      loaded = isJust new.queueVersion && isJust new.status
  if
    | loaded && not s.jumpedToPlaying -> do
        modifyS $ #jumpedToPlaying .~ True
        jumpToPlaying
    | s.toggles.followPlaying && oldId /= newId -> jumpToPlaying
    | otherwise -> pure ()
  case (old.status, new.status) of
    (Just a, Just b) -> reportOptions a b
    _ -> pure ()

reportOptions :: App es => Status -> Status -> Eff es ()
reportOptions a b =
  case catMaybes changes of
    [] -> pure ()
    ms -> showMessage $ T.intercalate ", " ms
  where
    changes :: [Maybe T.Text]
    changes =
      [ changed (.repeat) $ "Repeat: " <> onOff b.repeat
      , changed (.random) $ "Random: " <> onOff b.random
      , changed (.single) $
          "Single: " <> case b.single of
            SingleOff -> "off"
            SingleOn -> "on"
            SingleOneshot -> "oneshot"
      , changed (.consume) $
          "Consume: " <> case b.consume of
            ConsumeOff -> "off"
            ConsumeOn -> "on"
            ConsumeOneshot -> "oneshot"
      , changed (.crossfade) $ "Crossfade: " <> T.pack (show b.crossfade) <> "s"
      ]

    changed :: Eq a => (Status -> a) -> T.Text -> Maybe T.Text
    changed field msg = if field a /= field b then Just msg else Nothing

onOff :: Bool -> T.Text
onOff b = if b then "on" else "off"

----------------------------------------
-- Keys

handleKey :: App es => KeySpec -> Eff es ()
handleKey k = do
  s <- getS
  modifyS $ #lastInput .~ s.now
  after cursorHideDelay Redraw
  case s.prompt of
    Just p -> handlePromptKey p k
    Nothing -> case s.pendingKeys of
      Just pending
        | isCancel k -> modifyS $ #pendingKeys .~ Nothing
        | otherwise -> continue pending.layers (pending.keys <> [k])
      Nothing -> continue (startLayers (focusedView s).screen s.keymaps) [k]
  where
    continue :: App es => Layers -> [KeySpec] -> Eff es ()
    continue layers keys = case lookupKey layers k of
      Bound a -> do
        modifyS $ #pendingKeys .~ Nothing
        runAction a
      Pending layers' -> modifyS $ #pendingKeys ?~ PendingKeys keys layers'
      Unbound -> do
        modifyS $ #pendingKeys .~ Nothing
        when (length keys > 1) . showMessage $
          T.unwords (map renderKeySpec keys) <> " is not bound"

isCancel :: KeySpec -> Bool
isCancel k = k `elem` [plain Escape, ctrl 'g']
  where
    plain :: Key -> KeySpec
    plain = KeySpec mempty

    ctrl :: Char -> KeySpec
    ctrl c = KeySpec (S.singleton Ctrl) (CharKey c)

handlePromptKey :: App es => Prompt -> KeySpec -> Eff es ()
handlePromptKey p k = case p.input of
  YesNo onYes
    | k == KeySpec mempty (CharKey 'y') -> do
        close
        handleEvent onYes
    | k == KeySpec mempty (CharKey 'n') || isCancel k -> do
        close
        showMessage "Cancelled"
    | otherwise -> pure ()
  Line edit purpose
    | k == KeySpec mempty Enter -> do
        close
        answer purpose (lineEditText edit)
    | isCancel k -> do
        close
        case purpose of
          ForFind f -> restoreView f.origin
          _ -> pure ()
    | Just edit' <- editLine k edit -> do
        purpose' <- case purpose of
          ForFind f -> ForFind <$> findAsYouType f (lineEditText edit')
          other -> pure other
        modifyS $ #prompt ?~ Prompt p.question (Line edit' purpose')
    | otherwise -> pure ()
  where
    close :: App es => Eff es ()
    close = modifyS $ #prompt .~ Nothing

-- | Run what a line prompt asked for. An empty line of @:@ does nothing.
answer :: App es => LinePurpose -> T.Text -> Eff es ()
answer purpose text = case purpose of
  ForFind f -> acceptFind f text
  ForCommand
    | T.null (T.strip text) -> pure ()
    | otherwise -> either showError runAction (parseAction text)

openLine :: App es => T.Text -> LineEdit -> LinePurpose -> Eff es ()
openLine question edit purpose = modifyS $ #prompt ?~ Prompt question (Line edit purpose)

----------------------------------------
-- Find

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
  s <- getS
  note <-
    if T.null text
      then restoreView f.origin >> pure Nothing
      else case compilePattern text of
        -- The cursor stays until the pattern is complete again.
        Left err -> pure (Just err)
        Right p -> case search p f.direction (fst f.origin) (queueTexts s) of
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
  s <- getS
  case s.queueState.findPattern of
    Nothing -> showMessage "Nothing was found yet"
    Just text -> case compilePattern text of
      Left err -> showError (capitalize err)
      Right p -> case search p direction (focusedView s).cursor (queueTexts s) of
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

-- | The rows of the queue as text, which find matches.
queueTexts :: AppState -> Seq.Seq T.Text
queueTexts s = rowText s.config.lists s.config.songs s.toggles.queueDisplay <$> s.mirror.queue

restoreView :: App es => (Int, Int) -> Eff es ()
restoreView (c, o) = modifyView $ (#cursor .~ c) . (#offset .~ o)

capitalize :: T.Text -> T.Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> t

----------------------------------------
-- Actions

runAction :: App es => Action -> Eff es ()
runAction = \case
  Move t -> moveCursor t
  JumpToPlaying -> do
    modifyView $ switchScreen QueueScreen
    jumpToPlaying
  action@Activate -> onQueue action . withSongUnderCursor $ \song ->
    forM_ song.songId (mutate . playId)
  action@(Select t) -> onQueue action $ select t
  action@Delete -> onQueue action $ do
    ps <- getsS markedPositions
    mutate $ deletePositions ps
  action@(Priority p) -> onQueue action $ do
    s <- getS
    let ids = mapMaybe (\i -> Seq.lookup i s.mirror.queue >>= (.songId)) (markedPositions s)
    mutate $ prioId p ids
    showMessage $ "Priority " <> T.pack (show p) <> " set for " <> countSongs (length ids)
  action@(MoveSelection t) -> onQueue action $ moveSelection t
  action@(Find t) -> onQueue action $ case t of
    FindForward -> startFind Forward
    FindBackward -> startFind Backward
    FindNext -> findAgain Forward
    FindPrevious -> findAgain Backward
  Crossfade n -> mutate $ setCrossfade n
  AddPath path -> mutate $ add path Nothing
  CommandPrompt start ->
    openLine ":" (LineEdit (if T.null start then start else start <> " ") "") ForCommand
  Pause -> withStatus $ \st -> mutate $ case st.state of
    Playing -> pause True
    Paused -> pause False
    Stopped -> play Nothing
  Stop -> mutate stop
  Previous -> mutate previous
  Next -> mutate next
  Replay -> withStatus $ \st -> case st.state of
    Stopped -> mutate $ play st.currentPosition
    _ -> mutate . seekCur $ SeekTo 0
  Seek t -> seekAction t
  Volume v -> withStatus $ \st -> case st.volume of
    Nothing -> showError "MPD has no mixer, so the volume can't change"
    Just _ -> mutate $ case v of
      VolumeBy n -> changeVolume n
      VolumeTo n -> setVolume n
  Toggle t -> toggle t
  Show screen
    | screen `elem` [QueueScreen, HelpScreen] -> modifyView $ switchScreen screen
    | otherwise -> notAvailable $ "The " <> screenText screen
  Quit -> halt
  Clear -> do
    n <- getsS (queueLength . (.mirror))
    if n == 0
      then showMessage "The queue is empty"
      else confirm ("Clear " <> countSongs n <> " from the queue?") (Confirmed Clear)
  -- Shuffling the selected songs loses nothing the user didn't point at, so
  -- only shuffling the whole queue asks first.
  Shuffle -> do
    s <- getS
    let n = queueLength s.mirror
    case runs (selectedPositions s) of
      _
        | (focusedView s).screen /= QueueScreen || null (selectedPositions s) ->
            if n < 2
              then showMessage "There is nothing to shuffle"
              else confirm ("Shuffle " <> countSongs n <> " in the queue?") (Confirmed Shuffle)
      [(a, b)] -> do
        mutate . shuffle . Just $ Range (SongPos a) (Just (SongPos (b + 1)))
        showMessage $ "Shuffled " <> countSongs (b - a + 1)
      _ -> showError "Only selected songs next to each other can be shuffled"
  Update _ -> do
    mutate . void $ update Nothing
    showMessage "Updating the database"
  action -> notAvailable $ "The action " <> renderAction action

countSongs :: Int -> T.Text
countSongs n = T.pack (show n) <> if n == 1 then " song" else " songs"

-- | Run a verb that only the queue implements so far. Another screen says
-- that it doesn't implement it, instead of acting on the queue.
onQueue :: App es => Action -> Eff es () -> Eff es ()
onQueue action k = do
  screen <- getsS ((.screen) . focusedView)
  if screen == QueueScreen
    then k
    else showMessage $ "The " <> screenText screen <> " has no " <> renderAction action

screenText :: ScreenName -> T.Text
screenText screen = T.replace "_" " " (screenName screen) <> " screen"

-- | Run a destructive action that the user confirmed.
runConfirmed :: App es => Action -> Eff es ()
runConfirmed = \case
  Clear -> mutate clear
  Shuffle -> mutate $ shuffle Nothing
  _ -> pure ()

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
    forM_ andMove moveCursor
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
    s <- getS
    case compilePattern <$> s.queueState.findPattern of
      Nothing -> showMessage "Nothing was found yet"
      Just (Left err) -> showError (capitalize err)
      Just (Right p) -> case matchAll p (queueTexts s) of
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

confirm :: App es => T.Text -> AppEvent -> Eff es ()
confirm question onYes = modifyS $ #prompt ?~ Prompt question (YesNo onYes)

withStatus :: App es => (Status -> Eff es ()) -> Eff es ()
withStatus k =
  getsS (.mirror.status) >>= \case
    Just st -> k st
    Nothing -> showError "Not connected to MPD"

withSongUnderCursor :: App es => (Song -> Eff es ()) -> Eff es ()
withSongUnderCursor k = do
  s <- getS
  forM_ (Seq.lookup (focusedView s).cursor s.mirror.queue) k

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

-- | Move the cursor to an item in the middle of the list, as every jump
-- does, so that the item's neighbours show on both sides.
jumpTo :: App es => Int -> Eff es ()
jumpTo p = do
  h <- getsS (\s -> listHeight s (focusedView s))
  -- modifyView brings the offset back into the list.
  modifyView $ (#cursor .~ p) . (#offset .~ p - h `div` 2)

toggle :: App es => ToggleTarget -> Eff es ()
toggle = \case
  ToggleRepeat -> withStatus $ \st -> mutate $ setRepeat (not st.repeat)
  ToggleRandom -> withStatus $ \st -> mutate $ setRandom (not st.random)
  ToggleSingle -> withStatus $ \st ->
    mutate . setSingle $ if st.single == SingleOff then SingleOn else SingleOff
  ToggleConsume -> withStatus $ \st ->
    mutate . setConsume $ if st.consume == ConsumeOff then ConsumeOn else ConsumeOff
  ToggleCrossfade n -> withStatus $ \st ->
    mutate . setCrossfade $ if st.crossfade > 0 then 0 else n
  ToggleReplayGain -> request replayGainStatus ReplayGainFetched
  ToggleDisplay -> do
    modifyS $
      #toggles % #queueDisplay %~ \case
        Classic -> Columns
        Columns -> Classic
    modifyView id
    d <- getsS (.toggles.queueDisplay)
    showMessage $
      "Display: " <> case d of
        Classic -> "classic"
        Columns -> "columns"
  ToggleAlbumSeparators -> notAvailable "Album separators"
  ToggleFollowPlaying -> do
    localToggle "Follow playing" #followPlaying
    follow <- getsS (.toggles.followPlaying)
    when follow jumpToPlaying
  ToggleBitrate -> localToggle "Bitrate" #showBitrate
  where
    localToggle :: App es => T.Text -> Lens' Toggles Bool -> Eff es ()
    localToggle name field = do
      modifyS $ #toggles % field %~ not
      v <- getsS (view (#toggles % field))
      showMessage $ name <> ": " <> onOff v

----------------------------------------
-- The cursor

moveCursor :: App es => MoveTarget -> Eff es ()
moveCursor t = do
  s <- getS
  let view_ = focusedView s
      h = max 1 (listHeight s view_)
      q = s.mirror.queue
      c = view_.cursor
  case view_.screen of
    QueueScreen -> case t of
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
    -- A text without items scrolls.
    screen -> case t of
      MoveUp -> scroll (-1)
      MoveDown -> scroll 1
      MovePageUp -> scroll (-h)
      MovePageDown -> scroll h
      MoveFirst -> modifyView $ #offset .~ 0
      MoveLast -> modifyView $ #offset .~ screenLength s screen
      _ -> showMessage $ "The " <> screenText screen <> " has no " <> renderAction (Move t)
  where
    scroll :: App es => Int -> Eff es ()
    scroll delta = modifyView $ #offset %~ (+ delta)

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

-- | Move the cursor of the focused view and scroll it into view.
setCursor :: App es => Int -> Eff es ()
setCursor c = modifyView $ #cursor .~ c

-- | Change the focused view, then keep its cursor in the list and visible.
-- A screen of text has no cursor, so only its offset is kept in the text.
modifyView :: App es => (View -> View) -> Eff es ()
modifyView f = do
  s <- getS
  let centered = s.config.lists.keepCursorCentered
  modifyS $
    #views % ix s.focus %~ \v ->
      let v' = f v
          n = screenLength s v'.screen
          h = max 1 (listHeight s v')
          c = max 0 (min (n - 1) v'.cursor)
          o
            | v'.screen /= QueueScreen = v'.offset
            | centered = c - h `div` 2
            | c < v'.offset = c
            | c >= v'.offset + h = c - h + 1
            | otherwise = v'.offset
      in v' & #cursor .~ c & #offset .~ max 0 (min (n - h) o)

-- | The number of items or lines of a screen.
screenLength :: AppState -> ScreenName -> Int
screenLength s = \case
  QueueScreen -> Seq.length s.mirror.queue
  HelpScreen -> length (helpLines s.keymaps)
  _ -> 0

-- | The number of rows of the list in a view: the titles of the columns
-- take one.
listHeight :: AppState -> View -> Int
listHeight s v
  | v.screen == QueueScreen
  , s.toggles.queueDisplay == Columns
  , s.config.songs.columns.showTitles =
      max 0 (v.height - 1)
  | otherwise = v.height

-- | Whether the queue shows its cursor: it hides it a while after the last
-- key.
cursorVisible :: AppState -> Bool
cursorVisible s = s.now - s.lastInput < cursorHideDelay

----------------------------------------
-- Seeking

seekAction :: App es => SeekStep -> Eff es ()
seekAction = \case
  SeekBy n -> do
    s <- getS
    case (currentDuration s, displayedElapsed s) of
      (Just d, Just e) -> do
        let (base, started) = case s.seek of
              Just sk -> (sk.target, sk.started)
              Nothing -> (e, s.now)
            held = s.now - started
            step =
              fromIntegral (abs n) + fromIntegral (floor @Double @Int (held / seekAccelerationPeriod))
            target = max 0 . min d $ if n >= 0 then base + step else base - step
        token <- newToken
        modifyS $ #seek ?~ SeekState target started token
        after seekCommitDelay (SeekCommit token)
      _ -> showError "The current song has no length"
  SeekToSecond n -> mutate . seekCur . SeekTo $ fromIntegral n
  SeekToPercent p -> do
    s <- getS
    case currentDuration s of
      Just d -> mutate . seekCur . SeekTo $ d * fromIntegral p / 100
      Nothing -> showError "The current song has no length"

commitSeek :: App es => Int -> Eff es ()
commitSeek token =
  getsS (.seek) >>= \case
    Just sk | sk.token == token -> do
      now <- getsS (.now)
      modifyS $
        (#seek .~ Nothing)
          . (#mirror % #status % _Just % #elapsed ?~ sk.target)
          . (#mirror % #statusTime .~ now)
      mutate . seekCur $ SeekTo sk.target
    _ -> pure ()

currentDuration :: AppState -> Maybe Seconds
currentDuration s = s.mirror.status >>= (.duration)

-- | The elapsed time to show: the target of a seek in progress, or the
-- interpolated elapsed time.
displayedElapsed :: AppState -> Maybe Seconds
displayedElapsed s = case s.seek of
  Just sk -> Just sk.target
  Nothing -> elapsedAt s.now s.mirror

----------------------------------------
-- Redraws

-- | Schedule a redraw for the next change of the elapsed time on the screen:
-- the next whole second, or the next cell of the progress bar.
scheduleTick :: App es => Eff es ()
scheduleTick = do
  s <- getS
  forM_ (nextRedraw s) $ \t -> case s.tick of
    Just (_, scheduled) | scheduled <= t -> pure ()
    _ -> do
      token <- newToken
      modifyS $ #tick ?~ (token, t)
      after (t - s.now) (Tick token)

nextRedraw :: AppState -> Maybe Double
nextRedraw s = do
  st <- s.mirror.status
  guard $ st.state == Playing && isNothing s.seek
  e <- realToFrac <$> elapsedAt s.now s.mirror
  let width = fromIntegral (fst s.terminalSize)
      nextSecond = fromIntegral (floor @Double @Int e + 1) - e
  delay <- case realToFrac <$> st.duration of
    -- A stream has no length, so it has no progress bar to move.
    Nothing -> Just nextSecond
    Just d
      | e >= d -> Nothing
      | width > 0 ->
          Just . min nextSecond $
            (fromIntegral (floor @Double @Int (e / d * width) + 1) * d / width) - e
      | otherwise -> Just nextSecond
  pure $ s.now + delay

updateWindowTitle :: App es => Eff es ()
updateWindowTitle = do
  s <- getS
  forM_ s.config.windowTitle $ \fmt -> do
    let title = case (currentSong s.mirror, (.state) <$> s.mirror.status) of
          (Just song, Just st) | st /= Stopped -> renderPlain ctx song fmt
          _ -> "reprise"
        ctx = RenderContext s.config.lists.tagSeparator [Span Nothing s.config.lists.missingTag]
    when (s.windowTitle /= Just title) $ do
      modifyS $ #windowTitle ?~ title
      setTitle title

----------------------------------------
-- Messages

showMessage :: App es => T.Text -> Eff es ()
showMessage = message False

notAvailable :: App es => T.Text -> Eff es ()
notAvailable what = showMessage $ what <> " isn't available yet"

showError :: App es => T.Text -> Eff es ()
showError = message True

message :: App es => Bool -> T.Text -> Eff es ()
message isError text = do
  token <- newToken
  modifyS $ #message ?~ Message text isError token
  after messageTimeout (MessageExpired token)

----------------------------------------
-- Helpers

newToken :: App es => Eff es Int
newToken = state @AppState $ \s -> (s.nextToken, s & #nextToken %~ (+ 1))

getS :: App es => Eff es AppState
getS = get @AppState

getsS :: App es => (AppState -> a) -> Eff es a
getsS = gets @AppState

modifyS :: App es => (AppState -> AppState) -> Eff es ()
modifyS = modify @AppState
