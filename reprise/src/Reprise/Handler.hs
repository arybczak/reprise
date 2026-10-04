{-# LANGUAGE MultiWayIf #-}

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
import Reprise.Format
import Reprise.Keymap
import Reprise.Keys
import Reprise.Mpd.Mirror
import Reprise.Screen.Help
import Reprise.State

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
  MpdDisconnected reason -> modifyS $ #connection .~ Disconnected reason
  MpdChanged subsystems
    | PlaylistSubsystem `elem` subsystems -> fetchQueueChanges
    | any (`elem` statusSubsystems) subsystems -> request status StatusFetched
    | otherwise -> pure ()
  QueueFetched r -> withReply r $ \(st, songs) -> do
    now <- getsS (.now)
    updateMirror $ setQueue now st songs
  QueueChangesFetched r -> withReply r $ \(st, changes) -> do
    s <- getS
    case applyQueueChanges s.now st changes s.mirror of
      Right m -> updateMirror (const m)
      Left err -> do
        showError $ "The queue is out of sync, fetching it again: " <> err
        fetchQueue
  StatusFetched r -> withReply r $ \st -> do
    now <- getsS (.now)
    updateMirror $ setStatus now st
  ReplayGainFetched r -> withReply r $ \mode -> do
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
  MpdDone r -> withReply r pure
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

withReply :: App es => Either MpdError a -> (a -> Eff es ()) -> Eff es ()
withReply r k = either (showError . describeMpdError) k r

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
handlePromptKey p k = case p of
  Confirm _ onYes
    | k == KeySpec mempty (CharKey 'y') -> do
        modifyS $ #prompt .~ Nothing
        handleEvent onYes
    | k == KeySpec mempty (CharKey 'n') || isCancel k -> do
        modifyS $ #prompt .~ Nothing
        showMessage "Cancelled"
    | otherwise -> pure ()

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
  action@Delete -> onQueue action . withSongUnderCursor $ \song ->
    forM_ song.songId (mutate . deleteId)
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
      else confirm ("Clear " <> songs n <> " from the queue?") (Confirmed Clear)
  Shuffle -> do
    n <- getsS (queueLength . (.mirror))
    if n < 2
      then showMessage "There is nothing to shuffle"
      else confirm ("Shuffle " <> songs n <> " in the queue?") (Confirmed Shuffle)
  Update _ -> do
    mutate . void $ update Nothing
    showMessage "Updating the database"
  action@(MoveSelection MoveSelectionUp) -> onQueue action $ moveSongUnderCursor (-1)
  action@(MoveSelection MoveSelectionDown) -> onQueue action $ moveSongUnderCursor 1
  action -> notAvailable $ "The action " <> renderAction action
  where
    songs :: Int -> T.Text
    songs n = T.pack (show n) <> if n == 1 then " song" else " songs"

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

confirm :: App es => T.Text -> AppEvent -> Eff es ()
confirm question onYes = modifyS $ #prompt ?~ Confirm question onYes

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
  forM_ (currentPosition s.mirror) $ \p -> do
    -- modifyView brings the offset back into the list.
    let centered = p - h `div` 2
    if v.screen == QueueScreen
      then modifyView $ (#cursor .~ p) . (#offset .~ centered)
      else modifyS $ #views % ix s.focus % #positions % at QueueScreen ?~ (p, centered)

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
    QueueScreen -> setCursor $ case t of
      MoveUp -> c - 1
      MoveDown -> c + 1
      MovePageUp -> c - h
      MovePageDown -> c + h
      MoveFirst -> 0
      MoveLast -> Seq.length q - 1
      MovePreviousAlbum -> previousGroup albumKey q c
      MoveNextAlbum -> nextGroup albumKey q c
      MovePreviousArtist -> previousGroup artistKey q c
      MoveNextArtist -> nextGroup artistKey q c
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

    albumKey :: Song -> (Maybe [T.Text], Maybe [T.Text])
    albumKey song =
      ( M.lookup AlbumArtist song.tags <|> M.lookup Artist song.tags
      , M.lookup Album song.tags
      )

    artistKey :: Song -> Maybe [T.Text]
    artistKey song = M.lookup Artist song.tags

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

moveSongUnderCursor :: App es => Int -> Eff es ()
moveSongUnderCursor delta = do
  s <- getS
  let c = (focusedView s).cursor
      target = c + delta
  when (target >= 0 && target < Seq.length s.mirror.queue) $
    withSongUnderCursor $ \song -> forM_ song.songId $ \i -> do
      mutate . moveId i . At $ SongPos target
      setCursor target

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
  d <- realToFrac <$> st.duration
  e <- realToFrac <$> elapsedAt s.now s.mirror
  guard $ e < d
  let width = fromIntegral (fst s.terminalSize)
      nextSecond = fromIntegral (floor @Double @Int e + 1) - e
      nextCell
        | width > 0 = (fromIntegral (floor @Double @Int (e / d * width) + 1) * d / width) - e
        | otherwise = nextSecond
  pure $ s.now + min nextSecond nextCell

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
