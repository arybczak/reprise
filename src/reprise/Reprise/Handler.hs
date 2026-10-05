-- | The handlers of events and actions. They are 'Eff' code over the state,
-- and they only request MPD commands and UI operations, so the tests run
-- them with pure handlers. Actions of a screen go on to the screen's module.
module Reprise.Handler
  ( runEvent
  , handleEvent
  , runAction
  ) where

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Effectful
import Effectful.Input.Static
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Find
import Reprise.Format
import Reprise.Groups
import Reprise.Handler.Core
import Reprise.Keymap
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Browser
import Reprise.Screen.Lyrics
import Reprise.Screen.Queue
import Reprise.Screen.SongInfo
import Reprise.Screen.Visualizer
import Reprise.Selection
import Reprise.State

-- | Handle an event at a monotonic time, with pure handlers that collect the
-- requests. It runs in 'IO', but without 'IOE' the code that handles the
-- event can't do any.
runEvent
  :: AppEnv -> Double -> AppEvent -> AppState -> IO (AppState, [PendingRequest], [UiCommand])
runEvent env now event s = do
  ((((), s'), requests), commands) <-
    runEff
      . collectUiRequests
      . collectMpdRequests
      . runInput env
      . runState (s & #now .~ now)
      $ do
        handleEvent event
        afterEvent
  pure (s', requests, commands)

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

----------------------------------------
-- Events

handleEvent :: App es => AppEvent -> Eff es ()
handleEvent = \case
  KeyPressed k -> handleKey k
  Resized w h -> do
    modifyS $ layoutViews . (#terminalSize .~ (w, h))
    modifyWithEnv (modifyView id)
  MpdConnected v -> do
    modifyS $ #connection .~ Connected v
    fetchQueue
    relistBrowser
  -- The player's status would be stale, but the queue stays to look at
  -- until the connection is back.
  MpdDisconnected reason ->
    modifyS $
      (#connection .~ Disconnected reason)
        . (#mirror % #status .~ Nothing)
        . (#seek .~ Nothing)
  MpdChanged subsystems -> do
    -- The changes of the queue come with the status.
    if
      | PlaylistSubsystem `elem` subsystems -> fetchQueueChanges
      | any (`elem` statusSubsystems) subsystems -> request status StatusFetched
      | otherwise -> pure ()
    browserChanged subsystems
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
  BrowserListed token entries -> browserListed token entries
  BrowserFailed token err -> browserFailed token err
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
  MpdDone -> keepScreen
  MpdFailed _ err -> showError $ T.pack (displayException err)
  Tick token ->
    getsS (.tick) >>= \case
      Just (t, _) | t == token -> modifyS $ #tick .~ Nothing
      _ -> keepScreen
  SeekCommit token -> commitSeek token
  MessageExpired token ->
    getsS (.message) >>= \case
      Just m | m.token == token -> modifyS $ #message .~ Nothing
      _ -> keepScreen
  -- One timer serves a run of keys: it waits again for the rest of the
  -- delay after the last key.
  HideCursor -> do
    s <- getS
    let remaining = s.lastInput + cursorHideDelay - s.now
    if remaining > 0
      then do
        after remaining HideCursor
        keepScreen
      else modifyS $ #cursorTimer .~ False
  VisualizerSamples samples -> visualizerSamples samples
  VisualizerSpectrum spectra -> visualizerSpectrum spectra
  VisualizerFailed reason -> showError reason
  LyricsFetching token fetcher -> lyricsFetching token fetcher
  LyricsLoaded token result -> lyricsLoaded token result
  Edited file failure -> lyricsEdited file failure
  SongCommentsFetched token comments -> songCommentsFetched token comments
  Confirmed action -> runConfirmed action
  where
    statusSubsystems :: [Subsystem]
    statusSubsystems = [PlayerSubsystem, MixerSubsystem, OptionsSubsystem, UpdateSubsystem, DatabaseSubsystem]

-- | Run after every event: schedule the next redraw of the elapsed time,
-- update the window title, read the visualizer's samples while it shows, and
-- fetch the lyrics of a new song that plays.
afterEvent :: App es => Eff es ()
afterEvent = do
  restartTitle
  scheduleTick
  updateWindowTitle
  updateVisualizer
  updateLyrics

-- | Scroll the header's title from its start when it shows another subject.
restartTitle :: App es => Eff es ()
restartTitle = do
  s <- getS
  when (fmap fst s.titleShown /= Just (titleSubject s))
    $ modifyS
    $ #titleShown ?~ (titleSubject s, s.now)

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
    modifyS $ #queueState % #selection %~ restrictTo ids
  modifyWithEnv (modifyView id)
  s <- getS
  let oldId = old.status >>= (.currentId)
      newId = new.status >>= (.currentId)
      loaded = isJust new.queueVersion && isJust new.status
  if
    | loaded && not s.jumpedToPlaying -> do
        modifyS $ #jumpedToPlaying .~ True
        modifyWithEnv jumpToPlaying
    | s.toggles.followPlaying && oldId /= newId -> modifyWithEnv jumpToPlaying
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
  env <- getAppEnv
  s <- getS
  modifyS $ #lastInput .~ s.now
  unless s.cursorTimer $ do
    modifyS $ #cursorTimer .~ True
    after cursorHideDelay HideCursor
  case s.prompt of
    Just p -> handlePromptKey p k
    Nothing -> case s.pendingKeys of
      Just pending
        | isCancel k -> modifyS $ #pendingKeys .~ Nothing
        | otherwise -> continue pending.layers (pending.keys <> [k])
      Nothing -> continue (startLayers (focusedView s).screen env.keymaps) [k]
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
          ForFind f -> modifyWithEnv (restoreView f.origin)
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

----------------------------------------
-- Actions

runAction :: App es => Action -> Eff es ()
runAction = \case
  action@(Move t) -> verb action $ \case
    QueueScreen -> Just $ modifyWithEnv (moveQueueCursor t)
    BrowserScreen -> Just $ modifyWithEnv (moveBrowserCursor t)
    LyricsScreen -> Just $ scrollLyrics t
    SongInfoScreen -> Just $ scrollLines t
    HelpScreen -> Just $ scrollLines t
    _ -> Nothing
  JumpToPlaying -> do
    modifyWithEnv . modifyView $ switchScreen QueueScreen
    modifyWithEnv jumpToPlaying
  JumpToBrowser ->
    getsS songUnderCursor >>= \case
      Nothing -> showMessage "There is no song under the cursor"
      Just song -> do
        modifyWithEnv . modifyView $ switchScreen BrowserScreen
        locateSong song
  action@Activate -> verb action $ \case
    QueueScreen -> Just activate
    BrowserScreen -> Just activateItem
    _ -> Nothing
  action@Parent -> verb action $ \case
    BrowserScreen -> Just leave
    _ -> Nothing
  action@EditLyrics -> verb action $ \case
    LyricsScreen -> Just editLyrics
    _ -> Nothing
  action@RefetchLyrics -> verb action $ \case
    LyricsScreen -> Just refetchLyrics
    _ -> Nothing
  action@NextSortMode -> verb action $ \case
    BrowserScreen -> Just nextSortMode
    _ -> Nothing
  action@(Add p) -> verb action $ \case
    BrowserScreen -> Just $ addMarked p
    _ -> Nothing
  action@AddAndPlay -> verb action $ \case
    BrowserScreen -> Just addAndPlay
    _ -> Nothing
  action@AddOrRemove -> verb action $ \case
    BrowserScreen -> Just addOrRemove
    _ -> Nothing
  action@(Select t) -> verb action $ \case
    QueueScreen -> Just $ select t
    BrowserScreen -> Just $ selectInBrowser t
    _ -> Nothing
  action@Delete -> verb action $ \case
    QueueScreen -> Just deleteMarked
    _ -> Nothing
  action@(Priority p) -> verb action $ \case
    QueueScreen -> Just $ prioritize p
    _ -> Nothing
  action@(MoveSelection t) -> verb action $ \case
    QueueScreen -> Just $ moveSelection t
    _ -> Nothing
  action@(Find t) -> verb action $ \screen ->
    if screen `elem` [QueueScreen, BrowserScreen]
      then Just $ case t of
        FindForward -> modifyS (startFind Forward)
        FindBackward -> modifyS (startFind Backward)
        FindNext -> findAgain Forward
        FindPrevious -> findAgain Backward
      else Nothing
  Crossfade n -> mutate $ setCrossfade n
  AddPath path -> mutate $ add path Nothing
  CommandPrompt start ->
    modifyS $
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
  Show LyricsScreen -> showLyrics
  Show SongInfoScreen -> showSongInfo
  Show screen
    | screen `elem` [QueueScreen, BrowserScreen, VisualizerScreen, HelpScreen] -> do
        modifyWithEnv . modifyView $ switchScreen screen
        when (screen == BrowserScreen) openBrowser
    | otherwise -> notAvailable $ "The " <> screenText screen
  Quit -> halt
  Clear -> do
    n <- getsS (queueLength . (.mirror))
    if n == 0
      then showMessage "The queue is empty"
      else modifyS $ confirm ("Clear " <> countSongs n <> " from the queue?") (Confirmed Clear)
  -- Shuffling the selected songs loses nothing the user didn't point at, so
  -- only shuffling the whole queue asks first.
  Shuffle -> do
    s <- getS
    let n = queueLength s.mirror
    case runs (selectedSongPositions s) of
      _
        | (focusedView s).screen /= QueueScreen || null (selectedSongPositions s) ->
            if n < 2
              then showMessage "There is nothing to shuffle"
              else
                modifyS $ confirm ("Shuffle " <> countSongs n <> " in the queue?") (Confirmed Shuffle)
      [(a, b)] -> do
        mutate . shuffle . Just $ Range (SongPos a) (Just (SongPos (b + 1)))
        showMessage $ "Shuffled " <> countSongs (b - a + 1)
      _ -> showError "Only selected songs next to each other can be shuffled"
  Update scope -> do
    s <- getS
    let path
          | scope == UpdateCurrent && (focusedView s).screen == BrowserScreen = browserDirectory s
          | otherwise = Nothing
    mutate . void $ update path
    showMessage $ "Updating " <> maybe "the database" ("/" <>) path
  action -> notAvailable $ "The action " <> renderAction action

-- | Run a verb the way the focused screen implements it. A screen that
-- doesn't implement it says so, instead of acting on another screen.
verb :: App es => Action -> (ScreenName -> Maybe (Eff es ())) -> Eff es ()
verb action implementation = do
  screen <- getsS ((.screen) . focusedView)
  case implementation screen of
    Just k -> k
    Nothing -> showMessage $ "The " <> screenText screen <> " has no " <> renderAction action

-- | Run a destructive action that the user confirmed.
runConfirmed :: App es => Action -> Eff es ()
runConfirmed = \case
  Clear -> mutate clear
  Shuffle -> mutate $ shuffle Nothing
  _ -> pure ()

confirm :: T.Text -> AppEvent -> AppState -> AppState
confirm question onYes = #prompt ?~ Prompt question (YesNo onYes)

withStatus :: App es => (Status -> Eff es ()) -> Eff es ()
withStatus k =
  getsS (.mirror.status) >>= \case
    Just st -> k st
    Nothing -> showError "Not connected to MPD"

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
  -- Of the browser in the browser, as ncmpcpp toggles the display of the
  -- screen that shows, and of the queue elsewhere.
  ToggleDisplay -> do
    screen <- getsS ((.screen) . focusedView)
    if screen == BrowserScreen
      then toggleBrowserDisplay
      else do
        modifyS $
          #toggles % #queueDisplay %~ \case
            Classic -> Columns
            Columns -> Classic
        modifyWithEnv (modifyView id)
        d <- getsS (.toggles.queueDisplay)
        showMessage $
          "Display: " <> case d of
            Classic -> "classic"
            Columns -> "columns"
  ToggleAlbumSeparators -> notAvailable "Album separators"
  -- Of the lyrics on the lyrics screen, and of the queue elsewhere.
  ToggleFollowPlaying -> do
    screen <- getsS ((.screen) . focusedView)
    if screen == LyricsScreen
      then toggleLyricsFollowing
      else do
        localToggle "Follow playing" #followPlaying
        follow <- getsS (.toggles.followPlaying)
        when follow (modifyWithEnv jumpToPlaying)
  ToggleBitrate -> localToggle "Bitrate" #showBitrate
  ToggleVisualization -> nextVisualization
  where
    localToggle :: App es => T.Text -> Lens' Toggles Bool -> Eff es ()
    localToggle name field = do
      modifyS $ #toggles % field %~ not
      v <- getsS (view (#toggles % field))
      showMessage $ name <> ": " <> onOff v

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
  rows <- focusedRows
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

-- | Keep the pattern for the next and the previous match, in every screen.
-- An empty pattern finds the last pattern again, as in Vim.
acceptFind :: App es => Finding -> T.Text -> Eff es ()
acceptFind f text
  | T.null text = findAgain f.direction
  | otherwise = case compilePattern text of
      Left _ -> do
        modifyWithEnv (restoreView f.origin)
        showError $ "Invalid pattern: " <> text
      Right _ -> do
        modifyS $ #findPattern ?~ text
        forM_ f.note (showMessage . capitalize)

-- | Move to the next or the previous match of the last pattern.
findAgain :: App es => Direction -> Eff es ()
findAgain direction = do
  rows <- focusedRows
  s <- getS
  case s.findPattern of
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

-- | The rows of the focused list as finds match them.
focusedRows :: App es => Eff es (Seq.Seq Folded)
focusedRows =
  getsS ((.screen) . focusedView) >>= \case
    QueueScreen -> queueRows
    BrowserScreen -> getsS (.browser.rows)
    _ -> pure Seq.empty

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
    _ -> keepScreen

currentDuration :: AppState -> Maybe Seconds
currentDuration s = s.mirror.status >>= (.duration)

----------------------------------------
-- Redraws

-- | Schedule a redraw for the next change of the screen with time: of the
-- elapsed time, the next whole second or the next cell of the progress bar,
-- or of the header's title while it scrolls.
scheduleTick :: App es => Eff es ()
scheduleTick = do
  env <- getAppEnv
  s <- getS
  forM_ (nextRedraw env s) $ \t -> case s.tick of
    Just (_, scheduled) | scheduled <= t -> pure ()
    _ -> do
      token <- newToken
      modifyS $ #tick ?~ (token, t)
      after (t - s.now) (Tick token)

nextRedraw :: AppEnv -> AppState -> Maybe Double
nextRedraw env s = case catMaybes [elapsedRedraw s, titleRedraw, nextLyricsLine s] of
  [] -> Nothing
  ts -> Just (minimum ts)
  where
    -- The next whole second since the title began to show its subject.
    titleRedraw :: Maybe Double
    titleRedraw = do
      guard $ titleScrolls env s
      let since = titleSince s
      pure $ since + fromIntegral (floor @Double @Int (s.now - since) + 1)

elapsedRedraw :: AppState -> Maybe Double
elapsedRedraw s = do
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
  env <- getAppEnv
  s <- getS
  forM_ env.config.windowTitle $ \fmt -> do
    let title = case (currentSong s.mirror, (.state) <$> s.mirror.status) of
          (Just song, Just st) | st /= Stopped -> renderPlain ctx song fmt
          _ -> "reprise"
        ctx = RenderContext env.config.lists.tagSeparator [Span Nothing env.config.lists.missingTag]
    when (s.windowTitle /= Just title) $ do
      modifyS $ #windowTitle ?~ title
      setTitle title
