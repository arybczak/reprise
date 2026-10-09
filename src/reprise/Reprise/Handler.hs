-- | The handlers of events and actions. They are 'Eff' code over the state,
-- and they only request MPD commands and UI operations, so the tests run
-- them with pure handlers. Actions of a screen go on to the screen's module.
module Reprise.Handler
  ( runEvent
  ) where

import Control.Monad
import Data.Foldable
import Data.Functor
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
import Reprise.Exception
import Reprise.Find
import Reprise.Format
import Reprise.Handler.Core
import Reprise.Header
import Reprise.History
import Reprise.Keymap
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Save
import Reprise.Screen.Browser
import Reprise.Screen.Help
import Reprise.Screen.Lyrics
import Reprise.Screen.Outputs
import Reprise.Screen.Queue
import Reprise.Screen.SongInfo
import Reprise.Screen.Visualizer
import Reprise.Selection
import Reprise.State
import Reprise.UI.SongList

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
  Started -> showScreen . (.config.startupScreen) =<< getAppEnv
  KeyPressed k -> handleKey k
  Resized w h -> do
    modifyS $ layoutViews . (#terminalSize .~ (w, h))
    modifyWithEnv (modifyView id)
  MpdConnected v -> do
    modifyS $ #connection .~ Connected v
    fetchQueue
    relistBrowser
    refreshOutputs
  -- The player's status would be stale, but the queue stays to look at
  -- until the connection is back.
  MpdDisconnected reason ->
    modifyS $
      (#connection .~ Disconnected reason)
        . (#mirror %~ forgetStatus)
        . (#seek .~ Nothing)
  MpdChanged subsystems -> do
    -- The changes of the queue come with the status.
    if
      | PlaylistSubsystem `elem` subsystems -> fetchQueueChanges
      | any (`elem` statusSubsystems) subsystems -> request status StatusFetched
      | otherwise -> pure ()
    browserChanged subsystems
    when (OutputSubsystem `elem` subsystems) refreshOutputs
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
    let nextMode = cycleNext mode
    mutate $ setReplayGainMode nextMode
    showMessage $ "Replay gain: " <> replayGainModeName nextMode
  MpdDone -> keepScreen
  MpdFailed _ err -> showError $ exceptionText err
  -- MPD's requests wait for the answer, so the prompt replaces any other.
  PasswordNeeded err -> do
    let reason = case err of
          AckError ack
            | ack.code == AckPassword -> "Wrong password"
            | otherwise -> "MPD refused " <> ack.command
          _ -> exceptionText err
    modifyS $
      (#pendingKeys .~ Nothing)
        . openLine (reason <> ". Password: ") emptyLineEdit ForPassword
  Tick token -> whenCurrent (.tick) fst token $ \_ -> modifyS $ #tick .~ Nothing
  SeekCommit token -> commitSeek token
  MessageExpired token ->
    whenCurrent (.message) (.token) token $ \_ -> modifyS $ #message .~ Nothing
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
  VisualizerSpectrum left right -> visualizerSpectrum left right
  VisualizerWave samples -> visualizerWave samples
  VisualizerFailed reason -> showError reason
  VisualizerStats frameStats -> visualizerStats frameStats
  LyricsFetching token fetcher -> lyricsFetching token fetcher
  LyricsLoaded token result -> lyricsLoaded token result
  Edited file failure -> lyricsEdited file failure
  EditLyricsFile file -> editLyricsFile file
  SongCommentsFetched token comments -> songCommentsFetched token comments
  OutputsFetched fetched -> outputsFetched fetched
  Confirmed action -> runConfirmed action
  SaveChecked name source exists -> saveChecked name source exists
  SaveTo name source mode -> saveTo name source mode
  where
    statusSubsystems :: [Subsystem]
    statusSubsystems = [PlayerSubsystem, MixerSubsystem, OptionsSubsystem, UpdateSubsystem, DatabaseSubsystem]

-- | Run after every event: fetch the lyrics of a new song that plays,
-- schedule the next redraw of the elapsed time, update the window title,
-- and read the visualizer's samples while it shows. The lyrics go first, as
-- the header's title can show their song.
afterEvent :: App es => Eff es ()
afterEvent = do
  updateLyrics
  restartTitle
  scheduleTick
  updateWindowTitle
  updateVisualizer

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
      -- The queue comes with the status, the first time too.
      firstQueue = isNothing old.queueVersion && isJust new.queueVersion
  -- A seek that the keys moved is of the song that played.
  when (oldId /= newId) dropSeek
  if
    | firstQueue -> modifyWithEnv jumpToPlaying
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
    _ | k == alwaysQuit -> halt
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

handlePromptKey :: App es => Prompt -> KeySpec -> Eff es ()
handlePromptKey p k = case p.input of
  Choice options
    | Just o <- find (\o -> k == char o.letter) options -> do
        close
        maybe (showMessage "Cancelled") handleEvent o.event
    | isCancel k -> do
        close
        showMessage "Cancelled"
    | otherwise -> pure ()
  Line edit purpose recall
    | k == plain Enter -> do
        close
        let text = lineEditText edit
        when (purpose /= ForPassword) $ do
          modifyS $ #history %~ remember text
          saveToHistory text
        answer purpose text
    | isCancel k -> do
        close
        case purpose of
          ForFind f -> modifyWithEnv (restoreView f.origin)
          ForPassword -> answerPassword Nothing
          _ -> pure ()
    | otherwise -> do
        history <- getsS (.history)
        forM_ (lineKey history edit purpose recall) $ \(edit', recall') -> do
          purpose' <- case purpose of
            ForFind f -> ForFind <$> findAsYouType f (lineEditText edit')
            other -> pure other
          modifyS $ #prompt ?~ Prompt p.question (Line edit' purpose' recall')
  where
    close :: App es => Eff es ()
    close = modifyS $ #prompt .~ Nothing

    -- The line after a key that edits it or recalls a line of the history.
    -- The history leaves out the password, and the password the history.
    lineKey
      :: [T.Text] -> LineEdit -> LinePurpose -> Maybe Recall -> Maybe (LineEdit, Maybe Recall)
    lineKey history edit purpose recall
      | purpose == ForPassword = edited
      | k `elem` [plain ArrowUp, ctrl 'p'] =
          fmap Just <$> recallOlder history edit recall
      | k `elem` [plain ArrowDown, ctrl 'n'] =
          recallNewer history <$> recall
      | k == plain PageUp = fmap Just <$> recallOldest history edit recall
      -- As bash's end-of-history, back to the typed line.
      | k == plain PageDown = (\r -> (r.typed, Nothing)) <$> recall
      | otherwise = edited
      where
        -- An edit makes the recalled line the typed one.
        edited :: Maybe (LineEdit, Maybe Recall)
        edited = (,Nothing) <$> editLine k edit

-- | Run what a line prompt asked for. An empty line of @:@ does nothing.
answer :: App es => LinePurpose -> T.Text -> Eff es ()
answer purpose text = case purpose of
  ForFind f -> acceptFind f text
  ForSave source -> saveNamed source (T.strip text)
  ForPassword -> answerPassword (Just text)
  ForCommand
    | T.null (T.strip text) -> pure ()
    | otherwise -> either showError runAction (parseAction text)

----------------------------------------
-- Actions

-- | Run an action. A verb, e.g. activate, goes to the focused screen, which
-- does it its own way.
runAction :: App es => Action -> Eff es ()
runAction action = case action of
  Move _ -> verb action
  JumpToPlaying -> verb action
  Back -> goBack
  JumpToBrowser -> showSongScreen BrowserScreen locateSong
  Activate -> verb action
  Parent -> verb action
  Save -> verb action
  EditLyrics -> verb action
  RefetchLyrics -> verb action
  NextSortMode -> verb action
  Add _ -> verb action
  AddAndPlay -> verb action
  AddOrRemove -> verb action
  Select _ -> verb action
  Delete -> verb action
  Priority _ -> verb action
  MoveSongs _ -> verb action
  Find _ -> verb action
  NextColumn -> notAvailable $ "The action " <> renderAction action
  PreviousColumn -> notAvailable $ "The action " <> renderAction action
  Crossfade n -> mutate $ setCrossfade n
  AddPath path -> mutate $ add path Nothing
  CommandPrompt start ->
    modifyS $
      openLine ":" (LineEdit (if T.null start then start else start <> " ") "") ForCommand
  Pause -> withStatus $ \st -> mutate $ case st.state of
    Playing -> pause True
    Paused -> pause False
    Stopped -> play Nothing
  Stop -> dropSeek >> mutate stop
  Previous -> mutate previous
  Next -> mutate next
  Replay -> withStatus $ \st -> do
    dropSeek
    mutate $ case st.state of
      Stopped -> play st.currentPosition
      _ -> seekCur $ SeekTo 0
  Seek t -> seekAction t
  Volume v -> withStatus $ \st -> case st.volume of
    Nothing -> showError "MPD has no mixer, so the volume can't change"
    Just _ -> mutate $ case v of
      VolumeBy n -> changeVolume n
      VolumeTo n -> setVolume n
  Toggle t -> toggle t
  Show screen -> showScreen screen
  NextScreen screens -> cycleScreens screens
  PreviousScreen screens -> cycleScreens (reverse screens)
  Quit -> halt
  Clear -> do
    n <- getsS (queueLength . (.mirror))
    if n == 0
      then showMessage "The queue is empty"
      else
        modifyS $
          confirm ("Clear " <> countSongs n <> " from the queue?") (Confirmed ConfirmClear)
  Shuffle -> verbOr shuffleQueue action
  Update _ -> verbOr (updateDatabase Nothing) action

-- | Run a verb the way the focused screen implements it. A screen that
-- doesn't implement it says so, instead of acting on another screen.
verb :: App es => Action -> Eff es ()
verb action = do
  screen <- getsS ((.screen) . focusedView)
  verbOr (screenHasNo screen (renderAction action)) action

-- | Run a verb the way the focused screen implements it, else the other
-- way.
verbOr :: App es => Eff es () -> Action -> Eff es ()
verbOr otherwise' action = do
  screen <- getsS ((.screen) . focusedView)
  fromMaybe otherwise' (screenVerb screen action)

-- | How a screen does a verb, if it does it. A screen finds if it has rows
-- to find in.
screenVerb :: App es => ScreenName -> Action -> Maybe (Eff es ())
screenVerb screen = \case
  Find t ->
    screenRows screen <&> \rows -> case t of
      FindForward -> modifyS (startFind Forward)
      FindBackward -> modifyS (startFind Backward)
      FindNext -> findAgain rows Forward
      FindPrevious -> findAgain rows Backward
  action -> case screen of
    QueueScreen -> queueVerb action
    BrowserScreen -> browserVerb action
    SearchEngineScreen -> Nothing
    MediaLibraryScreen -> Nothing
    PlaylistEditorScreen -> Nothing
    OutputsScreen -> outputsVerb action
    VisualizerScreen -> Nothing
    LyricsScreen -> lyricsVerb action
    SongInfoScreen -> songInfoVerb action
    HelpScreen -> helpVerb action

-- | Show a screen in the focused view, with what the screen does when it
-- shows, e.g. load the lyrics of the song under the cursor.
showScreen :: App es => ScreenName -> Eff es ()
showScreen = \case
  QueueScreen -> switchTo QueueScreen
  BrowserScreen -> switchTo BrowserScreen >> openBrowser
  SearchEngineScreen -> notBuilt SearchEngineScreen
  MediaLibraryScreen -> notBuilt MediaLibraryScreen
  PlaylistEditorScreen -> notBuilt PlaylistEditorScreen
  OutputsScreen -> showOutputs
  VisualizerScreen -> switchTo VisualizerScreen
  LyricsScreen -> showLyrics
  SongInfoScreen -> showSongInfo
  HelpScreen -> switchTo HelpScreen
  where
    switchTo :: App es => ScreenName -> Eff es ()
    switchTo = modifyWithEnv . modifyView . switchScreen

    notBuilt :: App es => ScreenName -> Eff es ()
    notBuilt screen = notAvailable $ "The " <> screenText screen

-- | Show the screen after the focused one in a list, or the first one if the
-- focused one isn't in the list. Screens that aren't built yet are skipped,
-- so that a list can name them ahead.
cycleScreens :: App es => [ScreenName] -> Eff es ()
cycleScreens screens = do
  current <- getsS ((.screen) . focusedView)
  case filter (\s -> (screenInfo s).built) screens of
    [] -> showMessage "None of the screens is available yet"
    built@(first : _) -> runAction . Show $ case dropWhile (/= current) built of
      _ : following : _ -> following
      _ -> first

-- | Run a destructive action that the user confirmed.
runConfirmed :: App es => Confirmation -> Eff es ()
runConfirmed = \case
  ConfirmClear -> mutate clear
  ConfirmShuffle -> mutate $ shuffle Nothing

----------------------------------------
-- Saving as a stored playlist

-- | Ask MPD which stored playlists there are, with the songs of those to
-- save, before a save to the one of a name. An empty name saves nothing.
saveNamed :: App es => SaveSource -> T.Text -> Eff es ()
saveNamed source name
  | T.null name = pure ()
  | otherwise =
      request ((,) <$> listPlaylists <*> traverse listPlaylistInfo playlists) $
        \(existing, songs) -> SaveChecked name (withSongs songs) (name `elem` existing)
  where
    playlists :: [T.Text]
    playlists = case source of
      SaveQueue -> []
      SaveItems items -> [p | SavePlaylist p <- items]

    -- The songs of each stored playlist take its place.
    withSongs :: [[Song]] -> SaveSource
    withSongs songs = case source of
      SaveQueue -> SaveQueue
      SaveItems items -> SaveItems (go items songs)
      where
        go :: [SaveItem] -> [[Song]] -> [SaveItem]
        go (SavePlaylist _ : rest) (ss : more) = map songToSave ss <> go rest more
        go (item : rest) more = item : go rest more
        go [] _ = []

-- | Save to a new stored playlist, or ask whether to replace the one of the
-- name or to append to it.
saveChecked :: App es => T.Text -> SaveSource -> Bool -> Eff es ()
saveChecked name source exists
  | Just why <- nothingToSave source = showMessage why
  | exists =
      modifyS $
        openChoice
          ("The playlist " <> name <> " exists.")
          [ ChoiceOption 'r' "replace" (Just (SaveTo name source ReplacePlaylist))
          , ChoiceOption 'a' "append" (Just (SaveTo name source AppendToPlaylist))
          ]
  | otherwise = saveTo name source CreatePlaylist

saveTo :: App es => T.Text -> SaveSource -> SaveMode -> Eff es ()
saveTo name source mode = do
  mutate $ case source of
    SaveQueue -> save name mode
    SaveItems items ->
      when (mode == ReplacePlaylist) (playlistClear name)
        *> traverse_ addItem (savedItems items)
  showMessage $
    ( case mode of
        CreatePlaylist -> "Saved " <> describeSave source <> " as " <> name
        ReplacePlaylist -> "Replaced " <> name <> " with " <> describeSave source
        AppendToPlaylist -> "Added " <> describeSave source <> " to " <> name
    )
      <> case partsLeftOut source of
        0 -> ""
        1 -> ", without 1 part of a file"
        n -> ", without " <> T.pack (show n) <> " parts of files"
  where
    addItem :: SaveItem -> Command ()
    addItem = \case
      SaveSong uri -> playlistAdd name uri
      SaveDirectory path -> playlistAddDirectory name path
      -- 'saveNamed' put their songs in their places, and 'savedItems' left
      -- the parts out.
      SavePlaylist _ -> pure ()
      SavePart -> pure ()

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
  ToggleDisplay -> verb (Toggle ToggleDisplay)
  ToggleAlbumSeparators -> notAvailable "Album separators"
  ToggleFollowPlaying -> verb (Toggle ToggleFollowPlaying)
  ToggleBitrate -> toggleSetting "Bitrate" #showBitrate
  ToggleVisualization -> nextVisualization

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
  | T.null text = findAgain focusedRows f.direction
  | otherwise = case compilePattern text of
      Left _ -> do
        modifyWithEnv (restoreView f.origin)
        showError $ "Invalid pattern: " <> text
      Right _ -> do
        modifyS $ #findPattern ?~ text
        forM_ f.note (showMessage . capitalize)

-- | Move to the next or the previous match of the last pattern in rows of
-- the focused list.
findAgain :: App es => Eff es (Seq.Seq Folded) -> Direction -> Eff es ()
findAgain getRows direction = withFindPattern $ \text p -> do
  rows <- getRows
  c <- getsS ((.cursor) . focusedView)
  case search p direction c rows of
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

-- | The rows of the focused list as finds match them. A find opens only on
-- a screen with rows, and its prompt keeps the screen until it closes.
focusedRows :: App es => Eff es (Seq.Seq Folded)
focusedRows = do
  screen <- getsS ((.screen) . focusedView)
  fromMaybe (pure Seq.empty) (screenRows screen)

-- | The rows of a screen's list as finds match them, on the screens that
-- find.
screenRows :: App es => ScreenName -> Maybe (Eff es (Seq.Seq Folded))
screenRows = \case
  QueueScreen -> Just queueRows
  BrowserScreen -> Just $ getsS (.browser.rows)
  SearchEngineScreen -> Nothing
  MediaLibraryScreen -> Nothing
  PlaylistEditorScreen -> Nothing
  OutputsScreen -> Nothing
  VisualizerScreen -> Nothing
  LyricsScreen -> Nothing
  SongInfoScreen -> Nothing
  HelpScreen -> Nothing

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
        modifyS $ #seek ?~ SeekState target started (s.mirror.status >>= (.currentId)) token
        after seekCommitDelay (SeekCommit token)
      _ -> showError "The current song has no length"
  SeekToSecond n -> do
    dropSeek
    mutate . seekCur . SeekTo $ fromIntegral n
  SeekToPercent p -> do
    dropSeek
    s <- getS
    case currentDuration s of
      Just d -> mutate . seekCur . SeekTo $ d * fromIntegral p / 100
      Nothing -> showError "The current song has no length"

-- | Send the seek that the keys moved, if its song still plays or pauses.
commitSeek :: App es => Int -> Eff es ()
commitSeek token = whenCurrent (.seek) (.token) token $ \sk -> do
  s <- getS
  modifyS $ #seek .~ Nothing
  case s.mirror.status of
    Just st | st.state /= Stopped && st.currentId == sk.songId -> do
      modifyS $ #mirror %~ seekTo s.now sk.target
      mutate . seekCur $ SeekTo sk.target
    _ -> pure ()

-- | Drop the seek that the keys moved, e.g. for a command that moves the
-- song itself.
dropSeek :: App es => Eff es ()
dropSeek = modifyS $ #seek .~ Nothing

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
  let width = fst s.terminalSize
      nextSecond = fromIntegral (floor @Double @Int e + 1) - e
  delay <- case realToFrac <$> st.duration of
    -- A stream has no length, so it has no progress bar to move.
    Nothing -> Just nextSecond
    Just d
      | e >= d -> Nothing
      | width > 0 -> Just . min nextSecond $ nextCellAt width e d - e
      | otherwise -> Just nextSecond
  pure $ s.now + delay

updateWindowTitle :: App es => Eff es ()
updateWindowTitle = do
  env <- getAppEnv
  s <- getS
  forM_ env.config.windowTitle $ \fmt -> do
    let title = case (currentSong s.mirror, (.state) <$> s.mirror.status) of
          (Just song, Just st)
            | st /= Stopped -> renderPlain (unstyledContext env.config.lists) song fmt
          _ -> "reprise"
    when (s.windowTitle /= Just title) $ do
      modifyS $ #windowTitle ?~ title
      setTitle title
