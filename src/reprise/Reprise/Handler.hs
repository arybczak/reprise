-- | The handlers of events and actions. They are 'Eff' code over the state,
-- and they only request MPD commands and UI operations, so the tests run
-- them with pure handlers. This module passes each event on, and keeps the
-- input that comes while an edit of the queue waits for its reply.
module Reprise.Handler
  ( runEvent
  ) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Input.Static
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Exception
import Reprise.Handler.Action
import Reprise.Handler.Core
import Reprise.Handler.Input
import Reprise.Handler.Mirror
import Reprise.Handler.Redraw
import Reprise.Handler.Save
import Reprise.Handler.Seek
import Reprise.Keymap
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Browser
import Reprise.Screen.Lyrics
import Reprise.Screen.Outputs
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
        holdOrHandle event
        afterEvent
  pure (s', requests, commands)

-- | Handle an event, or keep it for later if it is input that comes while
-- an edit of the queue waits for its reply. A prompt takes keys meanwhile.
-- As the keys wait, an open one is one that an event opened: the prompt
-- for the password, which the edit may wait for, or e.g. a choice that
-- MPD's reply opened, which the prompt for the password may wait for.
holdOrHandle
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => AppEvent
  -> Eff es ()
holdOrHandle event = do
  env <- getAppEnv
  s <- getS
  let toHold = case event of
        -- The reply may never come, e.g. on a connection that hangs.
        KeyPressed k | k /= alwaysQuit -> Just (HeldEvent event)
        MouseWheel {} -> Just (HeldEvent event)
        MouseClick button col row
          | (focusedView s).screen == QueueScreen
          , Just i <- listItemAt env s col row
          , Just songId <- (.songId) =<< Seq.lookup i s.mirror.queue ->
              Just (HeldQueueClick button songId)
          | otherwise -> Just (HeldEvent event)
        _ -> Nothing
  case (s.pendingEdit, toHold) of
    (Just _, Just held) | isNothing s.prompt -> hold held
    _ -> handleEvent event

-- | Keep input until the edit of the queue that waits gets its reply.
hold :: (State AppState :> es, UiRequest :> es) => HeldInput -> Eff es ()
hold held = do
  modifyS $ #pendingEdit % _Just % #heldInput %~ (Seq.|> held)
  keepScreen

-- | Handle the input that waited for an edit of the queue, whose changes
-- the mirror has now. If it edits the queue again, the rest of it waits
-- again.
releaseInput
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Eff es ()
releaseInput = do
  held <- endEdit
  -- The held input runs within the event of the reply, and a 'KeepScreen'
  -- anywhere in an event skips its redraw, which the reply needs. A key
  -- that changes nothing, e.g. a click outside a list, would send one.
  let redrawn :: UiRequest :> es => Eff es a -> Eff es a
      redrawn = interpose_ $ \case
        UiRequest KeepScreen -> pure ()
        UiRequest c -> send (UiRequest c)
  redrawn $ traverse_ release held
  where
    release
      :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
      => HeldInput
      -> Eff es ()
    release = \case
      HeldEvent event -> holdOrHandle event
      click@(HeldQueueClick button songId) -> do
        s <- getS
        case s.pendingEdit of
          Just _ | isNothing s.prompt -> hold click
          _ -> clickSong button songId

handleEvent
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => AppEvent
  -> Eff es ()
handleEvent = \case
  Started -> showScreen . (.config.startupScreen) =<< getAppEnv
  -- A choice's option is an event.
  KeyPressed k -> handleKey k >>= traverse_ handleEvent
  MouseWheel t col row -> handleWheel t col row
  MouseClick button col row -> handleClick button col row
  Resized w h -> do
    modifyS $ \s ->
      s
        & #terminalSize .~ (w, h)
        & layoutViews
    modifyWithEnv (modifyView id)
  -- The reply to an edit may be lost, e.g. with a restarted worker, and
  -- the input that waited for it was meant for the queue before the
  -- connection broke. MPD gives the ids of the songs anew when it
  -- restarts, so the ids of the selection could be of other songs now.
  MpdConnected v -> do
    modifyS $ \s ->
      s
        & #connection .~ Connected v
        & #pendingEdit .~ Nothing
        & #queueState % #selection .~ noSelection
        -- MPD may have restarted with other decoders.
        & #browser % #suffixes .~ Nothing
    fetchQueue
    relistBrowser
    refreshOutputs
  -- The player's status would be stale, but the queue stays to look at
  -- until the connection is back.
  MpdDisconnected reason ->
    modifyS $ \s ->
      s
        & #connection .~ Disconnected reason
        & #mirror %~ forgetStatus
        & #seek .~ Nothing
  MpdChanged subsystems -> do
    editing <- getsS (isJust . (.pendingEdit))
    let queueChanged = PlaylistSubsystem `elem` subsystems
        statusChanged = any (`elem` statusSubsystems) subsystems
    -- The changes of the queue come with the status.
    if
      | queueChanged && editing -> do
          modifyS $ #pendingEdit % _Just % #queueChanged .~ True
          when statusChanged $ request status StatusFetched
      | queueChanged -> fetchQueueChanges
      | statusChanged -> request status StatusFetched
      | otherwise -> pure ()
    browserChanged subsystems
    when (OutputSubsystem `elem` subsystems) refreshOutputs
  QueueFetched (st, songs) -> do
    now <- getsS (.now)
    updateMirror $ setQueue now st songs
  QueueChangesFetched changes -> applyChanges changes
  QueueFetchFailed err -> queueFetchFailed err
  QueueEditStatus edits st -> do
    s <- getS
    case applyQueueEdits s.now st edits s.mirror of
      Just m -> do
        updateMirror (const m)
        releaseInput
      Nothing -> forM_ s.mirror.queueVersion $ \v ->
        requestOr ((,) <$> status <*> plChanges v) QueueEditFailed QueueEdited
  QueueEdited changes -> do
    applyChanges changes
    releaseInput
  -- The input was meant for the queue after the edit.
  QueueEditFailed err -> do
    showError $ exceptionText err
    void endEdit
  StatusFetched st -> do
    now <- getsS (.now)
    updateMirror $ setStatus now st
  BrowserListed token entries -> browserListed token entries
  BrowserFailed token err -> browserFailed token err
  DecodersFetched token ds -> decodersFetched token ds
  LocalListed token result -> localListed token result
  LocalWalked token files -> localWalked token files
  LocalAdded token -> localAdded token
  LocalAddFailed token err -> localAddFailed token err
  ReplayGainFetched mode -> do
    let nextMode = cycleNext mode
    mutate $ setReplayGainMode nextMode
    showMessage $ "Replay gain: " <> replayGainModeName nextMode
  MpdDone -> keepScreen
  QuitReady -> halt
  MpdFailed _ err -> showError $ exceptionText err
  -- MPD's requests wait for the answer, so the prompt replaces any other.
  -- A prompt that is open, e.g. the choice to replace a playlist, keeps its
  -- turn, as its request waits for the password with the others.
  PasswordNeeded err -> do
    open <- getsS (isJust . (.prompt))
    modifyS $ if open then #passwordWaits ?~ err else askPassword err
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
  VisualizerFailed reason -> visualizerFailed reason
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
afterEvent :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es) => Eff es ()
afterEvent = do
  -- The prompt for the password that waited for another one.
  s <- getS
  case (s.prompt, s.passwordWaits) of
    (Nothing, Just err) ->
      modifyS $ \st ->
        st
          & askPassword err
          & #passwordWaits .~ Nothing
    _ -> pure ()
  updateLyrics
  restartTitle
  scheduleTick
  updateWindowTitle
  updateVisualizer
