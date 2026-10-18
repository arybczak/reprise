-- | Running actions. A verb, e.g. activate, goes on to the focused screen,
-- which does it its own way.
module Reprise.Handler.Action
  ( runAction
  , screenVerb
  , showScreen
  , quit
  , runConfirmed
  ) where

import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Text qualified as T
import Effectful
import Effectful.Input.Static
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Action
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Find
import Reprise.Handler.Core
import Reprise.Handler.Find
import Reprise.Handler.Screens
import Reprise.Handler.Seek
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Browser
import Reprise.Screen.Lyrics
import Reprise.Screen.Visualizer
import Reprise.State

-- | Run an action. A verb, e.g. activate, goes to the focused screen, which
-- does it its own way.
runAction
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Action
  -> Eff es ()
runAction action = case action of
  Move _ -> verb action
  JumpToPlaying -> verb action
  Back -> goBack
  JumpToBrowser -> showSongScreen BrowserScreen locateSong
  Activate -> verb action
  Parent -> verb action
  Save -> verb action
  EditLyrics -> verb action
  Rename -> verb action
  RenameByFormat -> verb action
  RefetchLyrics -> verb action
  NextSortMode -> verb action
  ChangeBrowseMode -> verb action
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
  Ask name -> modifyS $ openLine (askQuestion name) (LineEdit "" "") (ForArgument name)
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
  Quit -> quit
  Clear -> verb action
  Shuffle -> verb action
  Update UpdateCurrent -> verb action
  Update UpdateAll -> updateDatabase Nothing

-- | Run a verb the way the focused screen implements it. A screen that
-- doesn't implement it says so, instead of acting on another screen.
verb
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Action
  -> Eff es ()
verb action = do
  screen <- getsS ((.screen) . focusedView)
  verbOr (screenHasNo screen (renderAction action)) action

-- | Run a verb the way the focused screen implements it, else the other
-- way.
verbOr
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Eff es ()
  -> Action
  -> Eff es ()
verbOr otherwise' action = do
  screen <- getsS ((.screen) . focusedView)
  fromMaybe otherwise' (screenVerb screen action)

-- | How a screen does a verb, if it does it. A screen finds if it has rows
-- to find in.
screenVerb
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => ScreenName
  -> Action
  -> Maybe (Eff es ())
screenVerb screen = \case
  Find t ->
    (screenOps screen).rows <&> \rows -> do
      -- A find moves the lyrics, so it stops following the song as a
      -- scroll does, before it notes where it starts.
      when (screen == LyricsScreen) stopFollowing
      case t of
        FindForward -> modifyS (startFind Forward)
        FindBackward -> modifyS (startFind Backward)
        FindNext -> findAgain rows Forward
        FindPrevious -> findAgain rows Backward
  action -> (screenOps screen).verb action

-- | Show a screen in the focused view, with what the screen does when it
-- shows, e.g. load the lyrics of the song under the cursor.
showScreen
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => ScreenName
  -> Eff es ()
showScreen screen = (screenOps screen).shown

-- | Show the screen after the focused one in a list, or the first one if the
-- focused one isn't in the list. Screens that aren't built yet are skipped,
-- so that a list can name them ahead.
cycleScreens
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => [ScreenName]
  -> Eff es ()
cycleScreens screens = do
  current <- getsS ((.screen) . focusedView)
  case filter (\s -> (screenInfo s).built) screens of
    [] -> showMessage "None of the screens is available yet"
    built@(first : _) -> runAction . Show $ case dropWhile (/= current) built of
      _ : following : _ -> following
      _ -> first

-- | Run a destructive action that the user confirmed.
runConfirmed
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es) => Confirmation -> Eff es ()
runConfirmed = \case
  ConfirmClear -> editQueue [ClearQueue]
  ConfirmShuffle -> editQueue [ShuffleRange Nothing]
  ConfirmRename location renames -> renameIn location renames

-- | Quit once the requests before the quit ran, so that the command of a
-- key right before it isn't lost. A command without requests marks that
-- point in the queue of requests. While MPD waits for the password, the
-- requests wait for its answer, so reprise quits at once, as it does for a
-- second quit.
quit :: (State AppState :> es, MpdRequest :> es, UiRequest :> es) => Eff es ()
quit = do
  s <- getS
  let askingPassword = case s.prompt of
        Just (Prompt _ (Line _ ForPassword _)) -> True
        _ -> False
  if s.quitting || askingPassword || isJust s.passwordWaits
    then halt
    else do
      modifyS $ #quitting .~ True
      showMessage "Quitting (do it again to force)…"
      requestOr (pure ()) (const QuitReady) (const QuitReady)

withStatus
  :: (State AppState :> es, UiRequest :> es)
  => (Status -> Eff es ())
  -> Eff es ()
withStatus k =
  getsS (.mirror.status) >>= \case
    Just st -> k st
    Nothing -> showError "Not connected to MPD"

toggle
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => ToggleTarget
  -> Eff es ()
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
