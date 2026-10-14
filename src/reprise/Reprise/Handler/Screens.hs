-- | What each screen does for the handlers, from the screen's module.
module Reprise.Handler.Screens
  ( screenOps
  ) where

import Effectful
import Effectful.Input.Static
import Effectful.State.Static.Local

import Reprise.Action
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Handler.Core
import Reprise.Screen.Browser
import Reprise.Screen.Help
import Reprise.Screen.Lyrics
import Reprise.Screen.Outputs
import Reprise.Screen.Queue
import Reprise.Screen.SongInfo
import Reprise.Screen.Visualizer
import Reprise.State

screenOps
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => ScreenName
  -> ScreenOps es
screenOps = \case
  QueueScreen -> queueOps
  BrowserScreen -> browserOps
  SearchEngineScreen -> notBuilt SearchEngineScreen
  MediaLibraryScreen -> notBuilt MediaLibraryScreen
  PlaylistEditorScreen -> notBuilt PlaylistEditorScreen
  OutputsScreen -> outputsOps
  VisualizerScreen -> visualizerOps
  LyricsScreen -> lyricsOps
  SongInfoScreen -> songInfoOps
  HelpScreen -> helpOps
  where
    notBuilt :: (State AppState :> es, UiRequest :> es) => ScreenName -> ScreenOps es
    notBuilt screen =
      ScreenOps
        { verb = const Nothing
        , rows = Nothing
        , shown = notAvailable $ "The " <> screenText screen
        }
