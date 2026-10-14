-- | What only the UI loop can do: halt, set the terminal's title, send an
-- event later, skip a redraw, start the samples of the visualizer, ask for
-- lyrics, edit a file, save the history of the prompts and list a local
-- directory.
module Reprise.Effect.UiRequest
  ( -- * Effect
    UiRequest (..)
  , UiCommand (..)

    -- ** Handlers
  , collectUiRequests

    -- ** Operations
  , halt
  , setTitle
  , after
  , keepScreen
  , visualize
  , fetchLyrics
  , fetchLyricsInBackground
  , editFile
  , saveToHistory
  , listLocal
  ) where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Output.Static.Local.List

import Reprise.Config
import Reprise.Event
import Reprise.LocalFiles
import Reprise.Lyrics
import Reprise.Mpd.Protocol.Types

data UiRequest :: Effect where
  UiRequest :: UiCommand -> UiRequest m ()

type instance DispatchOf UiRequest = Dynamic

data UiCommand
  = Halt
  | SetTitle T.Text
  | -- | Send the event after a number of seconds.
    After Double AppEvent
  | -- | The event changed nothing on the screen, so it isn't redrawn.
    KeepScreen
  | -- | Read the samples of the visualizer for a visualization, or stop.
    Visualize (Maybe Visualization)
  | -- | Load the lyrics of a request, with its token. A newer
    -- request replaces one that the worker didn't take yet.
    FetchLyrics Int LyricsRequest
  | -- | Fetch and store the lyrics of a song without showing them, after a
    -- request of the screen.
    FetchLyricsInBackground Song
  | -- | Edit a file with an editor command, which has the terminal until it
    -- exits.
    Edit T.Text FilePath
  | -- | Add a line of a prompt to the history file.
    SaveToHistory T.Text
  | -- | List a local directory for the browser's listing with the token,
    -- with the tags of the files whose suffixes MPD decodes.
    ListLocal Int Suffixes FilePath
  deriving stock (Eq, Show)

-- | Collect the requests, in the order the action made them.
collectUiRequests :: Eff (UiRequest : es) a -> Eff es (a, [UiCommand])
collectUiRequests = reinterpret_ runOutput $ \case
  UiRequest c -> output c

halt :: UiRequest :> es => Eff es ()
halt = send $ UiRequest Halt

setTitle :: UiRequest :> es => T.Text -> Eff es ()
setTitle = send . UiRequest . SetTitle

-- | Send an event after a number of seconds.
after :: UiRequest :> es => Double -> AppEvent -> Eff es ()
after delay = send . UiRequest . After delay

-- | Skip the redraw after an event that changed nothing on the screen.
keepScreen :: UiRequest :> es => Eff es ()
keepScreen = send $ UiRequest KeepScreen

-- | Read the samples of the visualizer for a visualization, or stop.
visualize :: UiRequest :> es => Maybe Visualization -> Eff es ()
visualize = send . UiRequest . Visualize

-- | Load the lyrics of a request, with its token.
fetchLyrics :: UiRequest :> es => Int -> LyricsRequest -> Eff es ()
fetchLyrics token = send . UiRequest . FetchLyrics token

-- | Fetch and store the lyrics of a song without showing them.
fetchLyricsInBackground :: UiRequest :> es => Song -> Eff es ()
fetchLyricsInBackground = send . UiRequest . FetchLyricsInBackground

-- | Edit a file with an editor command, which has the terminal until it
-- exits.
editFile :: UiRequest :> es => T.Text -> FilePath -> Eff es ()
editFile command = send . UiRequest . Edit command

-- | Add a line of a prompt to the history file.
saveToHistory :: UiRequest :> es => T.Text -> Eff es ()
saveToHistory = send . UiRequest . SaveToHistory

-- | List a local directory for the browser's listing with the token.
listLocal :: UiRequest :> es => Int -> Suffixes -> FilePath -> Eff es ()
listLocal token suffixes = send . UiRequest . ListLocal token suffixes