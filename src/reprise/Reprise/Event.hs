-- | The events of the application. Every continuation is an event: of an MPD
-- reply, of a timer and of a prompt. Events are data, so that a test can
-- compare them and run them.
module Reprise.Event
  ( AppEvent (..)
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS

import Reprise.Action
import Reprise.Keys
import Reprise.Lyrics
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types

data AppEvent
  = -- | A key that the user pressed.
    KeyPressed KeySpec
  | -- | The terminal's new width and height.
    Resized Int Int
  | -- | The idle connection is up.
    MpdConnected Version
  | -- | The idle connection is down, with the reason.
    MpdDisconnected T.Text
  | MpdChanged [Subsystem]
  | -- | The status and the whole queue.
    QueueFetched (Status, [Song])
  | -- | The status and the songs of the queue that changed since its version
    -- in the mirror.
    QueueChangesFetched (Status, [Song])
  | StatusFetched Status
  | -- | The entries of the browser's listing with the token.
    BrowserListed Int [Entry]
  | -- | The browser's listing with the token failed.
    BrowserFailed Int MpdError
  | ReplayGainFetched ReplayGainMode
  | -- | The reply to a command that changes MPD's state. The new state comes
    -- through idle.
    MpdDone
  | -- | A requested command failed, instead of its continuation.
    MpdFailed [Request] MpdError
  | -- | A timer for the next redraw of the elapsed time, with its token.
    Tick Int
  | -- | The pause after the last seek key, with its token.
    SeekCommit Int
  | MessageExpired Int
  | -- | The timer that hides the queue's cursor a while after the last key.
    HideCursor
  | -- | The samples of the visualizer's next frame, as MPD's fifo output
    -- wrote them. Empty while nothing plays.
    VisualizerSamples BS.ByteString
  | -- | The magnitudes of each channel's spectrum for the visualizer's next
    -- frame.
    VisualizerSpectrum [VS.Vector Double]
  | -- | The visualizer's data source can't be read, with the reason.
    VisualizerFailed T.Text
  | -- | The lyrics of the request with the token aren't stored, so a fetcher
    -- with the name is asked for them.
    LyricsFetching Int T.Text
  | -- | The lyrics of the request with the token.
    LyricsLoaded Int LyricsResult
  | -- | The comments of the file of the song info screen's request with the
    -- token.
    SongCommentsFetched Int [(T.Text, T.Text)]
  | -- | The editor of a file exited, with why it failed.
    Edited FilePath (Maybe T.Text)
  | -- | The user confirmed a destructive action.
    Confirmed Action
  deriving stock (Eq, Show)
