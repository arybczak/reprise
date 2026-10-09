-- | The events of the application. Every continuation is an event: of an MPD
-- reply, of a timer and of a prompt. Events are data, so that a test can
-- compare them and run them.
module Reprise.Event
  ( AppEvent (..)
  , Confirmation (..)
  , FrameStats (..)
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS

import Reprise.Action
import Reprise.Keys
import Reprise.Lyrics
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Save

data AppEvent
  = -- | The loop started, after the terminal's first size.
    Started
  | -- | A key that the user pressed.
    KeyPressed KeySpec
  | -- | A step of the mouse wheel, up or down.
    MouseWheel MoveTarget
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
  | -- | MPD refused a command without a password, or refused the password.
    -- The requests wait for 'Reprise.Effect.MpdRequest.answerPassword'.
    PasswordNeeded MpdError
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
  | -- | The magnitudes of the spectra of the left channel and of the right
    -- one for the visualizer's next frame.
    VisualizerSpectrum (VS.Vector Double) (VS.Vector Double)
  | -- | The samples of the wave of the visualizer's next frame, as MPD's
    -- fifo output wrote them.
    VisualizerWave BS.ByteString
  | -- | The visualizer's data source can't be read, with the reason.
    VisualizerFailed T.Text
  | -- | What happened to the visualizer's frames in the last second, with
    -- @visualizer.debug@.
    VisualizerStats FrameStats
  | -- | The lyrics of the request with the token aren't stored, so a fetcher
    -- with the name is asked for them.
    LyricsFetching Int T.Text
  | -- | The lyrics of the request with the token.
    LyricsLoaded Int LyricsResult
  | -- | The comments of the file of the song info screen's request with the
    -- token.
    SongCommentsFetched Int [(T.Text, T.Text)]
  | OutputsFetched [Output]
  | -- | The editor of a file exited, with why it failed.
    Edited FilePath (Maybe T.Text)
  | -- | Edit the file of lyrics, which the user chose as there were none.
    EditLyricsFile FilePath
  | -- | The user confirmed a destructive action.
    Confirmed Confirmation
  | -- | MPD's reply before a save to the stored playlist of the name: what
    -- to save, with the songs of its stored playlists, and whether a stored
    -- playlist of the name exists.
    SaveChecked T.Text SaveSource Bool
  | -- | Save to the stored playlist of the name.
    SaveTo T.Text SaveSource SaveMode
  deriving stock (Eq, Show)

-- | A destructive action that asks before it runs.
data Confirmation
  = -- | Clear the queue.
    ConfirmClear
  | -- | Shuffle the whole queue.
    ConfirmShuffle
  deriving stock (Eq, Show)

-- | What happened to the visualizer worker's frames in a second.
data FrameStats = FrameStats
  { frames :: Int
  -- ^ The frames that the worker woke up for.
  , late :: Int
  -- ^ The frames that it skipped, as it woke up after their time.
  , empty :: Int
  -- ^ The frames without samples while MPD writes them, as the buffer ran
  -- out.
  , dropped :: Int
  -- ^ The frames that the UI had no room for.
  , bytes :: Int
  -- ^ What it read from the fifo, 176400 bytes a second at 44100:16:2.
  }
  deriving stock (Eq, Show)
