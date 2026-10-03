-- | The events of the application. Every continuation is an event: of an MPD
-- reply, of a timer and of a prompt. Events are data, so that a test can
-- compare them and run them.
module Reprise.Event
  ( AppEvent (..)
  ) where

import Data.Text qualified as T
import MPD.Types

import Reprise.Action
import Reprise.Keys

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
    QueueFetched (Either MpdError (Status, [Song]))
  | -- | The status and the songs of the queue that changed since its version
    -- in the mirror.
    QueueChangesFetched (Either MpdError (Status, [Song]))
  | StatusFetched (Either MpdError Status)
  | ReplayGainFetched (Either MpdError ReplayGainMode)
  | -- | The reply to a command that changes MPD's state. The new state comes
    -- through idle, so only an error matters.
    MpdDone (Either MpdError ())
  | -- | A timer for the next redraw of the elapsed time, with its token.
    Tick Int
  | -- | The pause after the last seek key, with its token.
    SeekCommit Int
  | MessageExpired Int
  | -- | A timer that redraws the screen, e.g. to hide the queue's cursor.
    Redraw
  | -- | The user confirmed a destructive action.
    Confirmed Action
  deriving stock (Eq, Show)
