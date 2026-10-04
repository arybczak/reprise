-- | What reprise shows and logs when a call to MPD fails.
module Reprise.Mpd.Error
  ( describeMpdError
  ) where

import Data.Text qualified as T

import Reprise.Mpd.Protocol.Types

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
