-- | Connections to MPD. The operations throw 'MpdError'.
module MPD.Connection
  ( -- * Settings
    Settings (..)
  , Address (..)

    -- * Connections
  , Connection
  , connect
  , close
  , withConnection
  , serverVersion
  , minimumVersion

    -- * Commands
  , run
  ) where

import Control.Exception

import MPD.Command
import MPD.Internal.Connection
import MPD.Types

-- | Run an action with a new connection, and close it afterwards.
--
-- @since 0.1.0.0
withConnection :: Settings -> (Connection -> IO a) -> IO a
withConnection settings = bracket (connect settings) close

-- | The protocol version that MPD sent when the connection opened.
--
-- @since 0.1.0.0
serverVersion :: Connection -> Version
serverVersion conn = conn.version

-- | Run a command, or a command list, in one round trip.
--
-- After a 'ConnectionError', the connection is unusable: close it and
-- connect again.
--
-- @since 0.1.0.0
run :: Connection -> Command a -> IO a
run conn = withTimeout conn.timeout . exchangeCommand conn
