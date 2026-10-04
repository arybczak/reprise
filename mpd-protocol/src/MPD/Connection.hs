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
withConnection :: Settings -> (Connection -> IO a) -> IO a
withConnection settings = bracket (connect settings) close

-- | The protocol version that MPD sent when the connection opened.
serverVersion :: Connection -> Version
serverVersion conn = conn.version

-- | Run a command, or a command list, in one round trip.
--
-- After a 'ConnectionError', the connection is unusable: close it and
-- connect again.
run :: Connection -> Command a -> IO a
run conn = withTimeout conn.timeout . exchangeCommand conn
