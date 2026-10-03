-- | Connections to MPD.
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
withConnection
  :: Settings -> (Connection -> IO (Either MpdError a)) -> IO (Either MpdError a)
withConnection settings action =
  bracket (connect settings) (either (const $ pure ()) close) $ \case
    Left err -> pure $ Left err
    Right conn -> action conn

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
run :: Connection -> Command a -> IO (Either MpdError a)
run conn = withTimeout conn.timeout . exchangeCommand conn
