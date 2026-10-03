-- | Waiting for changes with @idle@.
module MPD.Idle
  ( idle
  , noidle
  ) where

import MPD.Internal.Connection
import MPD.Protocol.Request
import MPD.Protocol.Response
import MPD.Types

-- | Wait until one of the subsystems changes, or any subsystem for an empty
-- list, and return the subsystems that changed. There is no timeout.
--
-- @since 0.1.0.0
idle :: Connection -> [Subsystem] -> IO (Either MpdError [Subsystem])
idle conn subsystems = do
  r <- exchange conn . renderRequest $ Request "idle" (map subsystemName subsystems)
  pure $
    r >>= \case
      [fields] -> either (Left . ProtocolError . ("idle: " <>)) Right $ parseSubsystems fields
      _ -> Left $ ProtocolError "idle: the reply has more than one part"

-- | Make a running 'idle' return at once. Unlike any other operation, it is
-- safe to call from another thread while 'idle' waits.
--
-- @since 0.1.0.0
noidle :: Connection -> IO (Either MpdError ())
noidle conn = sendRaw conn . renderRequest $ Request "noidle" []
