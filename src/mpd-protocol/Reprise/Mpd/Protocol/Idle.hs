-- | Waiting for changes with @idle@. The operations throw 'MpdError'.
module Reprise.Mpd.Protocol.Idle
  ( idle
  , noidle
  ) where

import Control.Exception

import Reprise.Mpd.Protocol.Internal.Connection
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Response
import Reprise.Mpd.Protocol.Types

-- | Wait until one of the subsystems changes, or any subsystem for an empty
-- list, and return the subsystems that changed. There is no timeout.
idle :: Connection -> [Subsystem] -> IO [Subsystem]
idle conn subsystems =
  exchange Nothing conn (renderRequest $ Request "idle" (map subsystemName subsystems)) >>= \case
    [fields] -> either (throwIO . ProtocolError . ("idle: " <>)) pure $ parseSubsystems fields
    _ -> throwIO $ ProtocolError "idle: the reply has more than one part"

-- | Make a running 'idle' return at once. Unlike any other operation, it is
-- safe to call from another thread while 'idle' waits.
noidle :: Connection -> IO ()
noidle conn = sendRaw conn . renderRequest $ Request "noidle" []
