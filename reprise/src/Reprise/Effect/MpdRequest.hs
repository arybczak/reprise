-- | Requests of MPD commands. An action never waits for a reply: a request
-- carries a continuation that turns the reply into an event.
module Reprise.Effect.MpdRequest
  ( -- * Effect
    MpdRequest (..)
  , PendingRequest (..)
  , pendingRequestLines

    -- ** Handlers
  , collectMpdRequests

    -- ** Operations
  , request
  , mutate
  ) where

import Control.Monad
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Output.Static.Local.List
import MPD.Command
import MPD.Protocol.Request

import Reprise.Event

data MpdRequest :: Effect where
  RequestCommand :: Command a -> (a -> AppEvent) -> MpdRequest m ()

type instance DispatchOf MpdRequest = Dynamic

-- | A command with its continuation. If the command fails, the event is
-- 'MpdFailed' instead.
data PendingRequest = forall a. PendingRequest (Command a) (a -> AppEvent)

-- | The request lines of a pending request, e.g. for a test.
pendingRequestLines :: PendingRequest -> [Request]
pendingRequestLines (PendingRequest cmd _) = commandRequests cmd

-- | Collect the requests, in the order the action made them.
collectMpdRequests :: Eff (MpdRequest : es) a -> Eff es (a, [PendingRequest])
collectMpdRequests = reinterpret_ runOutput $ \case
  RequestCommand cmd k -> output $ PendingRequest cmd k

-- | Request a command, with a continuation for its reply.
request :: MpdRequest :> es => Command a -> (a -> AppEvent) -> Eff es ()
request cmd k = send $ RequestCommand cmd k

-- | Request a command that changes MPD's state. Its effect comes back
-- through idle. A command without requests, e.g. a move of songs that are
-- at the top already, isn't sent.
mutate :: MpdRequest :> es => Command () -> Eff es ()
mutate cmd = unless (null (commandRequests cmd)) $ request cmd (const MpdDone)
