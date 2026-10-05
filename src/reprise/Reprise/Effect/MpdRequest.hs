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
  , requestOr
  , mutate
  ) where

import Control.Monad
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Output.Static.Local.List

import Reprise.Event
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types

data MpdRequest :: Effect where
  RequestCommand
    :: Command a -> (MpdError -> AppEvent) -> (a -> AppEvent) -> MpdRequest m ()

type instance DispatchOf MpdRequest = Dynamic

-- | A command with the event of its failure and the continuation of its
-- reply.
data PendingRequest
  = forall a. PendingRequest (Command a) (MpdError -> AppEvent) (a -> AppEvent)

-- | The request lines of a pending request, e.g. for a test.
pendingRequestLines :: PendingRequest -> [Request]
pendingRequestLines (PendingRequest cmd _ _) = commandRequests cmd

-- | Collect the requests, in the order the action made them.
collectMpdRequests :: Eff (MpdRequest : es) a -> Eff es (a, [PendingRequest])
collectMpdRequests = reinterpret_ runOutput $ \case
  RequestCommand cmd onFailure k -> output $ PendingRequest cmd onFailure k

-- | Request a command, with a continuation for its reply. A failure is
-- 'MpdFailed', which shows the error.
request :: MpdRequest :> es => Command a -> (a -> AppEvent) -> Eff es ()
request cmd = requestOr cmd (MpdFailed (commandRequests cmd))

-- | Request a command, with an event for its failure, for a requester that
-- can recover from it.
requestOr
  :: MpdRequest :> es => Command a -> (MpdError -> AppEvent) -> (a -> AppEvent) -> Eff es ()
requestOr cmd onFailure k = send $ RequestCommand cmd onFailure k

-- | Request a command that changes MPD's state. Its effect comes back
-- through idle. A command without requests, e.g. a move of songs that are
-- at the top already, isn't sent.
mutate :: MpdRequest :> es => Command () -> Eff es ()
mutate cmd = unless (null (commandRequests cmd)) $ request cmd (const MpdDone)
