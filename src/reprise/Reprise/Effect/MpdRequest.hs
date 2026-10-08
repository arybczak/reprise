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
  , answerPassword
  ) where

import Control.Monad
import Data.Text qualified as T
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
  AnswerPassword :: Maybe T.Text -> MpdRequest m ()

type instance DispatchOf MpdRequest = Dynamic

data PendingRequest
  = -- | A command with the event of its failure and the continuation of its
    -- reply.
    forall a. PendingRequest (Command a) (MpdError -> AppEvent) (a -> AppEvent)
  | -- | The answer to 'PasswordNeeded': a password, or 'Nothing' for a
    -- cancel.
    PasswordAnswer (Maybe T.Text)

-- | The request lines of a pending request, e.g. for a test. A password
-- answer has none.
pendingRequestLines :: PendingRequest -> [Request]
pendingRequestLines = \case
  PendingRequest cmd _ _ -> commandRequests cmd
  PasswordAnswer _ -> []

-- | Collect the requests, in the order the action made them.
collectMpdRequests :: Eff (MpdRequest : es) a -> Eff es (a, [PendingRequest])
collectMpdRequests = reinterpret_ runOutput $ \case
  RequestCommand cmd onFailure k -> output $ PendingRequest cmd onFailure k
  AnswerPassword p -> output $ PasswordAnswer p

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

-- | Answer 'PasswordNeeded', which the requests wait for.
answerPassword :: MpdRequest :> es => Maybe T.Text -> Eff es ()
answerPassword = send . AnswerPassword
