-- | What only the UI loop can do: halt, set the terminal's title, send an
-- event later, and skip a redraw.
module Reprise.Effect.UiRequest
  ( -- * Effect
    UiRequest (..)
  , UiCommand (..)

    -- ** Handlers
  , collectUiRequests

    -- ** Operations
  , halt
  , setTitle
  , after
  , keepScreen
  ) where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Output.Static.Local.List

import Reprise.Event

data UiRequest :: Effect where
  UiRequest :: UiCommand -> UiRequest m ()

type instance DispatchOf UiRequest = Dynamic

data UiCommand
  = Halt
  | SetTitle T.Text
  | -- | Send the event after a number of seconds.
    After Double AppEvent
  | -- | The event changed nothing on the screen, so it isn't redrawn.
    KeepScreen
  deriving stock (Eq, Show)

-- | Collect the requests, in the order the action made them.
collectUiRequests :: Eff (UiRequest : es) a -> Eff es (a, [UiCommand])
collectUiRequests = reinterpret_ runOutput $ \case
  UiRequest c -> output c

halt :: UiRequest :> es => Eff es ()
halt = send $ UiRequest Halt

setTitle :: UiRequest :> es => T.Text -> Eff es ()
setTitle = send . UiRequest . SetTitle

-- | Send an event after a number of seconds.
after :: UiRequest :> es => Double -> AppEvent -> Eff es ()
after delay = send . UiRequest . After delay

-- | Skip the redraw after an event that changed nothing on the screen.
keepScreen :: UiRequest :> es => Eff es ()
keepScreen = send $ UiRequest KeepScreen
