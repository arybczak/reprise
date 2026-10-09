-- | The monotonic clock of a worker, and sleeping until a time of it. The
-- tests replace it with a clock that only a sleep moves.
module Reprise.Effect.Clock
  ( -- * Effect
    Clock (..)

    -- ** Handlers
  , runClock

    -- ** Operations
  , monotonicTime
  , sleepUntil
  ) where

import Control.Concurrent
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Clock

data Clock :: Effect where
  MonotonicTime :: Clock m Double
  SleepUntil :: Double -> Clock m ()

type instance DispatchOf Clock = Dynamic

-- | Run the effect with the system's monotonic clock.
runClock :: IOE :> es => Eff (Clock : es) a -> Eff es a
runClock = interpret_ $ \case
  MonotonicTime -> liftIO getMonotonicTime
  SleepUntil t -> liftIO $ do
    now <- getMonotonicTime
    threadDelay (ceiling ((t - now) * 1000000))

-- | Seconds since a point in the past.
monotonicTime :: Clock :> es => Eff es Double
monotonicTime = send MonotonicTime

-- | Sleep until a monotonic time. A time that passed doesn't wait.
sleepUntil :: Clock :> es => Double -> Eff es ()
sleepUntil = send . SleepUntil
