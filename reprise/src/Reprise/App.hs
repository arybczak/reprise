-- | The brick application: a thin adapter that runs each event through the
-- pure handlers and then performs the requests they collected.
module Reprise.App
  ( Env (..)
  , app
  , eventChannelSize
  ) where

import Brick qualified as B
import Brick.BChan qualified as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Text qualified as T
import GHC.Clock
import Graphics.Vty qualified as V

import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Handler
import Reprise.Keys
import Reprise.State
import Reprise.UI.Layout

data Env = Env
  { requests :: TQueue PendingRequest
  , events :: B.BChan AppEvent
  }

-- | The size of the channel of events from the workers and the timers. A
-- full channel blocks them until the UI catches up. brick's own channel of
-- terminal events has this size.
eventChannelSize :: Int
eventChannelSize = 20

app :: Env -> B.App AppState AppEvent ()
app env =
  B.App
    { B.appDraw = \s ->
        [ maybe id (\(x, y) -> B.showCursor () (B.Location (x, y))) (promptCursor s) $
            B.raw (renderScreen s)
        ]
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = \case
        B.AppEvent e -> dispatch env e
        B.VtyEvent (V.EvKey k mods) -> forM_ (fromVtyKey k mods) (dispatch env . KeyPressed)
        B.VtyEvent (V.EvResize w h) -> dispatch env (Resized w h)
        _ -> pure ()
    , B.appStartEvent = do
        vty <- B.getVtyHandle
        (w, h) <- liftIO . V.displayBounds $ V.outputIface vty
        dispatch env (Resized w h)
    , B.appAttrMap = const $ B.attrMap V.defAttr []
    }

dispatch :: Env -> AppEvent -> B.EventM () AppState ()
dispatch env event = do
  now <- liftIO getMonotonicTime
  s <- B.get
  let (s', requests, commands) = runEvent now event s
  B.put s'
  liftIO . atomically $ mapM_ (writeTQueue env.requests) requests
  forM_ commands $ \case
    Halt -> B.halt
    SetTitle title -> do
      vty <- B.getVtyHandle
      liftIO $ V.setWindowTitle vty (T.unpack title)
    After delay e -> liftIO . void . forkIO $ do
      threadDelay (ceiling (delay * microsecondsPerSecond))
      B.writeBChan env.events e
  where
    microsecondsPerSecond :: Double
    microsecondsPerSecond = 1000000
