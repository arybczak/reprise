-- | The brick application: a thin adapter that runs each event through the
-- pure handlers and then performs the requests they collected.
module Reprise.App
  ( Channels (..)
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

-- | Where the requests of the handlers go, and where events come from.
data Channels = Channels
  { requests :: TQueue PendingRequest
  , events :: B.BChan AppEvent
  , visualizing :: TVar Bool
  -- ^ Whether the visualizer's worker reads the samples.
  }

-- | The size of the channel of events from the workers and the timers. A
-- full channel blocks them until the UI catches up. brick's own channel of
-- terminal events has this size.
eventChannelSize :: Int
eventChannelSize = 20

app :: AppEnv -> Channels -> B.App AppState AppEvent ()
app env channels =
  B.App
    { B.appDraw = \s ->
        [ maybe id (\(x, y) -> B.showCursor () (B.Location (x, y))) (promptCursor s) $
            B.raw (renderScreen env s)
        ]
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = \case
        B.AppEvent e -> dispatch env channels e
        B.VtyEvent (V.EvKey k mods) -> forM_ (fromVtyKey k mods) (dispatch env channels . KeyPressed)
        B.VtyEvent (V.EvResize w h) -> dispatch env channels (Resized w h)
        _ -> pure ()
    , B.appStartEvent = do
        vty <- B.getVtyHandle
        (w, h) <- liftIO . V.displayBounds $ V.outputIface vty
        dispatch env channels (Resized w h)
    , B.appAttrMap = const $ B.attrMap V.defAttr []
    }

dispatch :: AppEnv -> Channels -> AppEvent -> B.EventM () AppState ()
dispatch env channels event = do
  now <- liftIO getMonotonicTime
  s <- B.get
  (s', requests, commands) <- liftIO $ runEvent env now event s
  B.put s'
  liftIO . atomically $ mapM_ (writeTQueue channels.requests) requests
  forM_ commands $ \case
    Halt -> B.halt
    SetTitle title -> do
      vty <- B.getVtyHandle
      liftIO $ V.setWindowTitle vty (T.unpack title)
    After delay e -> liftIO . void . forkIO $ do
      threadDelay (ceiling (delay * microsecondsPerSecond))
      B.writeBChan channels.events e
    KeepScreen -> B.continueWithoutRedraw
    Visualize on -> liftIO . atomically $ writeTVar channels.visualizing on
  where
    microsecondsPerSecond :: Double
    microsecondsPerSecond = 1000000
