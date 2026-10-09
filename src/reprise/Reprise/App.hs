-- | The event loop: a thin adapter that runs each event through the pure
-- handlers, performs the requests they collected, and draws the screen
-- with vty.
module Reprise.App
  ( Channels (..)
  , runApp
  , eventQueueSize
  , runEditor
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor
import Data.IORef.Strict qualified as S
import Data.Text qualified as T
import GHC.Clock
import Graphics.Vty qualified as V
import Numeric.Natural
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Reprise.Config
import Reprise.Effect.Clock
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Exception
import Reprise.Handler
import Reprise.Keys
import Reprise.Lyrics
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.UI.Layout

-- | Where the requests of the handlers go, and where events come from.
data Channels = Channels
  { requests :: TQueue PendingRequest
  , events :: TBQueue AppEvent
  -- ^ The events of the workers and the timers.
  , visualizing :: TVar (Maybe Visualization)
  -- ^ What the visualizer's worker reads the samples for.
  , lyrics :: TVar (Maybe (Int, LyricsRequest))
  -- ^ The newest request of lyrics for their worker, with its token.
  , lyricsInBackground :: TVar (Maybe Song)
  -- ^ The song whose lyrics their worker fetches in the background.
  , saveToHistoryFile :: T.Text -> IO ()
  -- ^ Add a line of a prompt to the history file.
  }

-- | The size of the queue of events from the workers and the timers. A full
-- queue blocks them until the UI catches up, and drops the visualizer's
-- frames. It is the size of brick's channels, which reprise used before.
eventQueueSize :: Natural
eventQueueSize = 20

-- | Run the UI from the state until an action halts it. The terminal gets
-- a new vty from the builder after the editor had it.
runApp :: AppEnv -> Channels -> IO V.Vty -> AppState -> IO ()
runApp env channels buildVty initial = do
  first <- buildVty
  current <- S.newIORef first
  let finish = do
        V.shutdown =<< S.readIORef current
        V.restoreInputState (V.inputIface first)
  flip finally finish $ do
    (w, h) <- displaySize first
    sized <- dispatch current initial (Resized w h)
    loop current =<< case sized of
      Halted -> pure Halted
      Running s _ -> dispatch current s Started
  where
    loop :: S.IORef V.Vty -> Next -> IO ()
    loop current next = case next of
      Halted -> pure ()
      Running s redraw -> do
        vty <- S.readIORef current
        when redraw $ draw vty s
        -- Keys go first.
        received <-
          atomically $
            (Left <$> readTChan (V.eventChannel (V.inputIface vty)))
              `orElse` (Right <$> readTBQueue channels.events)
        event <- case received of
          Left (V.InputEvent (V.EvKey k mods)) -> pure (KeyPressed <$> fromVtyKey k mods)
          Left (V.InputEvent (V.EvResize w h)) -> pure . Just $ Resized w h
          -- vty-unix sends it after the terminal changed its size.
          Left V.ResumeAfterInterrupt -> Just . uncurry Resized <$> displaySize vty
          Left (V.InputEvent _) -> pure Nothing
          Right e -> pure (Just e)
        loop current =<< maybe (pure (Running s False)) (dispatch current s) event

    dispatch :: S.IORef V.Vty -> AppState -> AppEvent -> IO Next
    dispatch current s event = do
      now <- getMonotonicTime
      (s', requests, commands) <- runEvent env now event s
      atomically $ mapM_ (writeTQueue channels.requests) requests
      foldM (perform current) (Running s' True) commands

    -- The commands of an event in order. An edit runs the events that
    -- follow it, and a halt ends the loop after them.
    perform :: S.IORef V.Vty -> Next -> UiCommand -> IO Next
    perform current next command = case next of
      Halted -> pure Halted
      Running s _ -> case command of
        Halt -> pure Halted
        SetTitle title -> do
          vty <- S.readIORef current
          V.setWindowTitle vty (T.unpack title)
          pure next
        After delay e -> do
          void . forkIO $ do
            delaySeconds delay
            atomically $ writeTBQueue channels.events e
          pure next
        KeepScreen -> pure (Running s False)
        Visualize v -> do
          atomically $ writeTVar channels.visualizing v
          pure next
        FetchLyrics token wanted -> do
          atomically $ writeTVar channels.lyrics (Just (token, wanted))
          pure next
        FetchLyricsInBackground song -> do
          atomically $ writeTVar channels.lyricsInBackground (Just song)
          pure next
        SaveToHistory line -> do
          channels.saveToHistoryFile line
          pure next
        Edit cmd file -> do
          V.shutdown =<< S.readIORef current
          failure <- runEditor cmd file
          vty <- buildVty
          S.writeIORef current vty
          (w, h) <- displaySize vty
          resized <- dispatch current s (Resized w h)
          -- The new vty starts with an empty screen.
          case resized of
            Halted -> pure Halted
            Running s' _ ->
              dispatch current s' (Edited file failure) <&> \case
                Halted -> Halted
                Running s'' _ -> Running s'' True

    draw :: V.Vty -> AppState -> IO ()
    draw vty s =
      V.update vty $
        (V.picForImage (renderScreen env s))
          { V.picCursor = maybe V.NoCursor (uncurry V.AbsoluteCursor) (promptCursor s)
          }

    displaySize :: V.Vty -> IO (Int, Int)
    displaySize = V.displayBounds . V.outputIface

-- | What the loop does after an event.
data Next
  = Halted
  | -- | The state, and whether to draw it.
    Running AppState Bool

-- | Edit a file with an editor command, in a directory that exists. The
-- command is run by @sh@, as it can have arguments, e.g. @emacs -nw@, and
-- the file is its argument, so that no name of a file is read as shell
-- code. Returns why it failed.
runEditor :: T.Text -> FilePath -> IO (Maybe T.Text)
runEditor command file =
  (failure <$> run) `catchSync` \err ->
    pure . Just $ "The editor can't run: " <> exceptionText err
  where
    failure :: ExitCode -> Maybe T.Text
    failure = \case
      ExitSuccess -> Nothing
      ExitFailure code -> Just $ "The editor exited with " <> T.pack (show code)

    run :: IO ExitCode
    run = do
      createDirectoryIfMissing True (takeDirectory file)
      (_, _, _, p) <-
        createProcess $ proc "sh" ["-c", T.unpack command <> " \"$1\"", "sh", file]
      waitForProcess p
