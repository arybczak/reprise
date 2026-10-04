-- | The state of the application.
--
-- Screen state and view state are separate, like the buffers and windows of
-- Emacs. A screen holds its content, a view holds how it is shown. Today
-- there is one view, but nothing assumes it: actions find their view with
-- 'focusedView', and only the layout code reads the terminal size.
module Reprise.State
  ( -- * State
    AppState (..)
  , initialState
  , ConnectionState (..)
  , Prompt (..)
  , Message (..)
  , PendingKeys (..)
  , SeekState (..)
  , QueueState (..)
  , Toggles (..)

    -- * Views
  , ViewId (..)
  , View (..)
  , newView
  , switchScreen
  , Layout (..)
  , focusedView
  , focusedViewId
  , layoutViews
  , mainHeight
  ) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics
import MPD.Types
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Event
import Reprise.Keymap
import Reprise.Keys
import Reprise.Mpd.Mirror
import Reprise.Style

data AppState = AppState
  { config :: Config
  , keymaps :: Keymaps
  , colorMode :: ColorMode
  , connection :: ConnectionState
  , mirror :: Mirror
  , queueState :: QueueState
  , toggles :: Toggles
  , views :: M.Map ViewId View
  , layout :: Layout
  , focus :: ViewId
  , terminalSize :: (Int, Int)
  -- ^ The width and the height.
  , pendingKeys :: Maybe PendingKeys
  , prompt :: Maybe Prompt
  , message :: Maybe Message
  , seek :: Maybe SeekState
  , now :: Double
  -- ^ The monotonic time of the event being handled.
  , lastInput :: Double
  , nextToken :: Int
  -- ^ The token for the next timer.
  , tick :: Maybe (Int, Double)
  -- ^ The token and the time of the scheduled redraw of the elapsed time.
  , windowTitle :: Maybe T.Text
  -- ^ The title that reprise set last.
  , jumpedToPlaying :: Bool
  -- ^ Whether the cursor moved to the playing song after the start.
  }
  deriving stock (Show, Generic)

data ConnectionState
  = Connecting
  | Connected Version
  | Disconnected T.Text
  deriving stock (Eq, Show)

-- | A prompt in the status bar. Keys go to it while it is open.
data Prompt = Confirm
  { question :: T.Text
  , onYes :: AppEvent
  }
  deriving stock (Eq, Show, Generic)

data Message = Message
  { text :: T.Text
  , isError :: Bool
  , token :: Int
  }
  deriving stock (Eq, Show, Generic)

-- | The keys of an unfinished key sequence and where they lead.
data PendingKeys = PendingKeys
  { keys :: [KeySpec]
  , layers :: Layers
  }
  deriving stock (Eq, Show, Generic)

-- | A seek in progress: the presses move a target, and a pause sends it.
data SeekState = SeekState
  { target :: Seconds
  , started :: Double
  , token :: Int
  }
  deriving stock (Eq, Show, Generic)

-- | The content of the queue screen that isn't MPD's: the songs come from
-- the mirror.
newtype QueueState = QueueState
  { selection :: S.Set SongId
  -- ^ By id, so that the selection follows the songs when they move.
  }
  deriving stock (Eq, Show, Generic)

-- | Settings that the user can toggle while reprise runs. They start from
-- the config.
data Toggles = Toggles
  { queueDisplay :: Display
  , albumSeparators :: Bool
  , followPlaying :: Bool
  , showBitrate :: Bool
  }
  deriving stock (Eq, Show, Generic)

----------------------------------------
-- Views

newtype ViewId = ViewId Int
  deriving newtype (Eq, Ord, Show)

-- | How a screen is shown.
data View = View
  { screen :: ScreenName
  , cursor :: Int
  , offset :: Int
  -- ^ The index of the first visible item.
  , width :: Int
  , height :: Int
  , positions :: M.Map ScreenName (Int, Int)
  -- ^ The cursor and the offset of each other screen that the view showed,
  -- for when it shows the screen again.
  }
  deriving stock (Eq, Show, Generic)

-- | A view of a screen from its start.
newView :: ScreenName -> View
newView s = View s 0 0 0 0 M.empty

-- | Show another screen in a view, at the position where the view left it.
switchScreen :: ScreenName -> View -> View
switchScreen s v
  | s == v.screen = v
  | otherwise =
      let (c, o) = M.findWithDefault (0, 0) s v.positions
      in v
           & #positions %~ (M.insert v.screen (v.cursor, v.offset) . M.delete s)
           & #screen .~ s
           & #cursor .~ c
           & #offset .~ o

-- | The arrangement of the views. A window tree would add a split.
newtype Layout = Single ViewId
  deriving stock (Eq, Show)

initialState :: Config -> Keymaps -> ColorMode -> AppState
initialState config keymaps colorMode =
  AppState
    { config = config
    , keymaps = keymaps
    , colorMode = colorMode
    , connection = Connecting
    , mirror = emptyMirror
    , queueState = QueueState S.empty
    , toggles =
        Toggles
          { queueDisplay = config.queue.display
          , albumSeparators = config.queue.albumSeparators
          , followPlaying = config.queue.followPlaying
          , showBitrate = config.statusBar.showBitrate
          }
    , views = M.singleton mainView (newView config.startupScreen)
    , layout = Single mainView
    , focus = mainView
    , terminalSize = (0, 0)
    , pendingKeys = Nothing
    , prompt = Nothing
    , message = Nothing
    , seek = Nothing
    , now = 0
    , lastInput = 0
    , nextToken = 0
    , tick = Nothing
    , windowTitle = Nothing
    , jumpedToPlaying = False
    }
  where
    mainView :: ViewId
    mainView = ViewId 0

focusedViewId :: AppState -> ViewId
focusedViewId s = s.focus

-- | The view that actions and key lookups target.
focusedView :: AppState -> View
focusedView s = M.findWithDefault (newView QueueScreen) s.focus s.views

-- | The height of the main area: the terminal without the header (2
-- lines), the progress bar and the status bar.
mainHeight :: (Int, Int) -> Int
mainHeight (_, h) = max 0 (h - 4)

-- | Give the views their sizes from the terminal size.
layoutViews :: AppState -> AppState
layoutViews s = case s.layout of
  Single vid ->
    s
      & #views
        % ix vid
        %~ ( \v ->
               v
                 & #width .~ fst s.terminalSize
                 & #height .~ mainHeight s.terminalSize
           )
