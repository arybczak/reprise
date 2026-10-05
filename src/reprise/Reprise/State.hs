-- | The state of the application.
--
-- Screen state and view state are separate, like the buffers and windows of
-- Emacs. A screen holds its content, a view holds how it is shown. Today
-- there is one view, but nothing assumes it: actions find their view with
-- 'focusedView', and only the layout code reads the terminal size.
module Reprise.State
  ( -- * Settings
    AppEnv (..)

    -- * State
  , AppState (..)
  , initialState
  , ConnectionState (..)
  , Prompt (..)
  , PromptInput (..)
  , LinePurpose (..)
  , Finding (..)
  , Message (..)
  , PendingKeys (..)
  , SeekState (..)
  , QueueState (..)
  , FindRows (..)
  , BrowserState (..)
  , Location (..)
  , BrowserItem (..)
  , Listing (..)
  , ListingCursor (..)
  , ItemKey (..)
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
  , listHeight
  , screenDisplay

    -- * Queries
  , cursorVisible
  , cursorHideDelay
  , displayedElapsed
  ) where

import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Generics
import Optics.Core

import Reprise.Action
import Reprise.Collation
import Reprise.Config
import Reprise.Event
import Reprise.Find
import Reprise.Keymap
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.Selection
import Reprise.Style

-- | What doesn't change while reprise runs. The handlers read it through
-- the 'Effectful.Input.Static.Input' effect, so none of them can change it.
data AppEnv = AppEnv
  { config :: Config
  , keymaps :: Keymaps
  -- ^ The keymaps of the config, with the user's overrides.
  , colorMode :: ColorMode
  , collator :: Collator
  -- ^ How lists sort text.
  }
  deriving stock (Show, Generic)

data AppState = AppState
  { connection :: ConnectionState
  , mirror :: Mirror
  , queueState :: QueueState
  , browser :: BrowserState
  , toggles :: Toggles
  , views :: M.Map ViewId View
  , layout :: Layout
  , focus :: ViewId
  , terminalSize :: (Int, Int)
  -- ^ The width and the height.
  , pendingKeys :: Maybe PendingKeys
  , prompt :: Maybe Prompt
  , findPattern :: Maybe T.Text
  -- ^ The pattern of the last find, which every screen finds again.
  , message :: Maybe Message
  , seek :: Maybe SeekState
  , now :: Double
  -- ^ The monotonic time of the event being handled.
  , lastInput :: Double
  , cursorTimer :: Bool
  -- ^ Whether the timer that hides the queue's cursor is set.
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
data Prompt = Prompt
  { question :: T.Text
  , input :: PromptInput
  }
  deriving stock (Eq, Show, Generic)

data PromptInput
  = -- | The event that yes sends.
    YesNo AppEvent
  | -- | A line of text, and what it is for.
    Line LineEdit LinePurpose
  deriving stock (Eq, Show)

data LinePurpose
  = -- | The @:@ prompt, which runs an action.
    ForCommand
  | ForFind Finding
  deriving stock (Eq, Show)

-- | A find in progress, which moves the cursor while the user types.
data Finding = Finding
  { direction :: Direction
  , origin :: (Int, Int)
  -- ^ The cursor and the offset where the find started, for a cancel.
  , note :: Maybe T.Text
  -- ^ What the find found so far, e.g. that it wrapped around.
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
data QueueState = QueueState
  { selection :: Selection SongId
  -- ^ By id, so that the selection follows the songs when they move.
  , findRows :: Maybe FindRows
  }
  deriving stock (Eq, Show, Generic)

-- | The rows of the queue as finds match them, for the version of the queue
-- and the display that they were made for. A find runs on every key, and
-- without a match near the start it goes through every row.
data FindRows = FindRows
  { version :: Maybe PlaylistVersion
  , display :: Display
  , rows :: Seq.Seq Folded
  -- ^ Made lazily, as finds reach them.
  }
  deriving stock (Eq, Show, Generic)

-- | The content of the browser: the listing of a directory or a playlist.
data BrowserState = BrowserState
  { location :: Maybe Location
  -- ^ What the items list, before the first listing nothing.
  , entries :: [Entry]
  -- ^ In MPD's order, for another sort.
  , items :: Seq.Seq BrowserItem
  -- ^ The entries in the order of the sort, after @..@.
  , rows :: Seq.Seq Folded
  -- ^ The text of each item as finds match it, made lazily, as finds reach
  -- the items.
  , selection :: Selection ItemKey
  -- ^ Of the items of what the browser lists.
  , listing :: Maybe Listing
  -- ^ The listing that was requested last, until its reply comes.
  }
  deriving stock (Eq, Show, Generic)

-- | What the browser lists.
data Location
  = -- | A directory of the database, @""@ for the root.
    InDirectory T.Text
  | -- | The songs of a playlist.
    InPlaylist T.Text
  deriving stock (Eq, Show)

data BrowserItem
  = -- | The way up, @..@.
    ParentItem
  | EntryItem Entry
  deriving stock (Eq, Show)

-- | A listing on its way. A reply with another token is of a listing that
-- a newer one replaced.
data Listing = Listing
  { token :: Int
  , location :: Location
  , cursor :: ListingCursor
  }
  deriving stock (Eq, Show, Generic)

-- | Where the cursor goes when a listing comes.
data ListingCursor
  = -- | To the first item, e.g. in a directory that the user entered.
    AtTop
  | -- | To an item in the middle of the list, e.g. the directory that the
    -- user went up from.
    JumpTo ItemKey
  | -- | To the item that it was on, without scrolling, when the browser lists
    -- the same again, e.g. after the database changed. Without the item, it
    -- stays where it was.
    StayOn ItemKey
  deriving stock (Eq, Show)

-- | What an item is found by in a new listing.
data ItemKey
  = ParentKey
  | DirectoryKey T.Text
  | SongKey T.Text (Maybe SongRange)
  | PlaylistKey T.Text
  deriving stock (Eq, Ord, Show)

-- | Settings that the user can toggle while reprise runs. They start from
-- the config.
data Toggles = Toggles
  { queueDisplay :: Display
  , albumSeparators :: Bool
  , followPlaying :: Bool
  , showBitrate :: Bool
  , browserDisplay :: Display
  , browserSort :: SortBy
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

initialState :: Config -> AppState
initialState config =
  AppState
    { connection = Connecting
    , mirror = emptyMirror
    , queueState = QueueState noSelection Nothing
    , browser = BrowserState Nothing [] Seq.empty Seq.empty noSelection Nothing
    , toggles =
        Toggles
          { queueDisplay = config.queue.display
          , albumSeparators = config.queue.albumSeparators
          , followPlaying = config.queue.followPlaying
          , showBitrate = config.statusBar.showBitrate
          , browserDisplay = config.browser.display
          , browserSort = config.browser.sort.by
          }
    , views = M.singleton mainView (newView config.startupScreen)
    , layout = Single mainView
    , focus = mainView
    , terminalSize = (0, 0)
    , pendingKeys = Nothing
    , prompt = Nothing
    , findPattern = Nothing
    , message = Nothing
    , seek = Nothing
    , now = 0
    , lastInput = 0
    , cursorTimer = False
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

-- | The number of rows of the list in a view: the titles of the columns
-- take one.
listHeight :: AppEnv -> AppState -> View -> Int
listHeight env s v
  | env.config.songs.columns.showTitles && screenDisplay s v.screen == Just Columns =
      max 0 (v.height - 1)
  | otherwise = v.height

-- | How a screen shows songs, if it lists them.
screenDisplay :: AppState -> ScreenName -> Maybe Display
screenDisplay s = \case
  QueueScreen -> Just s.toggles.queueDisplay
  BrowserScreen -> Just s.toggles.browserDisplay
  _ -> Nothing

----------------------------------------
-- Queries

-- | Whether the queue shows its cursor: it hides it a while after the last
-- key.
cursorVisible :: AppState -> Bool
cursorVisible s = s.now - s.lastInput < cursorHideDelay

-- | How long after the last key the queue hides its cursor. ncmpcpp's
-- default @playlist_disable_highlight_delay@.
cursorHideDelay :: Double
cursorHideDelay = 5

-- | The elapsed time to show: the target of a seek in progress, or the
-- interpolated elapsed time.
displayedElapsed :: AppState -> Maybe Seconds
displayedElapsed s = case s.seek of
  Just sk -> Just sk.target
  Nothing -> elapsedAt s.now s.mirror
