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
  , ChoiceOption (..)
  , LinePurpose (..)
  , Finding (..)
  , Message (..)
  , PendingKeys (..)
  , SeekState (..)
  , QueueState (..)
  , FindRows (..)
  , BrowserState (..)
  , VisualizerState (..)
  , VisualizerPicture (..)
  , newVisualizer
  , pictureVisualization
  , LyricsState (..)
  , LyricsStatus (..)
  , SongInfoState (..)
  , Location (..)
  , BrowserItem (..)
  , Listing (..)
  , ListingCursor (..)
  , ItemKey (..)
  , Toggles (..)

    -- * Views
  , ViewId (..)
  , View (..)
  , switchScreen
  , Layout (..)
  , focusedView
  , layoutViews
  , mainHeight
  , viewAt
  , progressBarRow
  , statusBarRow
  , listHeight

    -- * Screens
  , ScreenInfo (..)
  , Content (..)
  , screenInfo

    -- * Text
  , countSongs
  , countItems

    -- * Queries
  , cursorVisible
  , cursorHideDelay
  , displayedElapsed
  , progressDuration
  , cellTime
  , progressCells
  , nextCellAt
  , lyricsRows
  , songInfoRows
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import GHC.Generics
import Optics.Core

import Reprise.Action
import Reprise.Collation
import Reprise.Config
import Reprise.Event
import Reprise.Find
import Reprise.Format
import Reprise.History
import Reprise.Keymap
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Lyrics
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.Save
import Reprise.Selection
import Reprise.SongInfo
import Reprise.Style
import Reprise.Width

-- | What doesn't change while reprise runs. The handlers read it through
-- the 'Effectful.Input.Static.Input' effect, so none of them can change it.
data AppEnv = AppEnv
  { config :: Config
  , keymaps :: Keymaps
  -- ^ The keymaps of the config, with the user's overrides.
  , colorMode :: ColorMode
  , collator :: Collator
  -- ^ How lists sort text.
  , lyricsDirectory :: FilePath
  -- ^ Where the lyrics are stored, from the config.
  , editor :: Maybe T.Text
  -- ^ The command that edits a file, from the config or the environment.
  }
  deriving stock (Show, Generic)

data AppState = AppState
  { connection :: ConnectionState
  , mirror :: Mirror
  , heldInput :: Maybe (Seq.Seq AppEvent)
  -- ^ While an edit of the queue waits for its reply, the input that came
  -- meanwhile, in order. It waits until the mirror has the edit, as an
  -- action plans from the positions in the mirror: a second delete would
  -- delete the songs after the first one's.
  , queueState :: QueueState
  , browser :: BrowserState
  , visualizer :: VisualizerState
  , lyrics :: LyricsState
  , songInfo :: SongInfoState
  , outputs :: Maybe (Seq.Seq Output)
  -- ^ MPD's audio outputs, from when the outputs screen first showed. Their
  -- changes keep them current after that.
  , toggles :: Toggles
  , views :: M.Map ViewId View
  , layout :: Layout
  , focus :: ViewId
  , terminalSize :: (Int, Int)
  -- ^ The width and the height.
  , pendingKeys :: Maybe PendingKeys
  , prompt :: Maybe Prompt
  , history :: [T.Text]
  -- ^ The lines of the line prompts, newest first.
  , findPattern :: Maybe T.Text
  -- ^ The pattern of the last find, which every screen finds again.
  , foundOn :: Maybe ScreenName
  -- ^ The screen of the last find, or of the last find again. A screen of
  -- text keeps showing the matches of 'findPattern' on it, since it has no
  -- cursor to show where it went.
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
  , titleShown :: Maybe ((ScreenName, Maybe T.Text), Double)
  -- ^ What the header's title shows and since when, from which a title that
  -- doesn't fit scrolls. Nothing before the first event.
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
  = -- | Options, each picked by a letter of its name, as in ncmpcpp.
    Choice [ChoiceOption]
  | -- | A line of text, what it is for, and the line of the history that
    -- it shows.
    Line LineEdit LinePurpose (Maybe Recall)
  deriving stock (Eq, Show)

-- | An option of a choice.
data ChoiceOption = ChoiceOption
  { letter :: Char
  -- ^ The key that picks it, which is in its name.
  , name :: T.Text
  , event :: Maybe AppEvent
  -- ^ What it sends. Without one, it cancels.
  }
  deriving stock (Eq, Show)

data LinePurpose
  = -- | The @:@ prompt, which runs an action.
    ForCommand
  | ForFind Finding
  | -- | The name of the stored playlist to save to.
    ForSave SaveSource
  | -- | The password that MPD asked for. The line shows as stars.
    ForPassword
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
  , songId :: Maybe SongId
  -- ^ Of the song that the seek moves in.
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

-- | What the visualizer shows.
data VisualizerState = VisualizerState
  { reading :: Maybe VisualizerPicture
  -- ^ What reprise reads the samples for, which it does while the
  -- visualizer shows, with what the samples show so far.
  , drawn :: Seq.Seq Double
  -- ^ When the frames of the last second were drawn, with
  -- @visualizer.debug@.
  , stats :: Maybe FrameStats
  -- ^ What the worker last sent, with @visualizer.debug@.
  , failure :: Maybe T.Text
  -- ^ Why the worker can't read the samples, until it tries again.
  }
  deriving stock (Eq, Show, Generic)

-- | A visualization with what it shows.
data VisualizerPicture
  = -- | The samples of the frames of the ellipse on the screen, the newest
    -- first.
    EllipseFrames (Seq.Seq BS.ByteString)
  | -- | The magnitudes of the left channel's spectrum and of the right
    -- one's.
    SpectrumFrame (Maybe (VS.Vector Double, VS.Vector Double))
  | -- | The samples of the wave.
    WaveFrame (Maybe BS.ByteString)
  deriving stock (Eq, Show)

-- | The visualizer reading the samples for a visualization, or not, before
-- the first frame.
newVisualizer :: Maybe Visualization -> VisualizerState
newVisualizer v = VisualizerState (empty <$> v) Seq.empty Nothing Nothing
  where
    empty :: Visualization -> VisualizerPicture
    empty = \case
      Ellipse -> EllipseFrames Seq.empty
      Spectrum -> SpectrumFrame Nothing
      Wave -> WaveFrame Nothing

-- | The visualization that a picture is of.
pictureVisualization :: VisualizerPicture -> Visualization
pictureVisualization = \case
  EllipseFrames _ -> Ellipse
  SpectrumFrame _ -> Spectrum
  WaveFrame _ -> Wave

-- | What the lyrics screen shows.
data LyricsState = LyricsState
  { song :: Maybe Song
  , token :: Int
  -- ^ Of the request of the song's lyrics. A reply with another token is
  -- of a song that the screen showed before.
  , status :: LyricsStatus
  , following :: Bool
  -- ^ Whether the screen keeps the line being sung in view, until the user
  -- scrolls.
  , inBackground :: Maybe Song
  -- ^ The song whose lyrics were fetched in the background last.
  , playing :: Maybe Song
  -- ^ The song that played after the last event, from which the screen
  -- follows the next one.
  }
  deriving stock (Eq, Show, Generic)

-- | What the song info screen shows.
data SongInfoState = SongInfoState
  { song :: Maybe Song
  , token :: Int
  -- ^ Of the request of the comments of the song's file. A reply with another
  -- token is of a song that the screen showed before.
  , comments :: [(T.Text, T.Text)]
  -- ^ Of the song's file, for its ReplayGain. None until they come.
  }
  deriving stock (Eq, Show, Generic)

data LyricsStatus
  = -- | Until the worker reads the stored lyrics, which takes no time to
    -- see, so the screen shows nothing.
    ReadingLyrics
  | -- | From the fetcher with the name.
    FetchingLyrics T.Text
  | ShowingLyrics LyricsResult
  deriving stock (Eq, Show)

-- | Settings that the user can toggle while reprise runs. They start from
-- the config.
data Toggles = Toggles
  { queueDisplay :: Display
  , followPlaying :: Bool
  , lyricsFollowPlaying :: Bool
  , showBitrate :: Bool
  , browserDisplay :: Display
  , browserSort :: SortBy
  , visualization :: Visualization
  }
  deriving stock (Eq, Show, Generic)

----------------------------------------
-- Views

newtype ViewId = ViewId Int
  deriving newtype (Eq, Ord, Show)

-- | How a screen is shown.
data View = View
  { screen :: ScreenName
  , previous :: Maybe ScreenName
  -- ^ The screen that the view showed before this one, which the screens
  -- of a song, e.g. its lyrics, go back to.
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
newView s = View s Nothing 0 0 0 0 M.empty

-- | Show another screen in a view, at the position where the view left it,
-- after the one that it shows.
switchScreen :: ScreenName -> View -> View
switchScreen s v
  | s == v.screen = v
  | otherwise =
      let (c, o) = M.findWithDefault (0, 0) s v.positions
      in v
           & #positions %~ (M.insert v.screen (v.cursor, v.offset) . M.delete s)
           & #previous ?~ v.screen
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
    , heldInput = Nothing
    , queueState = QueueState noSelection Nothing
    , browser = BrowserState Nothing [] Seq.empty Seq.empty noSelection Nothing
    , visualizer = newVisualizer Nothing
    , lyrics = LyricsState Nothing 0 ReadingLyrics True Nothing Nothing
    , songInfo = SongInfoState Nothing 0 []
    , outputs = Nothing
    , toggles =
        Toggles
          { queueDisplay = config.queue.display
          , followPlaying = config.queue.followPlaying
          , lyricsFollowPlaying = config.lyrics.followPlaying
          , showBitrate = config.statusBar.showBitrate
          , browserDisplay = config.browser.display
          , browserSort = config.browser.sort.by
          , visualization = config.visualizer.visualization
          }
    , -- 'Reprise.Event.Started' shows the startup screen, as a key would.
      views = M.singleton mainView (newView QueueScreen)
    , layout = Single mainView
    , focus = mainView
    , terminalSize = (0, 0)
    , pendingKeys = Nothing
    , prompt = Nothing
    , history = []
    , findPattern = Nothing
    , foundOn = Nothing
    , message = Nothing
    , seek = Nothing
    , now = 0
    , lastInput = 0
    , cursorTimer = False
    , nextToken = 0
    , tick = Nothing
    , windowTitle = Nothing
    , titleShown = Nothing
    }
  where
    mainView :: ViewId
    mainView = ViewId 0

-- | The view that actions and key lookups target.
focusedView :: AppState -> View
focusedView s = M.findWithDefault (newView QueueScreen) s.focus s.views

-- | The height of the main area: the terminal without the header, the
-- progress bar and the status bar.
mainHeight :: (Int, Int) -> Int
mainHeight (_, h) = max 0 (h - headerHeight - footerHeight)

-- | The title and the line of the header.
headerHeight :: Int
headerHeight = 2

-- | The progress bar and the status bar.
footerHeight :: Int
footerHeight = 2

-- | The row of the progress bar, at the top of the footer.
progressBarRow :: AppState -> Int
progressBarRow s = snd s.terminalSize - footerHeight

-- | The row of the status bar, at the bottom of the terminal.
statusBarRow :: AppState -> Int
statusBarRow s = snd s.terminalSize - 1

-- | The view that shows a cell of the terminal, at a column and a row, with
-- the column and the row of the cell in the view.
viewAt :: AppState -> Int -> Int -> Maybe (ViewId, (Int, Int))
viewAt s col row = case s.layout of
  Single vid
    | col >= 0 && col < fst s.terminalSize
    , row >= headerHeight && row < headerHeight + mainHeight s.terminalSize ->
        Just (vid, (col, row - headerHeight))
  Single _ -> Nothing

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
listHeight env s v = case (screenInfo v.screen).content of
  Songs display
    | env.config.songs.columns.showTitles && display s == Columns -> max 0 (v.height - 1)
  _ -> v.height

----------------------------------------
-- Screens

-- | What the code under the screens' modules knows of a screen, e.g. to keep
-- a view's cursor in its list.
data ScreenInfo = ScreenInfo
  { built :: Bool
  -- ^ Whether the screen is built yet. The others are names for later.
  , content :: Content
  , size :: AppEnv -> AppState -> View -> Int
  -- ^ The number of items, or of lines at the view's width.
  , songAt :: AppState -> Int -> Maybe Song
  -- ^ The song of the item at an index.
  , title :: AppEnv -> AppState -> (T.Text, T.Text)
  -- ^ The title in the header: a part that stays, and a part that scrolls
  -- if it doesn't fit, as in ncmpcpp.
  , subject :: AppState -> Maybe T.Text
  -- ^ What the screen shows, e.g. the song of the lyrics. The title scrolls
  -- from its start when it changes.
  }

data Content
  = -- | Items that can be songs, which show in a display.
    Songs (AppState -> Display)
  | -- | Items without songs, e.g. the outputs.
    Items
  | -- | Lines of text without a cursor, which a move scrolls.
    Lines
  | -- | A picture, e.g. the visualizer's.
    Picture

screenInfo :: ScreenName -> ScreenInfo
screenInfo = \case
  QueueScreen ->
    ScreenInfo
      { built = True
      , content = Songs (.toggles.queueDisplay)
      , size = \_ s _ -> Seq.length s.mirror.queue
      , songAt = \s i -> Seq.lookup i s.mirror.queue
      , title = queueTitle
      , -- The length of the queue in the title changes as the songs play.
        subject = const Nothing
      }
  BrowserScreen ->
    ScreenInfo
      { built = True
      , content = Songs (.toggles.browserDisplay)
      , size = \_ s _ -> Seq.length s.browser.items
      , songAt = \s i -> case Seq.lookup i s.browser.items of
          Just (EntryItem (SongEntry song)) -> Just song
          _ -> Nothing
      , title = \_ s -> ("Browse: ", "/" <> fromMaybe "" (browsed s))
      , subject = browsed
      }
  SearchEngineScreen -> unbuilt SearchEngineScreen
  MediaLibraryScreen -> unbuilt MediaLibraryScreen
  PlaylistEditorScreen -> unbuilt PlaylistEditorScreen
  OutputsScreen ->
    ScreenInfo
      { built = True
      , content = Items
      , size = \_ s _ -> maybe 0 Seq.length s.outputs
      , songAt = noSongs
      , title = named OutputsScreen
      , subject = const Nothing
      }
  VisualizerScreen ->
    ScreenInfo
      { built = True
      , content = Picture
      , size = \_ _ _ -> 0
      , songAt = noSongs
      , title = named VisualizerScreen
      , subject = const Nothing
      }
  LyricsScreen ->
    ScreenInfo
      { built = True
      , content = Lines
      , size = \_ s v -> length (lyricsRows v.width s.lyrics)
      , songAt = noSongs
      , title = \_ s -> ("Lyrics: ", maybe "" songName s.lyrics.song)
      , subject = \s -> songName <$> s.lyrics.song
      }
  SongInfoScreen ->
    ScreenInfo
      { built = True
      , content = Lines
      , size = \env s v -> length (songInfoRows env s v.width)
      , songAt = noSongs
      , title = \_ s -> ("Song info: ", maybe "" songName s.songInfo.song)
      , subject = \s -> songName <$> s.songInfo.song
      }
  HelpScreen ->
    ScreenInfo
      { built = True
      , content = Lines
      , size = \env _ _ -> length (helpLines env.keymaps)
      , songAt = noSongs
      , title = named HelpScreen
      , subject = const Nothing
      }
  where
    unbuilt :: ScreenName -> ScreenInfo
    unbuilt screen =
      ScreenInfo
        { built = False
        , content = Items
        , size = \_ _ _ -> 0
        , songAt = noSongs
        , title = named screen
        , subject = const Nothing
        }

    -- The path of the directory or the playlist that the browser lists.
    browsed :: AppState -> Maybe T.Text
    browsed s =
      s.browser.location <&> \case
        InDirectory path -> path
        InPlaylist path -> path

    noSongs :: AppState -> Int -> Maybe Song
    noSongs _ _ = Nothing

    named :: ScreenName -> AppEnv -> AppState -> (T.Text, T.Text)
    named screen _ _ = (screenLabel screen, "")

    queueTitle :: AppEnv -> AppState -> (T.Text, T.Text)
    queueTitle env s =
      let q = s.mirror.queue
          total = s.mirror.totalLength
          remaining = case currentPosition s.mirror of
            Just _ -> s.mirror.lengthFromCurrent - fromMaybe 0 (displayedElapsed s)
            Nothing -> total
          times =
            [formatTotal total | total > 0]
              <> [formatTotal remaining <> " left" | env.config.queue.showRemainingTime, remaining > 0]
      in ("Queue ", "(" <> T.intercalate ", " (countSongs (Seq.length q) : times) <> ")")
      where
        -- A short total, e.g. @1h 23m@.
        formatTotal :: Seconds -> T.Text
        formatTotal secs =
          let total = floor @Seconds @Int secs
              (d, r1) = total `divMod` 86400
              (h, r2) = r1 `divMod` 3600
              (m, sec) = r2 `divMod` 60
              parts = [(d, "d"), (h, "h"), (m, "m"), (sec, "s")]
              significant = take 2 $ dropWhile ((== 0) . fst) parts
          in case significant of
               [] -> "0s"
               _ -> T.unwords [T.pack (show n) <> unit | (n, unit) <- significant, n > 0]

----------------------------------------
-- Text

countSongs :: Int -> T.Text
countSongs n = T.pack (show n) <> if n == 1 then " song" else " songs"

countItems :: Int -> T.Text
countItems n = T.pack (show n) <> if n == 1 then " item" else " items"

----------------------------------------
-- Queries

-- | Whether the queue shows its cursor: it hides it a while after the last
-- key or step of the wheel.
cursorVisible :: AppState -> Bool
cursorVisible s = s.now - s.lastInput < cursorHideDelay

-- | How long after the last key the queue hides its cursor. ncmpcpp's
-- default @playlist_disable_highlight_delay@.
cursorHideDelay :: Double
cursorHideDelay = 5

-- | The length of the song whose progress the progress bar shows, while it
-- plays or pauses.
progressDuration :: AppState -> Maybe Seconds
progressDuration s = do
  st <- s.mirror.status
  guard (st.state /= Stopped)
  d <- st.duration
  guard (d > 0)
  pure d

-- | The time in a song of a length where a cell of a progress bar of a
-- width begins to fill.
cellTime :: Int -> Int -> Seconds -> Seconds
cellTime width cell duration = duration * fromIntegral cell / fromIntegral width

-- | The cells of a progress bar of a width that the elapsed time of a song
-- of a length fills.
progressCells :: Int -> Double -> Double -> Int
progressCells width elapsed duration =
  min width (floor (min elapsed duration / duration * fromIntegral width))

-- | The elapsed time at which the next cell of a progress bar of a width
-- fills, for a redraw.
nextCellAt :: Int -> Double -> Double -> Double
nextCellAt width elapsed duration =
  fromIntegral (progressCells width elapsed duration + 1) * duration / fromIntegral width

-- | The elapsed time to show: the target of a seek in progress, or the
-- interpolated elapsed time.
displayedElapsed :: AppState -> Maybe Seconds
displayedElapsed s = case s.seek of
  Just sk -> Just sk.target
  Nothing -> elapsedAt s.now s.mirror

-- | The rows of the lyrics screen at a width, with long lines wrapped, and
-- the index of the timed line of each.
lyricsRows :: Int -> LyricsState -> [(Maybe Int, T.Text)]
lyricsRows width st = case (st.song, st.status) of
  (Nothing, _) -> []
  (_, ReadingLyrics) -> []
  (_, FetchingLyrics fetcher) -> untimed ["Fetching the lyrics from " <> fetcher <> "…"]
  (_, ShowingLyrics result) -> case result of
    LyricsFound _ lyrics -> case lyrics.timed of
      Just timed ->
        concat
          [ map (Just i,) (wrapText width (sanitize text))
          | (i, (_, text)) <- zip [0 ..] timed.entries
          ]
      Nothing -> untimed (T.lines lyrics.plain)
    LyricsInstrumental -> untimed ["Instrumental"]
    LyricsMissing [] -> untimed ["No lyrics stored"]
    LyricsMissing asked -> untimed (missingLines asked)
    LyricsFailed reason -> untimed [reason]
  where
    untimed :: [T.Text] -> [(Maybe Int, T.Text)]
    untimed = concatMap (map (Nothing,) . wrapText width . sanitize)

-- | The rows of the song info screen at a width: a label and a value, which
-- the song can be without.
songInfoRows :: AppEnv -> AppState -> Int -> [(T.Text, Maybe T.Text)]
songInfoRows env s width = case s.songInfo.song of
  Nothing -> []
  Just song ->
    infoRows width $
      songInfoLines env.config.songs.tagSeparator (bitrateOf song) s.songInfo.comments song
  where
    -- MPD has the bitrate of the song that plays only.
    bitrateOf :: Song -> Maybe Int
    bitrateOf song = do
      playing <- currentSong s.mirror
      guard $ sameSong playing song
      s.mirror.status >>= (.bitrate)
