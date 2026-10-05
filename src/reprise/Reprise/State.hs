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
  , VisualizerState (..)
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
  , screenTitle
  , titleSubject
  , shownTitle
  , titleSince
  , titleScrolls
  , headerRight
  , displayedElapsed
  , lyricsRows
  , songInfoRows
  , sungLine
  , lyricsOffset
  , nextLyricsLine
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.List qualified as L
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
import Reprise.Keymap
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Lyrics
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
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
  , queueState :: QueueState
  , browser :: BrowserState
  , visualizer :: VisualizerState
  , lyrics :: LyricsState
  , songInfo :: SongInfoState
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
  , titleShown :: Maybe ((ScreenName, Maybe Location), Double)
  -- ^ What the header's title shows and since when, from which a title that
  -- doesn't fit scrolls. Nothing before the first event.
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

-- | What the visualizer shows.
data VisualizerState = VisualizerState
  { reading :: Maybe Visualization
  -- ^ What reprise asked the samples for, which it does while the
  -- visualizer shows.
  , frames :: Seq.Seq BS.ByteString
  -- ^ The samples of the frames of the ellipse on the screen, the newest
  -- first.
  , spectrum :: [VS.Vector Double]
  -- ^ The magnitudes of each channel's spectrum.
  }
  deriving stock (Eq, Show, Generic)

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
  , returnTo :: ScreenName
  -- ^ The screen that the lyrics were shown from, which showing them again
  -- goes back to.
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
  , returnTo :: ScreenName
  -- ^ The screen that the song was shown from, which showing it again goes
  -- back to.
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
  , albumSeparators :: Bool
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
    , visualizer = VisualizerState Nothing Seq.empty []
    , lyrics = LyricsState Nothing 0 ReadingLyrics True QueueScreen Nothing Nothing
    , songInfo = SongInfoState Nothing 0 [] QueueScreen
    , toggles =
        Toggles
          { queueDisplay = config.queue.display
          , albumSeparators = config.queue.albumSeparators
          , followPlaying = config.queue.followPlaying
          , lyricsFollowPlaying = config.lyrics.followPlaying
          , showBitrate = config.statusBar.showBitrate
          , browserDisplay = config.browser.display
          , browserSort = config.browser.sort.by
          , visualization = config.visualizer.visualization
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
    , titleShown = Nothing
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

-- | The title of a screen in the header: a part that stays, and a part that
-- scrolls if it doesn't fit, as in ncmpcpp.
screenTitle :: AppEnv -> AppState -> ScreenName -> (T.Text, T.Text)
screenTitle env s = \case
  QueueScreen ->
    let q = s.mirror.queue
        total = s.mirror.totalLength
        remaining = case currentPosition s.mirror of
          Just _ -> s.mirror.lengthFromCurrent - fromMaybe 0 (displayedElapsed s)
          Nothing -> total
        count = T.pack (show (Seq.length q)) <> if Seq.length q == 1 then " song" else " songs"
        times =
          [formatTotal total | total > 0]
            <> [formatTotal remaining <> " left" | env.config.queue.showRemainingTime, remaining > 0]
    in ("Queue ", "(" <> T.intercalate ", " (count : times) <> ")")
  BrowserScreen ->
    ( "Browse: "
    , "/" <> case s.browser.location of
        Just (InDirectory path) -> path
        Just (InPlaylist path) -> path
        Nothing -> ""
    )
  LyricsScreen -> ("Lyrics: ", maybe "" lyricsName s.lyrics.song)
  SongInfoScreen -> ("Song info: ", maybe "" lyricsName s.songInfo.song)
  other -> (T.toTitle (T.replace "_" " " (screenName other)), "")
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

-- | What the title shows: the focused screen, and what the browser lists.
-- The scrolling starts again when it changes.
titleSubject :: AppState -> (ScreenName, Maybe Location)
titleSubject s = case (focusedView s).screen of
  BrowserScreen -> (BrowserScreen, s.browser.location)
  screen -> (screen, Nothing)

-- | The focused screen's title as the header shows it. The part that doesn't
-- fit next to the volume scrolls by a character for each second since the
-- title began to show its subject.
shownTitle :: AppEnv -> AppState -> T.Text
shownTitle env s =
  let (stays, rest) = screenTitle env s (focusedView s).screen
      room = titleRoom s stays
  in if textWidth rest <= room
       then stays <> rest
       else stays <> scrollText room (floor (s.now - titleSince s)) rest

-- | When the title began to show its subject.
titleSince :: AppState -> Double
titleSince s = maybe s.now snd s.titleShown

-- | Whether the focused screen's title scrolls, for which the header is drawn
-- again each second.
titleScrolls :: AppEnv -> AppState -> Bool
titleScrolls env s =
  let (stays, rest) = screenTitle env s (focusedView s).screen
  in textWidth rest > titleRoom s stays

-- | The columns of a title's part that scrolls, with a space before the
-- volume.
titleRoom :: AppState -> T.Text -> Int
titleRoom s stays = max 0 (fst s.terminalSize - textWidth stays - textWidth (headerRight s) - 1)

-- | The right of the header's first line: the volume, or the state of the
-- connection.
headerRight :: AppState -> T.Text
headerRight s = case s.connection of
  Connecting -> "Connecting…"
  Disconnected _ -> "Disconnected"
  Connected _ -> case s.mirror.status >>= (.volume) of
    Just v -> "Volume: " <> T.pack (show v) <> "%"
    Nothing -> "Volume: n/a"

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
          [ map (Just i,) (wrapText width text)
          | (i, (_, text)) <- zip [0 ..] timed.entries
          ]
      Nothing -> untimed (T.lines lyrics.plain)
    LyricsInstrumental -> untimed ["Instrumental"]
    -- The fetchers' failures name them.
    LyricsMissing [] -> untimed ["No lyrics stored"]
    LyricsMissing asked ->
      untimed $
        [ "No lyrics found on " <> alternatives notThere
        | let notThere = [f | (f, Nothing) <- asked]
        , not (null notThere)
        ]
          <> [reason | (_, Just reason) <- asked]
    LyricsFailed reason -> untimed [reason]
  where
    untimed :: [T.Text] -> [(Maybe Int, T.Text)]
    untimed = concatMap (map (Nothing,) . wrapText width)

    -- E.g. @A, B or C@.
    alternatives :: [T.Text] -> T.Text
    alternatives names = case reverse names of
      lastName : rest@(_ : _) -> T.intercalate ", " (reverse rest) <> " or " <> lastName
      _ -> T.concat names

-- | The index of the timed line of the lyrics screen that is being sung:
-- the last one whose time came, in the song that plays.
sungLine :: AppState -> Maybe Int
sungLine s = do
  (timed, elapsed) <- playingTimedLyrics s
  let sung = takeWhile ((<= elapsed) . fst) timed.entries
  guard . not $ null sung
  pure $ length sung - 1

-- | The first row that the lyrics screen shows: while it follows the song,
-- the one that keeps the line being sung in the middle, else the view's.
lyricsOffset :: AppState -> View -> Int
lyricsOffset s v = fromMaybe v.offset $ do
  guard s.lyrics.following
  line <- sungLine s
  let rows = lyricsRows v.width s.lyrics
  row <- L.findIndex ((== Just line) . fst) rows
  pure . max 0 $ min (length rows - v.height) (row - v.height `div` 2)

-- | When the next timed line of the lyrics screen is sung, for a redraw.
nextLyricsLine :: AppState -> Maybe Double
nextLyricsLine s = do
  guard $ (focusedView s).screen == LyricsScreen && isNothing s.seek
  st <- s.mirror.status
  guard $ st.state == Playing
  (timed, elapsed) <- playingTimedLyrics s
  next <- L.find (> elapsed) (map fst timed.entries)
  pure $ s.now + realToFrac (next - elapsed)

-- | The timed lyrics of the lyrics screen, if its song is the one that
-- plays, with the elapsed time of the song.
playingTimedLyrics :: AppState -> Maybe (TimedLyrics, Seconds)
playingTimedLyrics s = do
  song <- s.lyrics.song
  playing <- currentSong s.mirror
  guard $ sameSong playing song
  ShowingLyrics (LyricsFound _ lyrics) <- Just s.lyrics.status
  timed <- lyrics.timed
  elapsed <- displayedElapsed s
  pure (timed, elapsed)

-- | The rows of the song info screen at a width: a label and a value, which
-- the song can be without.
songInfoRows :: AppEnv -> AppState -> Int -> [(T.Text, Maybe T.Text)]
songInfoRows env s width = case s.songInfo.song of
  Nothing -> []
  Just song ->
    infoRows width $
      songInfoLines env.config.lists.tagSeparator (bitrateOf song) s.songInfo.comments song
  where
    -- MPD has the bitrate of the song that plays only.
    bitrateOf :: Song -> Maybe Int
    bitrateOf song = do
      playing <- currentSong s.mirror
      guard $ sameSong playing song
      s.mirror.status >>= (.bitrate)
