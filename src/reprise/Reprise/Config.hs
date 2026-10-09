{-# LANGUAGE DerivingVia #-}

-- | The configuration file, its decoders and its defaults.
module Reprise.Config
  ( -- * Configuration
    Config (..)
  , MpdConfig (..)
  , Port (..)
  , parsePort
  , Timeout (..)
  , Duration (..)
  , SongsConfig (..)
  , RowFormat (..)
  , ColumnsConfig (..)
  , Column (..)
  , ColumnWidth (..)
  , Align (..)
  , ListsConfig (..)
  , Display (..)
  , displayName
  , QueueConfig (..)
  , BrowserConfig (..)
  , BrowserSort (..)
  , SortBy (..)
  , sortByName
  , SearchEngineConfig (..)
  , StatusBarConfig (..)
  , ProgressBarConfig (..)
  , ProgressChars (..)
  , VisualizerConfig (..)
  , LyricsConfig (..)
  , LyricsFetcher (..)
  , EditorConfig (..)
  , MouseConfig (..)
  , ScrollLines (..)
  , VolumeStep (..)
  , Visualization (..)
  , visualizationName
  , FrameRate (..)
  , StylesConfig (..)
  , TextStyles (..)
  , ListStyles (..)
  , HeaderStyles (..)
  , StatusBarStyles (..)
  , ProgressBarStyles (..)
  , KeysConfig (..)
  , defaultConfig

    -- * Loading
  , decodeConfig
  , loadConfig
  , loadUsualConfig

    -- * Keymaps
  , keymapsOf
  , defaultKeymaps
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import Data.Char hiding (Space)
import Data.Functor
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Void
import Network.Socket qualified as N
import Optics.Core hiding (view)
import System.IO.Error
import Yamlet

import Reprise.Action
import Reprise.Format
import Reprise.Keymap
import Reprise.Keys
import Reprise.Mpd.Protocol.Response qualified as Response
import Reprise.Mpd.Protocol.Types
import Reprise.Number
import Reprise.Style
import Reprise.Width

----------------------------------------
-- Configuration

data Config = Config
  { mpd :: MpdConfig
  , startupScreen :: ScreenName
  , windowTitle :: Maybe (Format Void)
  -- ^ 'Nothing' leaves the window title alone.
  , songs :: SongsConfig
  , lists :: ListsConfig
  , queue :: QueueConfig
  , browser :: BrowserConfig
  , searchEngine :: SearchEngineConfig
  , statusBar :: StatusBarConfig
  , progressBar :: ProgressBarConfig
  , visualizer :: VisualizerConfig
  , lyrics :: LyricsConfig
  , editor :: EditorConfig
  , mouse :: MouseConfig
  , styles :: StylesConfig
  , keys :: KeysConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml Config

data MpdConfig = MpdConfig
  { host :: Maybe T.Text
  -- ^ A host name, or the path of a unix socket.
  , port :: Maybe Port
  , password :: Maybe T.Text
  , timeout :: Timeout
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml MpdConfig

-- | How long MPD has to answer, which is longer than 0.
newtype Timeout = Timeout Duration
  deriving newtype (Eq, Ord, Show, Num, Fractional)

-- | A TCP port, from 1.
newtype Port = Port N.PortNumber
  deriving newtype (Eq, Show)

-- | A port from the command line, the config or the environment.
parsePort :: T.Text -> Either T.Text Port
parsePort t =
  maybe (Left $ "a port is from 1 to " <> T.pack (show highest) <> ", not " <> t) Right $
    Port . fromIntegral <$> decimalIn 1 (fromIntegral highest) t
  where
    highest :: N.PortNumber
    highest = maxBound

-- | A duration, written with a unit, e.g. @5s@ or @500ms@.
newtype Duration = Duration Seconds
  deriving newtype (Eq, Ord, Show, Num, Fractional)

data SongsConfig = SongsConfig
  { missingTag :: T.Text
  -- ^ What a tag without a value shows, in every format.
  , tagSeparator :: T.Text
  -- ^ Between the values of a tag, in every format.
  , classic :: RowFormat
  , columns :: ColumnsConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml SongsConfig

-- | A list row with a part on the left and a part aligned to the right.
data RowFormat = RowFormat
  { left :: Format Style
  , right :: Format Style
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml RowFormat

data ColumnsConfig = ColumnsConfig
  { showTitles :: Bool
  , list :: [Column]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml ColumnsConfig

data Column = Column
  { width :: ~ColumnWidth
  , style :: Style
  , format :: ~(Format Style)
  , title :: T.Text
  , align :: Align
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml Column

data ColumnWidth
  = FixedWidth Int
  | -- | A percentage of the width of the list.
    RelativeWidth Int
  deriving stock (Eq, Show)

data Align = AlignLeft | AlignRight
  deriving stock (Eq, Show)

data ListsConfig = ListsConfig
  { keepCursorCentered :: Bool
  , ignoreLeadingThe :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml ListsConfig

data Display = Classic | Columns
  deriving stock (Eq, Show, Enum, Bounded)

-- | The name of a display in the config.
displayName :: Display -> T.Text
displayName = \case
  Classic -> "classic"
  Columns -> "columns"

data QueueConfig = QueueConfig
  { display :: Display
  , albumSeparators :: Bool
  , followPlaying :: Bool
  , showRemainingTime :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml QueueConfig

data BrowserConfig = BrowserConfig
  { display :: Display
  , sort :: BrowserSort
  , playlistPrefix :: Format Style
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml BrowserConfig

data BrowserSort = BrowserSort
  { by :: SortBy
  , format :: Format Void
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml BrowserSort

-- | In the order that @next_sort_mode@ goes through, as in ncmpcpp.
data SortBy = SortByType | SortByName | SortByMtime | SortByFormat | SortByNone
  deriving stock (Eq, Show, Enum, Bounded)

-- | The name of a sort mode in the config.
sortByName :: SortBy -> T.Text
sortByName = \case
  SortByType -> "type"
  SortByName -> "name"
  SortByMtime -> "mtime"
  SortByFormat -> "format"
  SortByNone -> "none"

newtype SearchEngineConfig = SearchEngineConfig
  { display :: Display
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml SearchEngineConfig

data StatusBarConfig = StatusBarConfig
  { song :: Format Style
  , showRemainingTime :: Bool
  , showBitrate :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml StatusBarConfig

newtype ProgressBarConfig = ProgressBarConfig
  { chars :: ProgressChars
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml ProgressBarConfig

-- | The characters of the elapsed part, the current position and the
-- remaining part.
data ProgressChars = ProgressChars
  { elapsed :: Char
  , current :: Char
  , remaining :: Char
  }
  deriving stock (Eq, Show)

data VisualizerConfig = VisualizerConfig
  { dataSource :: Maybe FilePath
  -- ^ The fifo of MPD's fifo output, which must be in the format
  -- @44100:16:2@. A leading @~/@ is the home directory.
  , visualization :: Visualization
  -- ^ The one that the visualizer shows first.
  , fps :: FrameRate
  , trail :: Duration
  -- ^ How long the samples of a frame of the ellipse stay on the screen.
  , colors :: NE.NonEmpty Style
  -- ^ The stops of a gradient from quiet to loud: from the center of the
  -- ellipse to its edges, from the middle of a channel's wave to its edges,
  -- and from the foot of a bar of the spectrum to its top.
  , debug :: Bool
  -- ^ Show at the top how many frames a second the screen draws, and what
  -- happened to the frames of the worker that reads the samples.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml VisualizerConfig

data LyricsConfig = LyricsConfig
  { directory :: Maybe FilePath
  -- ^ Where the lyrics are stored, @$XDG_DATA_HOME/reprise/lyrics@ without
  -- it. A leading @~/@ is the home directory. ncmpcpp's is @~/.lyrics@.
  , fetchers :: [LyricsFetcher]
  -- ^ Where the lyrics that aren't stored are fetched from, in order. None
  -- shows only the stored ones.
  , fetchInBackground :: Bool
  -- ^ Fetch the lyrics of each song that plays, so that they are stored
  -- when the lyrics screen shows them.
  , followPlaying :: Bool
  -- ^ Show the lyrics of the next song that plays on the lyrics screen.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml LyricsConfig

newtype EditorConfig = EditorConfig
  { command :: Maybe T.Text
  -- ^ The command that edits a file, e.g. @mcedit@, which @sh@ runs with the
  -- file after it. Without it, @$VISUAL@, else @$EDITOR@.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml EditorConfig

data MouseConfig = MouseConfig
  { scrollLines :: ScrollLines
  -- ^ How far a step of the wheel moves the cursor of a list, or scrolls
  -- text.
  , volumeStep :: VolumeStep
  -- ^ How much a step of the wheel over the volume changes it.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml MouseConfig

-- | Lines, at least one.
newtype ScrollLines = ScrollLines Int
  deriving newtype (Eq, Show)

-- | Percents of the volume, from 1 to 'maxVolume'.
newtype VolumeStep = VolumeStep Int
  deriving newtype (Eq, Show)

data LyricsFetcher
  = -- | lrclib.net.
    Lrclib
  | -- | tekstowo.pl.
    Tekstowo
  deriving stock (Eq, Show, Enum, Bounded)

lyricsFetcherName :: LyricsFetcher -> T.Text
lyricsFetcherName = \case
  Lrclib -> "lrclib"
  Tekstowo -> "tekstowo"

data Visualization
  = -- | The levels of the frequencies, as bars.
    Spectrum
  | -- | The left channel across and the right one up.
    Ellipse
  | -- | The samples of each channel over time.
    Wave
  deriving stock (Eq, Show, Enum, Bounded)

visualizationName :: Visualization -> T.Text
visualizationName = \case
  Spectrum -> "spectrum"
  Ellipse -> "ellipse"
  Wave -> "wave"

-- | Frames per second, at least one.
newtype FrameRate = FrameRate Int
  deriving newtype (Eq, Show)

-- | The styles of every part of the screen, which make a theme.
data StylesConfig = StylesConfig
  { label :: Style
  -- ^ Names of fields, e.g. in the song info and the help.
  , value :: Style
  -- ^ Values of fields, e.g. the keys in the which-key panel.
  , missingTag :: Maybe Style
  -- ^ The marker of a tag without a value. Without it, the marker has the
  -- style around it. Not in the columns display, where the marker has the
  -- column's style. The song info screen shows its empty fields with it,
  -- and the search engine's form will, as ncmpcpp does.
  , popupBorder :: Style
  , text :: TextStyles
  , list :: ListStyles
  , header :: HeaderStyles
  , statusBar :: StatusBarStyles
  , progressBar :: ProgressBarStyles
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml StylesConfig

-- | The styles of the screens of text: the help, the lyrics and the song
-- info.
data TextStyles = TextStyles
  { normal :: Style
  -- ^ The lyrics and the descriptions of the help.
  , found :: Style
  -- ^ What a find matches.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml TextStyles

-- | The styles of every list. The states of an item lay their styles over
-- 'normal'.
data ListStyles = ListStyles
  { normal :: Style
  , cursor :: Style
  , inactiveCursor :: Style
  , selected :: Style
  , found :: Style
  -- ^ The items that match an unfinished find.
  , playing :: Style
  , queued :: Style
  -- ^ The songs that are in the queue, in the other screens. ncmpcpp
  -- always makes them bold.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml ListStyles

data HeaderStyles = HeaderStyles
  { normal :: Style
  , title :: Style
  -- ^ The title of the screen, laid over 'normal'.
  , volume :: Style
  , flags :: Style
  , line :: Style
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml HeaderStyles

data StatusBarStyles = StatusBarStyles
  { normal :: Style
  , state :: Style
  , time :: Style
  , error :: Style
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml StatusBarStyles

data ProgressBarStyles = ProgressBarStyles
  { normal :: Style
  , elapsed :: Style
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml ProgressBarStyles

-- | The user's changes to the default keymaps.
data KeysConfig = KeysConfig
  { global :: KeymapOverride
  , screens :: M.Map ScreenName KeymapOverride
  -- ^ Of the screens whose keymaps change.
  }
  deriving stock (Eq, Show, Generic)

----------------------------------------
-- Defaults

-- | The author's ncmpcpp setup.
defaultConfig :: Config
defaultConfig =
  Config
    { mpd = defaultMpd
    , startupScreen = QueueScreen
    , windowTitle = Just $ plainFormat "[%{artist} - ][%{title}|%{filename}]"
    , songs = defaultSongs
    , lists = defaultLists
    , queue = defaultQueue
    , browser = defaultBrowser
    , searchEngine = defaultSearchEngine
    , statusBar = defaultStatusBar
    , progressBar = defaultProgressBar
    , visualizer = defaultVisualizer
    , lyrics = defaultLyrics
    , editor = defaultEditor
    , mouse = defaultMouse
    , styles = defaultStyles
    , keys = defaultKeys
    }

defaultMpd :: MpdConfig
defaultMpd = MpdConfig {host = Nothing, port = Nothing, password = Nothing, timeout = 5}

defaultSongs :: SongsConfig
defaultSongs =
  SongsConfig
    { missingTag = "—"
    , tagSeparator = " | "
    , classic = defaultRowFormat
    , columns = defaultColumns
    }

defaultRowFormat :: RowFormat
defaultRowFormat =
  RowFormat
    { left = styledFormat "[%{artist} - ][%{title}|<white>%{filename}</>]"
    , right = styledFormat "<green>[%{length}|-:--]</>"
    }

defaultColumns :: ColumnsConfig
defaultColumns =
  ColumnsConfig
    { showTitles = False
    , list =
        [ column (RelativeWidth 20) "221" "%{artist}" "" AlignLeft
        , column (FixedWidth 6) "77" "[%{track_raw}]" "" AlignLeft
        , column (RelativeWidth 50) "white" "[%{title}|%{filename}]" "Title" AlignLeft
        , column (RelativeWidth 20) "75" "%{album}" "" AlignLeft
        , column (FixedWidth 5) "203" "[%{length}|-:--]" "" AlignRight
        ]
    }
  where
    column :: ColumnWidth -> T.Text -> T.Text -> T.Text -> Align -> Column
    column w s f t a = Column {width = w, style = style s, format = styledFormat f, title = t, align = a}

-- | A column without a width or a format is an error.
defaultColumn :: Column
defaultColumn =
  Column
    { width = requiredField
    , style = mempty
    , format = requiredField
    , title = ""
    , align = AlignLeft
    }

defaultLists :: ListsConfig
defaultLists =
  ListsConfig
    { keepCursorCentered = False
    , ignoreLeadingThe = False
    }

defaultQueue :: QueueConfig
defaultQueue =
  QueueConfig
    { display = Columns
    , albumSeparators = False
    , followPlaying = False
    , showRemainingTime = False
    }

defaultBrowser :: BrowserConfig
defaultBrowser =
  BrowserConfig
    { display = Classic
    , sort = defaultBrowserSort
    , playlistPrefix = styledFormat "<red>playlist</> "
    }

defaultBrowserSort :: BrowserSort
defaultBrowserSort = BrowserSort {by = SortByType, format = plainFormat "%{artist} - %{title}"}

defaultSearchEngine :: SearchEngineConfig
defaultSearchEngine = SearchEngineConfig {display = Classic}

defaultStatusBar :: StatusBarConfig
defaultStatusBar =
  StatusBarConfig
    { song = styledFormat "[[%{artist}[ \"%{album}\"[ (%{year})]] - ]%{title}|%{filename}]"
    , showRemainingTime = False
    , showBitrate = False
    }

defaultProgressBar :: ProgressBarConfig
defaultProgressBar =
  ProgressBarConfig
    { chars = ProgressChars '▅' '▅' '▅'
    }

defaultVisualizer :: VisualizerConfig
defaultVisualizer =
  VisualizerConfig
    { dataSource = Nothing
    , visualization = Spectrum
    , fps = FrameRate 60
    , trail = 0.15
    , -- Bold makes the thin braille dots of the ellipse and the wave easier
      -- to see.
      colors =
        style . (<> " bold")
          <$> "46"
            NE.:| ["82", "118", "154", "190", "226", "220", "214", "208", "202", "196", "160"]
    , debug = False
    }

defaultEditor :: EditorConfig
defaultEditor = EditorConfig {command = Nothing}

-- | The author's ncmpcpp sets @lines_scrolled@ to 4, and leaves
-- @volume_change_step@ at 2, which the volume keys take too.
defaultMouse :: MouseConfig
defaultMouse = MouseConfig {scrollLines = ScrollLines 4, volumeStep = VolumeStep 2}

defaultLyrics :: LyricsConfig
defaultLyrics =
  LyricsConfig
    { directory = Nothing
    , fetchers = [Lrclib, Tekstowo]
    , fetchInBackground = False
    , followPlaying = False
    }

defaultStyles :: StylesConfig
defaultStyles =
  StylesConfig
    { label = style "white"
    , value = style "green"
    , missingTag = Nothing
    , popupBorder = style "green"
    , text = defaultTextStyles
    , list = defaultListStyles
    , header = defaultHeaderStyles
    , statusBar = defaultStatusBarStyles
    , progressBar = defaultProgressBarStyles
    }

defaultTextStyles :: TextStyles
defaultTextStyles =
  TextStyles
    { -- As ncmpcpp's main_window_color.
      normal = style "yellow"
    , -- As ncmpcpp's find in text. Text has no cursor, whose reverse it
      -- would look like in a list.
      found = style "reverse"
    }

defaultListStyles :: ListStyles
defaultListStyles =
  ListStyles
    { normal = style "yellow"
    , cursor = style "yellow reverse"
    , inactiveCursor = style "yellow on 237"
    , selected = style "yellow on 24"
    , found = style "underline"
    , playing = style "bold"
    , queued = style "bold"
    }

defaultHeaderStyles :: HeaderStyles
defaultHeaderStyles =
  HeaderStyles
    { normal = style "default"
    , title = style "bold"
    , volume = style "default"
    , flags = style "bold"
    , line = style "default"
    }

defaultStatusBarStyles :: StatusBarStyles
defaultStatusBarStyles =
  StatusBarStyles
    { normal = style "default"
    , state = style "bold"
    , time = style "bold"
    , error = style "9"
    }

defaultProgressBarStyles :: ProgressBarStyles
defaultProgressBarStyles = ProgressBarStyles {normal = style "236", elapsed = style "28"}

defaultKeys :: KeysConfig
defaultKeys =
  KeysConfig {global = noOverride, screens = M.empty}

-- | No changes to a keymap.
noOverride :: KeymapOverride
noOverride = KeymapOverride Nothing M.empty

-- The defaults are literals that the tests decode, so an error here is a
-- bug in the defaults.
style :: T.Text -> Style
style = either (error . T.unpack) id . parseStyle

styledFormat :: T.Text -> Format Style
styledFormat = either (error . show) id . parseStyledFormat

plainFormat :: T.Text -> Format Void
plainFormat = either (error . show) id . parsePlainFormat

----------------------------------------
-- Generic options

options :: YamlOptions
options =
  defaultYamlOptions
    & #fieldLabelModifier .~ snakeCase
    & #rejectUnknownFields .~ True

instance GenericYamlOptions Config where
  yamlOptions = options
  yamlDefault = Just defaultConfig

instance GenericYamlOptions MpdConfig where
  yamlOptions = options
  yamlDefault = Just defaultMpd

instance GenericYamlOptions SongsConfig where
  yamlOptions = options
  yamlDefault = Just defaultSongs

instance GenericYamlOptions RowFormat where
  yamlOptions = options
  yamlDefault = Just defaultRowFormat

instance GenericYamlOptions ColumnsConfig where
  yamlOptions = options
  yamlDefault = Just defaultColumns

instance GenericYamlOptions Column where
  yamlOptions = options
  yamlDefault = Just defaultColumn

instance GenericYamlOptions ListsConfig where
  yamlOptions = options
  yamlDefault = Just defaultLists

instance GenericYamlOptions QueueConfig where
  yamlOptions = options
  yamlDefault = Just defaultQueue

instance GenericYamlOptions BrowserConfig where
  yamlOptions = options
  yamlDefault = Just defaultBrowser

instance GenericYamlOptions BrowserSort where
  yamlOptions = options
  yamlDefault = Just defaultBrowserSort

instance GenericYamlOptions SearchEngineConfig where
  yamlOptions = options
  yamlDefault = Just defaultSearchEngine

instance GenericYamlOptions TextStyles where
  yamlOptions = options
  yamlDefault = Just defaultTextStyles

instance GenericYamlOptions ListStyles where
  yamlOptions = options
  yamlDefault = Just defaultListStyles

instance GenericYamlOptions HeaderStyles where
  yamlOptions = options
  yamlDefault = Just defaultHeaderStyles

instance GenericYamlOptions StatusBarStyles where
  yamlOptions = options
  yamlDefault = Just defaultStatusBarStyles

instance GenericYamlOptions ProgressBarStyles where
  yamlOptions = options
  yamlDefault = Just defaultProgressBarStyles

instance GenericYamlOptions StatusBarConfig where
  yamlOptions = options
  yamlDefault = Just defaultStatusBar

instance GenericYamlOptions ProgressBarConfig where
  yamlOptions = options
  yamlDefault = Just defaultProgressBar

instance GenericYamlOptions VisualizerConfig where
  yamlOptions = options
  yamlDefault = Just defaultVisualizer

instance GenericYamlOptions EditorConfig where
  yamlOptions = options
  yamlDefault = Just defaultEditor

instance GenericYamlOptions LyricsConfig where
  yamlOptions = options
  yamlDefault = Just defaultLyrics

instance GenericYamlOptions MouseConfig where
  yamlOptions = options
  yamlDefault = Just defaultMouse

instance GenericYamlOptions StylesConfig where
  yamlOptions = options
  yamlDefault = Just defaultStyles

-- | A mapping of @global@ and the names of the screens to their changes.
instance FromYaml KeysConfig where
  parseYaml = withMapping $ \o ->
    rejectUnknownKeys ("global" : map screenName screenNames) o
      *> ( KeysConfig
             <$> (fromMaybe noOverride <$> parseFieldIfPresent o "global")
             <*> ( M.fromList . catMaybes
                     <$> traverse (\s -> fmap (s,) <$> parseFieldIfPresent o (screenName s)) screenNames
                 )
         )

----------------------------------------
-- Decoders of the small languages

instance FromYaml Duration where
  parseYaml n = withText (either (failAt n . T.unpack) (pure . Duration) . parseDuration) n
    where
      parseDuration :: T.Text -> Either T.Text Seconds
      parseDuration t
        | Just ms <- T.stripSuffix "ms" t, Just v <- number ms = Right (v / 1000)
        | Just s <- T.stripSuffix "s" t, Just v <- number s = Right v
        | otherwise = Left "expected a duration with a unit, e.g. 5s or 500ms"

      number :: T.Text -> Maybe Seconds
      number = Response.readSeconds . T.encodeUtf8

instance FromYaml Timeout where
  parseYaml n = do
    d <- parseYaml n
    if d > 0 then pure (Timeout d) else failAt n "a timeout must be longer than 0"

instance FromYaml Port where
  parseYaml n = case view n of
    IntView i -> either (failAt n . T.unpack) pure . parsePort . T.pack $ show i
    _ -> typeMismatch "a port, e.g. 6600" n

instance FromYaml ColumnWidth where
  parseYaml n = case view n of
    IntView i
      | i >= 1 && i <= toInteger (maxBound @Int) -> pure . FixedWidth $ fromInteger i
      | otherwise -> failAt n "a width must be at least 1"
    StringView t
      | Just p <- T.stripSuffix "%" t
      , Just v <- decimalIn 1 100 p ->
          pure $ RelativeWidth v
    _ -> failAt n "expected a number of columns, e.g. 6, or a percentage, e.g. 20%"

instance FromYaml Align where
  parseYaml = oneOf [("left", AlignLeft), ("right", AlignRight)]

instance FromYaml Display where
  parseYaml = oneOf [(displayName d, d) | d <- [minBound .. maxBound]]

instance FromYaml Visualization where
  parseYaml = oneOf [(visualizationName v, v) | v <- [minBound .. maxBound]]

instance FromYaml LyricsFetcher where
  parseYaml = oneOf [(lyricsFetcherName f, f) | f <- [minBound .. maxBound]]

instance FromYaml SortBy where
  parseYaml = oneOf [(sortByName by, by) | by <- [minBound .. maxBound]]

instance FromYaml FrameRate where
  parseYaml n = do
    rate <- parseYaml @Int n
    if rate >= 1
      then pure (FrameRate rate)
      else failAt n "expected at least 1 frame per second"

instance FromYaml ScrollLines where
  parseYaml n = do
    lines' <- parseYaml @Int n
    if lines' >= 1
      then pure (ScrollLines lines')
      else failAt n "expected at least 1 line"

instance FromYaml VolumeStep where
  parseYaml n = do
    step <- parseYaml @Int n
    if step >= 1 && step <= maxVolume
      then pure (VolumeStep step)
      else failAt n $ "expected a step from 1 to " <> show maxVolume

instance FromYaml ProgressChars where
  parseYaml n = withText chars n
    where
      chars :: T.Text -> Parser ProgressChars
      chars t = case T.unpack t of
        [e, c, r]
          -- The bar fills a cell with each, and the terminal would draw a
          -- control character, e.g. a tab, as something else.
          | all (\ch -> not (isControl ch) && textWidth (T.singleton ch) == 1) [e, c, r] ->
              pure $ ProgressChars e c r
          | otherwise -> failAt n "each character must take one column, e.g. = > -"
        _ ->
          failAt
            n
            "expected 3 characters: the elapsed part, the current position and the remaining part"

----------------------------------------
-- Loading

-- | Decode a configuration file's contents. The file name is for the
-- errors.
decodeConfig :: FilePath -> BS.ByteString -> Either [String] Config
decodeConfig file input = case Yamlet.decode input of
  Right (ConfigFile config) -> Right config
  Left errs -> Left . map (prettyError file) $ NE.toList errs

-- | A file without content, e.g. only comments, is null and gives the
-- defaults.
newtype ConfigFile = ConfigFile Config

instance FromYaml ConfigFile where
  parseYaml n = case view n of
    NullView -> pure $ ConfigFile defaultConfig
    _ -> ConfigFile <$> parseYaml n

-- | Load a configuration file that the user named, e.g. with @--config@. A
-- file that can't be read is an error, also a missing one, as the user
-- meant one.
loadConfig :: FilePath -> IO (Either [String] Config)
loadConfig file = either (Left . pure . readError) (decodeConfig file) <$> try (BS.readFile file)

-- | Load the usual configuration file, which gives the defaults if it is
-- missing.
loadUsualConfig :: FilePath -> IO (Either [String] Config)
loadUsualConfig file =
  try (BS.readFile file) <&> \case
    Right bytes -> decodeConfig file bytes
    Left err
      | isDoesNotExistError err -> Right defaultConfig
      | otherwise -> Left [readError err]

readError :: IOException -> String
readError err = "The config can't be read: " <> displayException err

----------------------------------------
-- Keymaps

-- | The keymaps of a configuration: the defaults with the user's changes.
keymapsOf :: KeysConfig -> Keymaps
keymapsOf user =
  Keymaps
    { global = applyOverride user.global defaultKeymaps.global
    , screens =
        M.fromList
          [ ( screen
            , applyOverride
                (M.findWithDefault noOverride screen user.screens)
                (screenKeymap screen defaultKeymaps)
            )
          | screen <- screenNames
          ]
    }

-- | The keymaps without the user's changes.
defaultKeymaps :: Keymaps
defaultKeymaps =
  Keymaps
    { global =
        keymap
          [ plain ArrowUp ~> Move MoveUp
          , plain ArrowDown ~> Move MoveDown
          , plain PageUp ~> Move MovePageUp
          , plain PageDown ~> Move MovePageDown
          , plain Home ~> Move MoveFirst
          , plain End ~> Move MoveLast
          , char '[' ~> Move MovePreviousAlbum
          , char ']' ~> Move MoveNextAlbum
          , char '{' ~> Move MovePreviousArtist
          , char '}' ~> Move MoveNextArtist
          , shift ArrowUp ~> Select (SelectItem (Just MoveUp))
          , shift ArrowDown ~> Select (SelectItem (Just MoveDown))
          , plain InsertKey ~> Select (SelectItem Nothing)
          , char 'V' ~> Select SelectNone
          , -- Verbs: each screen implements them its own way.
            plain Enter ~> Activate
          , plain Space ~> AddOrRemove
          , plain DeleteKey ~> Delete
          , char 'p' ~> Pause
          , char 's' ~> Stop
          , char '<' ~> Previous
          , char '>' ~> Next
          , -- The browser's goes to the parent directory instead, as in
            -- ncmpcpp.
            plain Backspace ~> Replay
          , char 'f' ~> Seek (SeekBy 1)
          , char 'b' ~> Seek (SeekBy (-1))
          , char '+' ~> Volume (VolumeBy 2)
          , char '-' ~> Volume (VolumeBy (-2))
          , plain ArrowRight ~> Volume (VolumeBy 2)
          , plain ArrowLeft ~> Volume (VolumeBy (-2))
          , char '/' ~> Find FindForward
          , char '?' ~> Find FindBackward
          , char '.' ~> Find FindNext
          , char ',' ~> Find FindPrevious
          , -- 4, 5 and 6 are kept for the media library, the playlist editor
            -- and the tag editor.
            char '1' ~> Show QueueScreen
          , char '2' ~> Show BrowserScreen
          , char '3' ~> Show SearchEngineScreen
          , char '7' ~> Show OutputsScreen
          , char '8' ~> Show VisualizerScreen
          , char 'l' ~> Show LyricsScreen
          , char 'i' ~> Show SongInfoScreen
          , plain Tab ~> NextScreen numberedScreens
          , shift Tab ~> PreviousScreen numberedScreens
          , plain (Function 1) ~> Show HelpScreen
          , char ':' ~> CommandPrompt ""
          , char 'q' ~> Quit
          , group
              (char 'a')
              "add"
              [ char 'e' ~> Add AddEnd
              , char 'n' ~> Add AddNext
              , char 'b' ~> Add AddBeginning
              , char 'p' ~> AddAndPlay
              , char '/' ~> CommandPrompt "add_path"
              ]
          , group
              (char 'e')
              "edit"
              [ char 'c' ~> Clear
              , char 's' ~> Shuffle
              , char 'w' ~> Save
              ]
          , group
              (char 'v')
              "selection"
              [ char 'r' ~> Select SelectRange
              , char 'i' ~> Select SelectInvert
              , char 'a' ~> Select SelectAlbum
              , char 'A' ~> Select SelectArtist
              , char 'f' ~> Select SelectFound
              ]
          , group
              (char 't')
              "toggle"
              [ char 'r' ~> Toggle ToggleRepeat
              , char 'z' ~> Toggle ToggleRandom
              , char 's' ~> Toggle ToggleSingle
              , char 'c' ~> Toggle ToggleConsume
              , char 'x' ~> Toggle (ToggleCrossfade 5)
              , char 'X' ~> CommandPrompt "crossfade"
              , char 'g' ~> Toggle ToggleReplayGain
              , char 'a' ~> Toggle ToggleAlbumSeparators
              , char 'b' ~> Toggle ToggleBitrate
              ]
          , group
              (char 'g')
              "go"
              [ char 'b' ~> JumpToBrowser
              , -- ncmpcpp's g.
                char 's' ~> CommandPrompt "seek"
              ]
          , -- As ncmpcpp's u.
            char 'u' ~> Update UpdateCurrent
          , char 'U' ~> Update UpdateAll
          ]
    , screens =
        M.fromList
          [
            ( QueueScreen
            , keymap
                [ -- As in the author's ncmpcpp bindings.
                  plain Space ~> Select (SelectItem (Just MoveDown))
                , char 'o' ~> JumpToPlaying
                , char 'm' ~> MoveSongs MoveSongsUp
                , char 'n' ~> MoveSongs MoveSongsDown
                , char 'M' ~> MoveSongs MoveSongsToCursor
                , group
                    (char 'e')
                    "edit"
                    [ group
                        (char 'm')
                        "move"
                        [ char 'e' ~> MoveSongs MoveSongsToEnd
                        , char 'b' ~> MoveSongs MoveSongsToBeginning
                        , char 'n' ~> MoveSongs MoveSongsToNext
                        ]
                    , char 'p' ~> CommandPrompt "priority"
                    ]
                , group
                    (char 't')
                    "toggle"
                    [char 'd' ~> Toggle ToggleDisplay, char 'f' ~> Toggle ToggleFollowPlaying]
                ]
            )
          ,
            ( BrowserScreen
            , keymap
                [ plain Backspace ~> Parent
                , char 'o' ~> JumpToPlaying
                , group (char 't') "toggle" [char 'd' ~> Toggle ToggleDisplay, char 'o' ~> NextSortMode]
                ]
            )
          , -- As in ncmpcpp.
            (VisualizerScreen, keymap [plain Space ~> Toggle ToggleVisualization])
          , -- As in ncmpcpp.

            ( LyricsScreen
            , keymap
                [ char '`' ~> RefetchLyrics
                , group (char 'e') "edit" [char 'e' ~> EditLyrics]
                , group (char 't') "toggle" [char 'f' ~> Toggle ToggleFollowPlaying]
                , plain Space ~> Toggle ToggleFollowPlaying
                , char 'o' ~> JumpToPlaying
                , -- As the key that showed them, as in ncmpcpp.
                  char 'l' ~> Back
                , plain Escape ~> Back
                ]
            )
          , (SongInfoScreen, keymap [char 'i' ~> Back, plain Escape ~> Back])
          , (HelpScreen, keymap [plain (Function 1) ~> Back, plain Escape ~> Back])
          ]
    }
  where
    keymap :: [(KeySpec, Binding)] -> Keymap
    keymap = Keymap Nothing . bindings

    group :: KeySpec -> T.Text -> [(KeySpec, Binding)] -> (KeySpec, Binding)
    group k name bs = (k, BindPrefix (Keymap (Just name) (bindings bs)))

    (~>) :: KeySpec -> Action -> (KeySpec, Binding)
    k ~> a = (k, BindAction a)

    -- A key bound twice is a mistake in the list, which every test that
    -- uses the default keymaps finds.
    bindings :: [(KeySpec, Binding)] -> M.Map KeySpec Binding
    bindings = M.fromListWithKey $ \k _ _ ->
      error $ "the default keymaps bind " <> T.unpack (renderKeySpec k) <> " twice"
