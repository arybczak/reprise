{-# LANGUAGE DerivingVia #-}

-- | The configuration file, its decoders and its defaults.
module Reprise.Config
  ( -- * Configuration
    Config (..)
  , MpdConfig (..)
  , Duration (..)
  , SongsConfig (..)
  , RowFormat (..)
  , ColumnsConfig (..)
  , Column (..)
  , ColumnWidth (..)
  , Align (..)
  , ListsConfig (..)
  , Display (..)
  , QueueConfig (..)
  , BrowserConfig (..)
  , BrowserSort (..)
  , SortBy (..)
  , SearchEngineConfig (..)
  , HeaderConfig (..)
  , StatusBarConfig (..)
  , ProgressBarConfig (..)
  , ProgressChars (..)
  , StylesConfig (..)
  , KeysConfig (..)
  , defaultConfig

    -- * Loading
  , decodeConfig
  , loadConfig

    -- * Keymaps
  , defaultKeymapsYaml
  , defaultKeymapOverrides
  , keymapsOf
  ) where

import Data.ByteString qualified as BS
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Void
import MPD.Types
import Optics.Core hiding (view)
import System.Directory
import Yamlet

import Reprise.Action
import Reprise.Format
import Reprise.Keymap
import Reprise.Style

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
  , header :: HeaderConfig
  , statusBar :: StatusBarConfig
  , progressBar :: ProgressBarConfig
  , styles :: StylesConfig
  , keys :: KeysConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml Config

data MpdConfig = MpdConfig
  { host :: Maybe T.Text
  -- ^ A host name, or the path of a unix socket.
  , port :: Maybe Int
  , password :: Maybe T.Text
  , timeout :: Duration
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml MpdConfig

-- | A duration, written with a unit, e.g. @5s@ or @500ms@.
newtype Duration = Duration Seconds
  deriving newtype (Eq, Ord, Show, Num, Fractional)

data SongsConfig = SongsConfig
  { classic :: RowFormat
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
  { style :: Style
  , cursorStyle :: Style
  , inactiveCursorStyle :: Style
  , selectedStyle :: Style
  , playingStyle :: Style
  , keepCursorCentered :: Bool
  , ignoreLeadingThe :: Bool
  , missingTag :: T.Text
  , missingTagStyle :: Style
  -- ^ Not in the columns display, where the marker has the column's style.
  , tagSeparator :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml ListsConfig

data Display = Classic | Columns
  deriving stock (Eq, Show)

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

data SortBy = SortByType | SortByName | SortByMtime | SortByFormat | SortByNone
  deriving stock (Eq, Show)

newtype SearchEngineConfig = SearchEngineConfig
  { display :: Display
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml SearchEngineConfig

data HeaderConfig = HeaderConfig
  { style :: Style
  , titleStyle :: Style
  -- ^ The title of the screen, laid over 'style'.
  , volumeStyle :: Style
  , flagsStyle :: Style
  , lineStyle :: Style
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml HeaderConfig

data StatusBarConfig = StatusBarConfig
  { song :: Format Style
  , style :: Style
  , stateStyle :: Style
  , timeStyle :: Style
  , showRemainingTime :: Bool
  , showBitrate :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml StatusBarConfig

data ProgressBarConfig = ProgressBarConfig
  { chars :: ProgressChars
  , style :: Style
  , elapsedStyle :: Style
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

data StylesConfig = StylesConfig
  { label :: Style
  , value :: Style
  , popupBorder :: Style
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml StylesConfig

-- | The user's changes to the default keymaps.
data KeysConfig = KeysConfig
  { global :: KeymapOverride
  , queue :: KeymapOverride
  , browser :: KeymapOverride
  , searchEngine :: KeymapOverride
  , mediaLibrary :: KeymapOverride
  , playlistEditor :: KeymapOverride
  , outputs :: KeymapOverride
  , help :: KeymapOverride
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml KeysConfig

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
    , header = defaultHeader
    , statusBar = defaultStatusBar
    , progressBar = defaultProgressBar
    , styles = defaultStyles
    , keys = defaultKeys
    }

defaultMpd :: MpdConfig
defaultMpd = MpdConfig {host = Nothing, port = Nothing, password = Nothing, timeout = 5}

defaultSongs :: SongsConfig
defaultSongs =
  SongsConfig
    { classic = defaultRowFormat
    , columns = defaultColumns
    }

defaultRowFormat :: RowFormat
defaultRowFormat =
  RowFormat
    { left = styledFormat "[%{artist} - ][%{title}|<white>%{filename}</>]"
    , right = styledFormat "<green>%{length}</>"
    }

defaultColumns :: ColumnsConfig
defaultColumns =
  ColumnsConfig
    { showTitles = False
    , list =
        [ column (RelativeWidth 20) "221" "%{artist}" "" AlignLeft
        , column (FixedWidth 6) "77" "[%{track_raw}]" "" AlignLeft
        , column (RelativeWidth 50) "white" "[%{title}|%{filename}]" "Title" AlignLeft
        , column (RelativeWidth 20) "cyan" "%{album}" "" AlignLeft
        , column (FixedWidth 5) "203" "%{length}" "" AlignRight
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
    { style = style "yellow"
    , cursorStyle = style "yellow reverse"
    , inactiveCursorStyle = style "yellow on 237"
    , selectedStyle = style "yellow on 24"
    , playingStyle = style "bold"
    , keepCursorCentered = False
    , ignoreLeadingThe = False
    , missingTag = "<empty>"
    , missingTagStyle = style "cyan"
    , tagSeparator = " | "
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

defaultHeader :: HeaderConfig
defaultHeader =
  HeaderConfig
    { style = style "default"
    , titleStyle = style "bold"
    , volumeStyle = style "default"
    , flagsStyle = style "bold"
    , lineStyle = style "default"
    }

defaultStatusBar :: StatusBarConfig
defaultStatusBar =
  StatusBarConfig
    { song = styledFormat "[[%{artist}[ \"%{album}\"[ (%{year})]] - ]%{title}|%{filename}]"
    , style = style "default"
    , stateStyle = style "bold"
    , timeStyle = style "bold"
    , showRemainingTime = False
    , showBitrate = False
    }

defaultProgressBar :: ProgressBarConfig
defaultProgressBar =
  ProgressBarConfig
    { chars = ProgressChars '▅' '▅' '▅'
    , style = style "236"
    , elapsedStyle = style "28"
    }

defaultStyles :: StylesConfig
defaultStyles =
  StylesConfig
    { label = style "white"
    , value = style "green"
    , popupBorder = style "green"
    }

defaultKeys :: KeysConfig
defaultKeys =
  KeysConfig
    { global = noOverride
    , queue = noOverride
    , browser = noOverride
    , searchEngine = noOverride
    , mediaLibrary = noOverride
    , playlistEditor = noOverride
    , outputs = noOverride
    , help = noOverride
    }
  where
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

instance GenericYamlOptions HeaderConfig where
  yamlOptions = options
  yamlDefault = Just defaultHeader

instance GenericYamlOptions StatusBarConfig where
  yamlOptions = options
  yamlDefault = Just defaultStatusBar

instance GenericYamlOptions ProgressBarConfig where
  yamlOptions = options
  yamlDefault = Just defaultProgressBar

instance GenericYamlOptions StylesConfig where
  yamlOptions = options
  yamlDefault = Just defaultStyles

instance GenericYamlOptions KeysConfig where
  yamlOptions = options
  yamlDefault = Just defaultKeys

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
      number t = case reads @Double (T.unpack t) of
        [(v, "")] | T.all (\c -> isDigit c || c == '.') t, v >= 0 -> Just (realToFrac v)
        _ -> Nothing

instance FromYaml ColumnWidth where
  parseYaml n = case view n of
    IntView i
      | i >= 1 && i <= toInteger (maxBound @Int) -> pure . FixedWidth $ fromInteger i
      | otherwise -> failAt n "a width must be at least 1"
    StringView t
      | Just p <- T.stripSuffix "%" t
      , not (T.null p)
      , T.all isDigit p
      , [(v, "")] <- reads @Integer (T.unpack p)
      , v >= 1 && v <= 100 ->
          pure . RelativeWidth $ fromInteger v
    _ -> failAt n "expected a number of columns, e.g. 6, or a percentage, e.g. 20%"

instance FromYaml Align where
  parseYaml = oneOf [("left", AlignLeft), ("right", AlignRight)]

instance FromYaml Display where
  parseYaml = oneOf [("classic", Classic), ("columns", Columns)]

instance FromYaml SortBy where
  parseYaml =
    oneOf
      [ ("type", SortByType)
      , ("name", SortByName)
      , ("mtime", SortByMtime)
      , ("format", SortByFormat)
      , ("none", SortByNone)
      ]

instance FromYaml ProgressChars where
  parseYaml n = withText chars n
    where
      chars :: T.Text -> Parser ProgressChars
      chars t = case T.unpack t of
        [e, c, r] -> pure $ ProgressChars e c r
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

-- | Load the configuration file. A missing file gives the defaults.
loadConfig :: FilePath -> IO (Either [String] Config)
loadConfig file =
  doesFileExist file >>= \case
    False -> pure $ Right defaultConfig
    True -> decodeConfig file <$> BS.readFile file

----------------------------------------
-- Keymaps

-- | The default keymaps, in the format of the configuration file.
defaultKeymapsYaml :: T.Text
defaultKeymapsYaml =
  T.unlines
    [ "global:"
    , "  # moving around"
    , "  up: move up"
    , "  down: move down"
    , "  page_up: move page_up"
    , "  page_down: move page_down"
    , "  home: move first"
    , "  end: move last"
    , "  \"[\": move previous_album"
    , "  \"]\": move next_album"
    , "  \"{\": move previous_artist"
    , "  \"}\": move next_artist"
    , "  o: jump_to_playing"
    , ""
    , "  # selecting"
    , "  shift-up: select up"
    , "  shift-down: select down"
    , "  insert: select"
    , ""
    , "  # verbs; each screen implements them its own way"
    , "  enter: activate"
    , "  space: add_or_remove"
    , "  delete: delete"
    , ""
    , "  # playback"
    , "  p: pause"
    , "  s: stop"
    , "  \"<\": previous"
    , "  \">\": next"
    , "  f: seek +1s"
    , "  b: seek -1s"
    , "  \"+\": volume +2"
    , "  \"-\": volume -2"
    , "  right: volume +2"
    , "  left: volume -2"
    , ""
    , "  # find and filter"
    , "  /: find forward"
    , "  \"?\": find backward"
    , "  .: find next"
    , "  \",\": find previous"
    , "  ctrl-f: filter"
    , ""
    , "  # screens; 4 and 5 are kept for the media library and the playlist editor"
    , "  1: show queue"
    , "  2: show browser"
    , "  3: show search_engine"
    , "  6: show outputs"
    , "  tab: next_screen [browser, media_library]"
    , "  shift-tab: previous_screen [browser, media_library]"
    , "  f1: show help"
    , "  \":\": command"
    , "  q: quit"
    , ""
    , "  ctrl-a:"
    , "    name: add"
    , "    e: add end"
    , "    n: add next"
    , "    b: add beginning"
    , "    p: add_and_play"
    , "    /: add_path"
    , "  ctrl-q:"
    , "    name: queue"
    , "    c: clear"
    , "    k: crop"
    , "    s: shuffle"
    , "    r: reverse"
    , "    w: save"
    , "  ctrl-s:"
    , "    name: selection"
    , "    r: select range"
    , "    i: select invert"
    , "    c: select none"
    , "    a: select album"
    , "    f: select found"
    , "  ctrl-t:"
    , "    name: toggle"
    , "    r: toggle repeat"
    , "    z: toggle random"
    , "    s: toggle single"
    , "    c: toggle consume"
    , "    x: toggle crossfade 5"
    , "    g: toggle replay_gain"
    , "    d: toggle display"
    , "    a: toggle album_separators"
    , "    f: toggle follow_playing"
    , "    b: toggle bitrate"
    , "  ctrl-p:"
    , "    name: playback"
    , "    g: seek_to"
    , "    r: replay"
    , "    v: set_volume"
    , "    x: set_crossfade"
    , "  ctrl-d:"
    , "    name: database"
    , "    u: update current"
    , "    U: update all"
    , ""
    , "queue:"
    , "  space: select down"
    , "  backspace: replay"
    , "  m: move_selection up"
    , "  n: move_selection down"
    , "  ctrl-q:"
    , "    m: move_selection cursor"
    , "    e: move_selection end"
    , "    p: priority"
    , ""
    , "browser:"
    , "  backspace: parent"
    , "  ctrl-t:"
    , "    o: next_sort_mode"
    ]

-- | The default keymaps as overrides of empty keymaps.
defaultKeymapOverrides :: Either [String] KeysConfig
defaultKeymapOverrides = case decodeText defaultKeymapsYaml of
  Right keys -> Right keys
  Left errs -> Left . map (prettyError "default keymaps") $ NE.toList errs

-- | The keymaps of a configuration: the defaults with the user's changes.
keymapsOf :: KeysConfig -> KeysConfig -> Keymaps
keymapsOf defaults user =
  Keymaps
    { global = apply (.global)
    , screens =
        M.fromList
          [ (QueueScreen, apply (.queue))
          , (BrowserScreen, apply (.browser))
          , (SearchEngineScreen, apply (.searchEngine))
          , (MediaLibraryScreen, apply (.mediaLibrary))
          , (PlaylistEditorScreen, apply (.playlistEditor))
          , (OutputsScreen, apply (.outputs))
          , (HelpScreen, apply (.help))
          ]
    }
  where
    apply :: (KeysConfig -> KeymapOverride) -> Keymap
    apply field = applyOverride (field user) (applyOverride (field defaults) emptyKeymap)
