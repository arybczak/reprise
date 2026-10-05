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
  , sortByName
  , SearchEngineConfig (..)
  , HeaderConfig (..)
  , StatusBarConfig (..)
  , ProgressBarConfig (..)
  , ProgressChars (..)
  , VisualizerConfig (..)
  , LyricsConfig (..)
  , Visualization (..)
  , visualizationName
  , FrameRate (..)
  , StylesConfig (..)
  , KeysConfig (..)
  , defaultConfig

    -- * Loading
  , decodeConfig
  , loadConfig

    -- * Keymaps
  , keymapsOf
  , defaultKeymaps
  ) where

import Data.ByteString qualified as BS
import Data.Char hiding (Space)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Optics.Core hiding (view)
import System.Directory
import Yamlet

import Reprise.Action
import Reprise.Format
import Reprise.Keymap
import Reprise.Keys
import Reprise.Mpd.Protocol.Types
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
  , visualizer :: VisualizerConfig
  , lyrics :: LyricsConfig
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
  , foundStyle :: Style
  -- ^ The items that match an unfinished find.
  , playingStyle :: Style
  , queuedStyle :: Style
  -- ^ The songs that are in the queue, in the other screens. ncmpcpp
  -- always makes them bold.
  , keepCursorCentered :: Bool
  , ignoreLeadingThe :: Bool
  , missingTag :: T.Text
  , missingTagStyle :: Style
  -- ^ Not in the columns display, where the marker has the column's style.
  -- The search engine's form and the song info screen will show their empty
  -- fields with it, as ncmpcpp does.
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

data VisualizerConfig = VisualizerConfig
  { dataSource :: Maybe FilePath
  -- ^ The fifo of MPD's fifo output, in the format @44100:16:2@, or
  -- @44100:16:1@ without 'inStereo'. A leading @~/@ is the home directory.
  , inStereo :: Bool
  , visualization :: Visualization
  -- ^ The one that the visualizer shows first.
  , fps :: FrameRate
  , trail :: Duration
  -- ^ How long the samples of a frame of the ellipse stay on the screen.
  , colors :: NE.NonEmpty Style
  -- ^ From quiet to loud: from the center of the ellipse to its edges, and
  -- from the foot of a bar of the spectrum to its top.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml VisualizerConfig

newtype LyricsConfig = LyricsConfig
  { directory :: Maybe FilePath
  -- ^ Where the lyrics are stored, @$XDG_DATA_HOME/reprise/lyrics@ without
  -- it. A leading @~/@ is the home directory. ncmpcpp's is @~/.lyrics@.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromYaml) via GenericYaml LyricsConfig

data Visualization
  = -- | The levels of the frequencies, as bars.
    Spectrum
  | -- | The left channel across and the right one up.
    Ellipse
  deriving stock (Eq, Show, Enum, Bounded)

visualizationName :: Visualization -> T.Text
visualizationName = \case
  Spectrum -> "spectrum"
  Ellipse -> "ellipse"

-- | Frames per second, at least one.
newtype FrameRate = FrameRate Int
  deriving newtype (Eq, Show)

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
  , visualizer :: KeymapOverride
  , lyrics :: KeymapOverride
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
    , visualizer = defaultVisualizer
    , lyrics = defaultLyrics
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
    , foundStyle = style "underline"
    , playingStyle = style "bold"
    , queuedStyle = style "bold"
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

defaultVisualizer :: VisualizerConfig
defaultVisualizer =
  VisualizerConfig
    { dataSource = Nothing
    , inStereo = True
    , visualization = Spectrum
    , fps = FrameRate 60
    , trail = 0.15
    , colors =
        style "46"
          NE.:| map style ["82", "118", "154", "190", "226", "220", "214", "208", "202", "196", "160"]
    }

defaultLyrics :: LyricsConfig
defaultLyrics = LyricsConfig {directory = Nothing}

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
    , visualizer = noOverride
    , lyrics = noOverride
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

instance GenericYamlOptions VisualizerConfig where
  yamlOptions = options
  yamlDefault = Just defaultVisualizer

instance GenericYamlOptions LyricsConfig where
  yamlOptions = options
  yamlDefault = Just defaultLyrics

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

instance FromYaml Visualization where
  parseYaml = oneOf [(visualizationName v, v) | v <- [minBound .. maxBound]]

instance FromYaml SortBy where
  parseYaml = oneOf [(sortByName by, by) | by <- [minBound .. maxBound]]

instance FromYaml FrameRate where
  parseYaml n = do
    rate <- parseYaml @Int n
    if rate >= 1
      then pure (FrameRate rate)
      else failAt n "expected at least 1 frame per second"

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

-- | The keymaps of a configuration: the defaults with the user's changes.
keymapsOf :: KeysConfig -> Keymaps
keymapsOf user =
  Keymaps
    { global = applyOverride user.global defaultKeymaps.global
    , screens =
        M.fromList
          [ (screen, applyOverride (field user) (screenKeymap screen defaultKeymaps))
          | (screen, field) <- screenFields
          ]
    }
  where
    screenFields :: [(ScreenName, KeysConfig -> KeymapOverride)]
    screenFields =
      [ (QueueScreen, (.queue))
      , (BrowserScreen, (.browser))
      , (SearchEngineScreen, (.searchEngine))
      , (MediaLibraryScreen, (.mediaLibrary))
      , (PlaylistEditorScreen, (.playlistEditor))
      , (OutputsScreen, (.outputs))
      , (VisualizerScreen, (.visualizer))
      , (LyricsScreen, (.lyrics))
      , (HelpScreen, (.help))
      ]

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
          , char 'o' ~> JumpToPlaying
          , char 'G' ~> JumpToBrowser
          , shift ArrowUp ~> Select (SelectItem (Just MoveUp))
          , shift ArrowDown ~> Select (SelectItem (Just MoveDown))
          , plain InsertKey ~> Select (SelectItem Nothing)
          , -- Verbs: each screen implements them its own way.
            plain Enter ~> Activate
          , plain Space ~> AddOrRemove
          , plain DeleteKey ~> Delete
          , char 'p' ~> Pause
          , char 's' ~> Stop
          , char '<' ~> Previous
          , char '>' ~> Next
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
          , ctrl 'f' ~> Filter
          , -- 4, 5 and 6 are kept for the media library, the playlist editor
            -- and the tag editor.
            char '1' ~> Show QueueScreen
          , char '2' ~> Show BrowserScreen
          , char '3' ~> Show SearchEngineScreen
          , char '7' ~> Show OutputsScreen
          , char '8' ~> Show VisualizerScreen
          , char 'l' ~> Show LyricsScreen
          , plain Tab ~> NextScreen [BrowserScreen, VisualizerScreen, MediaLibraryScreen]
          , shift Tab ~> PreviousScreen [BrowserScreen, VisualizerScreen, MediaLibraryScreen]
          , plain (Function 1) ~> Show HelpScreen
          , char ':' ~> CommandPrompt ""
          , char 'q' ~> Quit
          , group
              (ctrl 'a')
              "add"
              [ char 'e' ~> Add AddEnd
              , char 'n' ~> Add AddNext
              , char 'b' ~> Add AddBeginning
              , char 'p' ~> AddAndPlay
              , char '/' ~> CommandPrompt "add_path"
              ]
          , group
              (ctrl 'q')
              "queue"
              [ char 'c' ~> Clear
              , char 's' ~> Shuffle
              , char 'w' ~> Save
              ]
          , group
              (ctrl 's')
              "selection"
              [ char 'r' ~> Select SelectRange
              , char 'i' ~> Select SelectInvert
              , char 'c' ~> Select SelectNone
              , char 'a' ~> Select SelectAlbum
              , char 'A' ~> Select SelectArtist
              , char 'f' ~> Select SelectFound
              ]
          , group
              (ctrl 't')
              "toggle"
              [ char 'r' ~> Toggle ToggleRepeat
              , char 'z' ~> Toggle ToggleRandom
              , char 's' ~> Toggle ToggleSingle
              , char 'c' ~> Toggle ToggleConsume
              , char 'x' ~> Toggle (ToggleCrossfade 5)
              , char 'g' ~> Toggle ToggleReplayGain
              , char 'd' ~> Toggle ToggleDisplay
              , char 'a' ~> Toggle ToggleAlbumSeparators
              , char 'f' ~> Toggle ToggleFollowPlaying
              , char 'b' ~> Toggle ToggleBitrate
              ]
          , group
              (ctrl 'p')
              "playback"
              [ char 'r' ~> Replay
              , char 's' ~> CommandPrompt "seek"
              , char 'v' ~> CommandPrompt "volume"
              , char 'x' ~> CommandPrompt "crossfade"
              ]
          , group
              (ctrl 'd')
              "database"
              [ char 'u' ~> Update UpdateCurrent
              , char 'U' ~> Update UpdateAll
              ]
          ]
    , screens =
        M.fromList
          [
            ( QueueScreen
            , keymap
                [ -- As in the author's ncmpcpp bindings.
                  plain Space ~> Select (SelectItem (Just MoveDown))
                , plain Backspace ~> Replay
                , char 'm' ~> MoveSelection MoveSelectionUp
                , char 'n' ~> MoveSelection MoveSelectionDown
                , group
                    (ctrl 'q')
                    "queue"
                    [ char 'm' ~> MoveSelection MoveSelectionToCursor
                    , char 'e' ~> MoveSelection MoveSelectionToEnd
                    , char 'p' ~> CommandPrompt "priority"
                    ]
                ]
            )
          ,
            ( BrowserScreen
            , keymap
                [ plain Backspace ~> Parent
                , group (ctrl 't') "toggle" [char 'o' ~> NextSortMode]
                ]
            )
          , -- As in ncmpcpp.
            (VisualizerScreen, keymap [plain Space ~> Toggle ToggleVisualization])
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

    plain :: Key -> KeySpec
    plain = KeySpec S.empty

    char :: Char -> KeySpec
    char = plain . CharKey

    ctrl :: Char -> KeySpec
    ctrl = KeySpec (S.singleton Ctrl) . CharKey

    shift :: Key -> KeySpec
    shift = KeySpec (S.singleton Shift)
