{-# LANGUAGE AllowAmbiguousTypes #-}

module ConfigTests (configTests) where

import Control.Monad
import Data.Kind
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics
import GHC.TypeLits
import Optics.Core hiding (view)
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Yamlet hiding (decode, lookupKey)

import Reprise.Action
import Reprise.Config
import Reprise.Keymap
import Reprise.Keys
import Reprise.Style
import Utils

configTests :: TestTree
configTests =
  testGroup
    "Config"
    [ testCase "empty file" test_emptyFile
    , testCase "a section keeps its other defaults" test_sectionDefaults
    , testCase "the default keymaps" test_defaultKeymaps
    , testCase "a config can bind every default key" test_defaultKeysWritable
    , testCase "keymap changes merge with the defaults" test_keymapMerge
    , testCase "columns" test_columns
    , testCase "durations" test_durations
    , testCase "the documented defaults" test_documentedDefaults
    , testCase "the documentation lists everything built" test_documentedEverything
    , testCase "the visualizer" test_visualizer
    , testCase "the lyrics" test_lyrics
    , testCase "window title can be disabled" test_noWindowTitle
    , testCase "errors" test_errors
    ]

test_emptyFile :: Assertion
test_emptyFile = do
  assertEqual "empty" (Right defaultConfig) (decode "")
  assertEqual "empty mapping" (Right defaultConfig) (decode "{}")

test_sectionDefaults :: Assertion
test_sectionDefaults = do
  config <- expectRight $ decode "styles:\n  list:\n    normal: red\n"
  assertEqual "changed" (style "red") config.styles.list.normal
  assertEqual "default" (style "yellow reverse") config.styles.list.cursor
  assertEqual "another part" (style "bold") config.styles.header.title
  assertEqual "a shared style" (style "green") config.styles.value

test_defaultKeymaps :: Assertion
test_defaultKeymaps = do
  let global = startLayers QueueScreen (keymapsOf defaultConfig.keys)
  assertEqual "a single key" (Bound Pause) (lookupKey global (key "p"))
  assertEqual
    "a sequence"
    (Bound (Toggle ToggleRepeat))
    (lookupKeys global (keys ["t", "r"]))
  assertEqual
    "the queue's own group"
    (Bound (CommandPrompt "priority"))
    (lookupKeys global (keys ["e", "p"]))
  assertEqual
    "a group in the queue's group"
    (Bound (MoveSongs MoveSongsToBeginning))
    (lookupKeys global (keys ["e", "m", "b"]))
  assertEqual
    "the global group in the queue"
    (Bound Clear)
    (lookupKeys global (keys ["e", "c"]))
  assertEqual
    "the queue's space"
    (Bound (Select (SelectItem (Just MoveDown))))
    (lookupKey global (key "space"))
  assertEqual "a digit key" (Bound (Show QueueScreen)) (lookupKey global (key "1"))

-- | The default keymaps are built from key values, so they could hold a key
-- that a config can't name, e.g. one that a terminal can't tell apart from
-- another.
test_defaultKeysWritable :: Assertion
test_defaultKeysWritable =
  forM_ (allKeys defaultKeymaps.global <> concatMap allKeys defaultKeymaps.screens) $ \k ->
    assertEqual (show k) (Right k) (parseKeySpec (renderKeySpec k))
  where
    allKeys :: Keymap -> [KeySpec]
    allKeys keymap = flip concatMap (M.toList keymap.bindings) $ \(k, binding) -> case binding of
      BindAction _ -> [k]
      BindPrefix group -> k : allKeys group

test_keymapMerge :: Assertion
test_keymapMerge = do
  config <-
    expectRight . decode $
      T.unlines
        [ "keys:"
        , "  global:"
        , "    p: next"
        , "    s: ~"
        , "    t:"
        , "      y: toggle single"
        , "    d: ~"
        ]
  let layers = startLayers QueueScreen (keymapsOf config.keys)
  assertEqual "replaced" (Bound Next) (lookupKey layers (key "p"))
  assertEqual "removed" Unbound (lookupKey layers (key "s"))
  assertEqual
    "added to a group"
    (Bound (Toggle ToggleSingle))
    (lookupKeys layers (keys ["t", "y"]))
  assertEqual
    "kept in a group"
    (Bound (Toggle ToggleRepeat))
    (lookupKeys layers (keys ["t", "r"]))
  assertEqual "group removed" Unbound (lookupKey layers (key "d"))

test_columns :: Assertion
test_columns = do
  config <-
    expectRight . decode $
      T.unlines
        [ "songs:"
        , "  columns:"
        , "    list:"
        , "    - {width: 30%, format: '%{artist}'}"
        , "    - {width: 7, format: '%{length}', align: right}"
        ]
  case config.songs.columns.list of
    [a, b] -> do
      assertEqual "relative" (RelativeWidth 30) a.width
      assertEqual "fixed" (FixedWidth 7) b.width
      assertEqual "default alignment" AlignLeft a.align
      assertEqual "alignment" AlignRight b.align
      assertEqual "default style" mempty a.style
    cs -> assertFailure $ "expected 2 columns, got " <> show (length cs)

test_durations :: Assertion
test_durations = do
  config <- expectRight $ decode "mpd:\n  timeout: 500ms\n"
  assertEqual "milliseconds" 0.5 config.mpd.timeout
  assertEqual "default" 5 defaultConfig.mpd.timeout

-- | The config of the documentation shows the defaults: its keys make the
-- default keymaps, and the other options that it lists have their default
-- values.
test_documentedDefaults :: Assertion
test_documentedDefaults = do
  config <- either (assertFailure . unlines) pure =<< loadConfig docConfig
  assertEqual "the keymaps" (keymapsOf defaultConfig.keys) (keymapsOf config.keys)
  assertEqual "the other options" defaultConfig (config & #keys .~ defaultConfig.keys)

-- | The config of the documentation lists every option and every binding,
-- but those of the features that aren't built yet.
test_documentedEverything :: Assertion
test_documentedEverything = do
  config <- either (assertFailure . unlines) pure =<< loadConfig docConfig
  Tree tree <- either (assertFailure . show) pure =<< decodeFile docConfig
  assertEqual
    "the options left out"
    [ "search_engine"
    , "queue.album_separators"
    , "search_engine.display"
    , "styles.popup_border"
    , "styles.list.inactive_cursor"
    ]
    [ T.intercalate "." (path <> [name])
    | (path, names) <- sections
    , name <- names
    , name `notElem` keysAt path tree
    ]
  let documented =
        Keymaps
          { global = applyOverride config.keys.global emptyKeymap
          , screens = applyOverride `flip` emptyKeymap <$> config.keys.screens
          }
  assertEqual
    "the bindings left out"
    [ (Nothing, [key "3"], Show SearchEngineScreen)
    , (Nothing, [key "t", key "a"], Toggle ToggleAlbumSeparators)
    ]
    (bindingsOf defaultKeymaps L.\\ bindingsOf documented)
  where
    -- The paths of the sections of options, and the names of their options.
    sections :: [([T.Text], [T.Text])]
    sections =
      [ ([], optionNames @Config)
      , (["mpd"], optionNames @MpdConfig)
      , (["songs"], optionNames @SongsConfig)
      , (["songs", "classic"], optionNames @RowFormat)
      , (["songs", "columns"], optionNames @ColumnsConfig)
      , (["lists"], optionNames @ListsConfig)
      , (["queue"], optionNames @QueueConfig)
      , (["browser"], optionNames @BrowserConfig)
      , (["browser", "sort"], optionNames @BrowserSort)
      , (["search_engine"], optionNames @SearchEngineConfig)
      , (["status_bar"], optionNames @StatusBarConfig)
      , (["progress_bar"], optionNames @ProgressBarConfig)
      , (["visualizer"], optionNames @VisualizerConfig)
      , (["lyrics"], optionNames @LyricsConfig)
      , (["editor"], optionNames @EditorConfig)
      , (["mouse"], optionNames @MouseConfig)
      , (["styles"], optionNames @StylesConfig)
      , (["styles", "text"], optionNames @TextStyles)
      , (["styles", "list"], optionNames @ListStyles)
      , (["styles", "header"], optionNames @HeaderStyles)
      , (["styles", "status_bar"], optionNames @StatusBarStyles)
      , (["styles", "progress_bar"], optionNames @ProgressBarStyles)
      ]

    -- The keys of the mapping at the path.
    keysAt :: [T.Text] -> Node -> [T.Text]
    keysAt path n = case (path, view n) of
      ([], MappingView kvs) -> [k | (kn, _) <- kvs, StringView k <- [view kn]]
      (p : ps, MappingView kvs) ->
        concat [keysAt ps v | (kn, v) <- kvs, StringView k <- [view kn], k == p]
      _ -> []

    -- Each binding with its screen, or none for the global keymap, and the
    -- keys that lead to it.
    bindingsOf :: Keymaps -> [(Maybe ScreenName, [KeySpec], Action)]
    bindingsOf keymaps =
      [(Nothing, ks, a) | (ks, a) <- leaves keymaps.global]
        <> [ (Just screen, ks, a)
           | (screen, keymap) <- M.toList keymaps.screens
           , (ks, a) <- leaves keymap
           ]

    leaves :: Keymap -> [([KeySpec], Action)]
    leaves keymap =
      M.toList keymap.bindings >>= \case
        (k, BindAction a) -> [([k], a)]
        (k, BindPrefix group) -> [(k : ks, a) | (ks, a) <- leaves group]

test_visualizer :: Assertion
test_visualizer = do
  assertEqual "no data source by default" Nothing defaultConfig.visualizer.dataSource
  assertBool
    "bold colors by default"
    (all (\s -> Bold `elem` s.attributes) defaultConfig.visualizer.colors)
  config <-
    expectRight $
      decode "visualizer:\n  data_source: ~/.config/mpd/feed\n  fps: 30\n  colors: [red, 82]\n"
  assertEqual "the data source" (Just "~/.config/mpd/feed") config.visualizer.dataSource
  assertEqual "the rate" (FrameRate 30) config.visualizer.fps
  assertEqual "the colors" (style "red" NE.:| [style "82"]) config.visualizer.colors
  assertEqual "the spectrum first" Spectrum defaultConfig.visualizer.visualization
  ellipse <- expectRight $ decode "visualizer:\n  visualization: ellipse\n"
  assertEqual "the ellipse first" Ellipse ellipse.visualizer.visualization

test_lyrics :: Assertion
test_lyrics = do
  assertEqual "the default directory" Nothing defaultConfig.lyrics.directory
  config <- expectRight $ decode "lyrics:\n  directory: ~/.lyrics\n"
  assertEqual "the directory" (Just "~/.lyrics") config.lyrics.directory
  assertEqual "LRCLIB, then tekstowo.pl" [Lrclib, Tekstowo] defaultConfig.lyrics.fetchers
  tekstowo <- expectRight $ decode "lyrics:\n  fetchers: [tekstowo]\n"
  assertEqual "tekstowo.pl" [Tekstowo] tekstowo.lyrics.fetchers
  stored <- expectRight $ decode "lyrics:\n  fetchers: []\n"
  assertEqual "only the stored lyrics" [] stored.lyrics.fetchers
  assertEqual "no editor by default" Nothing defaultConfig.editor.command
  editing <- expectRight $ decode "editor:\n  command: mcedit\n"
  assertEqual "the editor" (Just "mcedit") editing.editor.command

test_noWindowTitle :: Assertion
test_noWindowTitle = do
  config <- expectRight $ decode "window_title: ~\n"
  assertEqual "disabled" Nothing config.windowTitle

test_errors :: Assertion
test_errors = do
  assertError "unknown key with a hint" "window_titel" (decode "window_titel: x\n")
  assertError "unknown fetcher" "genius" (decode "lyrics:\n  fetchers: [genius]\n")
  assertError "port too large" "a port is from 1 to 65535" (decode "mpd:\n  port: 70000\n")
  assertError "no lines to scroll" "at least 1 line" (decode "mouse:\n  scroll_lines: 0\n")
  assertError
    "a volume step too large"
    "from 1 to 100"
    (decode "mouse:\n  volume_step: 101\n")
  assertError "port 0" "a port is from 1 to 65535" (decode "mpd:\n  port: 0\n")
  assertError
    "no timeout"
    "a timeout must be longer than 0"
    (decode "mpd:\n  timeout: 0s\n")
  -- 'Seconds' counts milliseconds, so less than one is 0.
  assertError
    "a timeout too short"
    "a timeout must be longer than 0"
    (decode "mpd:\n  timeout: 0.1ms\n")
  assertError
    "bad style"
    "unknown color or attribute purple"
    (decode "styles:\n  list:\n    normal: purple\n")
  assertError
    "style in a plain format"
    "this format can't contain styles"
    (decode "window_title: '<red>x</>'\n")
  assertError
    "bad format"
    "at character 1 of the format: %a is not a tag"
    (decode "status_bar:\n  song: '%a'\n")
  assertError
    "bad key"
    "ctrl-i is the same key as tab"
    (decode "keys:\n  global:\n    ctrl-i: pause\n")
  assertError
    "bad action"
    "unknown action pasue"
    (decode "keys:\n  global:\n    p: pasue\n")
  assertError
    "same key twice"
    "the key 1 is bound twice"
    (decode "keys:\n  global:\n    1: pause\n    \"1\": stop\n")
  assertError
    "the key that always quits"
    "ctrl-q always quits, so a keymap can't bind it"
    (decode "keys:\n  queue:\n    t:\n      ctrl-q: pause\n")
  assertError
    "missing column format"
    "missing key \"format\""
    (decode "songs:\n  columns:\n    list:\n    - {width: 5}\n")
  assertError
    "line and column"
    "config.yaml:3:13"
    (decode "styles:\n  list:\n    normal: purple\n")
  assertError
    "no frames"
    "expected at least 1 frame per second"
    (decode "visualizer:\n  fps: 0\n")
  assertError "no colors" "expected a non-empty list" (decode "visualizer:\n  colors: []\n")
  where
    assertError :: String -> String -> Either [String] Config -> Assertion
    assertError msg expected = \case
      Left errs
        | any (expected `L.isInfixOf`) errs -> pure ()
        | otherwise ->
            assertFailure $ msg <> ": no error contains " <> show expected <> ":\n" <> unlines errs
      Right _ -> assertFailure $ msg <> ": decoded"

----------------------------------------
-- Helpers

decode :: T.Text -> Either [String] Config
decode = decodeConfig "config.yaml" . T.encodeUtf8

docConfig :: FilePath
docConfig = "doc" </> "config.yaml"

-- | A YAML document as it is.
newtype Tree = Tree Node

instance FromYaml Tree where
  parseYaml = pure . Tree

-- | The names of the options of a section of the config.
optionNames :: forall a. (GenericYamlOptions a, Selectors (Rep a)) => [T.Text]
optionNames = map (T.pack . (yamlOptions @a).fieldLabelModifier) (selectors @(Rep a))

-- | The names of the fields of a record.
class Selectors (r :: Type -> Type) where
  selectors :: [String]

instance Selectors f => Selectors (D1 c f) where
  selectors = selectors @f

instance Selectors f => Selectors (C1 c f) where
  selectors = selectors @f

instance (Selectors f, Selectors g) => Selectors (f :*: g) where
  selectors = selectors @f <> selectors @g

instance KnownSymbol name => Selectors (S1 (MetaSel (Just name) u s d) f) where
  selectors = [symbolVal (Proxy @name)]

expectRight :: Either [String] a -> IO a
expectRight = \case
  Right a -> pure a
  Left errs -> assertFailure (unlines errs)

style :: T.Text -> Style
style = either (error . T.unpack) id . parseStyle

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec

keys :: [T.Text] -> [KeySpec]
keys = map key
