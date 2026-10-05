module ConfigTests (configTests) where

import Control.Monad
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Config
import Reprise.Keymap
import Reprise.Keys
import Reprise.Style

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
  config <- expectRight $ decode "lists:\n  style: red\n"
  assertEqual "changed" (style "red") config.lists.style
  assertEqual "default" (style "yellow reverse") config.lists.cursorStyle
  header <- expectRight $ decode "header:\n  title_style: red\n"
  assertEqual "title style" (style "red") header.header.titleStyle
  assertEqual "bold by default" (style "bold") defaultConfig.header.titleStyle

test_defaultKeymaps :: Assertion
test_defaultKeymaps = do
  let global = startLayers QueueScreen (keymapsOf defaultConfig.keys)
  assertEqual "a single key" (Bound Pause) (lookupKey global (key "p"))
  assertEqual
    "a sequence"
    (Bound (Toggle ToggleRepeat))
    (lookupKeys global (keys ["ctrl-t", "r"]))
  assertEqual
    "the queue's own group"
    (Bound (CommandPrompt "priority"))
    (lookupKeys global (keys ["ctrl-q", "p"]))
  assertEqual
    "the global group in the queue"
    (Bound Clear)
    (lookupKeys global (keys ["ctrl-q", "c"]))
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
        , "    ctrl-t:"
        , "      y: toggle single"
        , "    ctrl-d: ~"
        ]
  let layers = startLayers QueueScreen (keymapsOf config.keys)
  assertEqual "replaced" (Bound Next) (lookupKey layers (key "p"))
  assertEqual "removed" Unbound (lookupKey layers (key "s"))
  assertEqual
    "added to a group"
    (Bound (Toggle ToggleSingle))
    (lookupKeys layers (keys ["ctrl-t", "y"]))
  assertEqual
    "kept in a group"
    (Bound (Toggle ToggleRepeat))
    (lookupKeys layers (keys ["ctrl-t", "r"]))
  assertEqual "group removed" Unbound (lookupKey layers (key "ctrl-d"))

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

test_visualizer :: Assertion
test_visualizer = do
  assertEqual "no data source by default" Nothing defaultConfig.visualizer.dataSource
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
  assertEqual "LRCLIB by default" [Lrclib] defaultConfig.lyrics.fetchers
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
  assertError
    "bad style"
    "unknown color or attribute purple"
    (decode "lists:\n  style: purple\n")
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
    "missing column format"
    "missing key \"format\""
    (decode "songs:\n  columns:\n    list:\n    - {width: 5}\n")
  assertError "line and column" "config.yaml:2:10" (decode "lists:\n  style: purple\n")
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
