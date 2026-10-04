-- | The layers of the modules that DESIGN.md describes under "Packages and
-- modules": an import only goes down. GHC can't check this without a
-- library for each layer, so this reads the imports of the sources.
module LayerTests (layerTests) where

import Control.Monad
import Data.Bool
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

layerTests :: TestTree
layerTests =
  testGroup
    "Layers"
    [ testCase "the modules follow the layers" test_sources
    , testCase "an import across the layers is reported" test_crossings
    ]

test_sources :: Assertion
test_sources = do
  modules <- sourceModules sourceDir
  assertBool "modules found" (not (null modules))
  assertEqual "imports across the layers" [] (crossings modules)

test_crossings :: Assertion
test_crossings = do
  let crossed importer imported = not . null $ crossings [(importer, [imported])]
  assertBool "the handlers" (crossed "Reprise.UI.Layout" "Reprise.Handler")
  assertBool "another screen" (crossed "Reprise.Screen.Help" "Reprise.Screen.Queue")
  assertBool "a screen from the core" (crossed "Reprise.Handler.Core" "Reprise.Screen.Help")
  assertBool "the layout from the core" (crossed "Reprise.Handler.Core" "Reprise.UI.Layout")
  assertBool "the core from below" (crossed "Reprise.State" "Reprise.Handler.Core")
  assertBool "a screen from below" (crossed "Reprise.UI.SongList" "Reprise.Screen.Queue")
  assertBool
    "a screen's own module"
    (not (crossed "Reprise.Screen.Queue" "Reprise.Screen.Queue.Edits"))
  assertBool
    "the shared drawing"
    (not (crossed "Reprise.Screen.Queue" "Reprise.UI.SongList"))

-- | Each import that crosses the layers, with the rule that it breaks.
crossings :: [(T.Text, [T.Text])] -> [T.Text]
crossings modules =
  [ importer <> " imports " <> imported <> ": " <> rule
  | (importer, imports) <- modules
  , imported <- imports
  , Just rule <- [crossing importer imported]
  ]
  where
    crossing :: T.Text -> T.Text -> Maybe T.Text
    crossing importer imported
      | imported == handler && importer /= app =
          Just "only Reprise.App imports Reprise.Handler"
      | isScreen imported
      , screenOf importer /= screenOf imported
      , importer `notElem` [handler, layout] =
          Just "only Reprise.Handler and Reprise.UI.Layout import a screen"
      | importer == core && (isScreen imported || "Reprise.UI." `T.isPrefixOf` imported) =
          Just "Reprise.Handler.Core imports no screen and no module of Reprise.UI"
      | isBelowCore importer && not (isBelowCore imported) =
          Just "a module under Reprise.Handler.Core imports none above it"
      | otherwise = Nothing

    isScreen :: T.Text -> Bool
    isScreen = ("Reprise.Screen." `T.isPrefixOf`)

    -- The screen that a module belongs to, e.g. Queue for
    -- Reprise.Screen.Queue.Edits.
    screenOf :: T.Text -> Maybe T.Text
    screenOf m = T.takeWhile (/= '.') <$> T.stripPrefix "Reprise.Screen." m

    isBelowCore :: T.Text -> Bool
    isBelowCore m = not (isScreen m) && m `notElem` [app, handler, layout, core]

    app, handler, layout, core :: T.Text
    app = "Reprise.App"
    handler = "Reprise.Handler"
    layout = "Reprise.UI.Layout"
    core = "Reprise.Handler.Core"

-- | The modules under a directory, each with the reprise modules that it
-- imports.
sourceModules :: FilePath -> IO [(T.Text, [T.Text])]
sourceModules dir = do
  files <- filter ((== ".hs") . takeExtension) <$> listRecursively dir
  forM (L.sort files) $ \file -> do
    source <- T.readFile file
    pure (moduleName file, imports source)
  where
    listRecursively :: FilePath -> IO [FilePath]
    listRecursively d = do
      entries <- map (d </>) <$> listDirectory d
      concat
        <$> forM entries (\e -> doesDirectoryExist e >>= bool (pure [e]) (listRecursively e))

    moduleName :: FilePath -> T.Text
    moduleName = T.pack . L.intercalate "." . splitDirectories . dropExtension . makeRelative dir

    -- Imports are on a line each, in the postpositive form that the
    -- warnings require.
    imports :: T.Text -> [T.Text]
    imports source =
      [ m
      | l <- T.lines source
      , Just rest <- [T.stripPrefix "import " l]
      , let m = T.takeWhile (/= ' ') rest
      , "Reprise." `T.isPrefixOf` m
      ]

-- | The sources of the reprise library, from the root of the package, where
-- the tests run.
sourceDir :: FilePath
sourceDir = "src" </> "reprise"
