-- | Keymaps: trees of key bindings, one global and one for each screen.
module Reprise.Keymap
  ( -- * Keymaps
    Keymap (..)
  , Binding (..)
  , Keymaps (..)
  , emptyKeymap
  , screenKeymap

    -- * Overrides
  , KeymapOverride (..)
  , Override (..)
  , applyOverride

    -- * Lookup
  , Layers (..)
  , Lookup (..)
  , startLayers
  , lookupKey
  , lookupKeys

    -- * Which-key entries
  , WhichKeyEntry (..)
  , layersName
  , whichKeyEntries

    -- * Help
  , HelpLine (..)
  , helpLines
  ) where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Yamlet hiding (lookupKey)

import Reprise.Action
import Reprise.Keys

----------------------------------------
-- Keymaps

-- | A keymap, or the group of a prefix key.
data Keymap = Keymap
  { name :: Maybe T.Text
  -- ^ The name of a group, which the which-key panel shows.
  , bindings :: M.Map KeySpec Binding
  }
  deriving stock (Eq, Show)

data Binding
  = BindAction Action
  | BindPrefix Keymap
  deriving stock (Eq, Show)

data Keymaps = Keymaps
  { global :: Keymap
  , screens :: M.Map ScreenName Keymap
  }
  deriving stock (Eq, Show)

emptyKeymap :: Keymap
emptyKeymap = Keymap Nothing M.empty

screenKeymap :: ScreenName -> Keymaps -> Keymap
screenKeymap s keymaps = M.findWithDefault emptyKeymap s keymaps.screens

----------------------------------------
-- Overrides

-- | A user's changes to a keymap or a group.
data KeymapOverride = KeymapOverride
  { name :: Maybe T.Text
  , entries :: M.Map KeySpec Override
  }
  deriving stock (Eq, Show)

data Override
  = OverrideAction Action
  | -- | Merged with the group of the same key, if there is one.
    OverrideGroup KeymapOverride
  | Remove
  deriving stock (Eq, Show)

instance FromYaml Override where
  parseYaml n = case view n of
    NullView -> pure Remove
    StringView _ -> OverrideAction <$> parseYaml n
    _ -> OverrideGroup <$> parseYaml n

-- | A mapping from keys to overrides. The key @name@ names the group.
instance FromYaml KeymapOverride where
  parseYaml = withMapping $ \o -> do
    entries <- forM (objectEntries o) $ \(k, v) -> case view k of
      StringView "name" -> Left <$> withText pure v
      _ -> (\spec binding -> Right (k, spec, binding)) <$> parseYaml k <*> parseYaml v
    bindings <- foldM insertUnique M.empty [b | Right b <- entries]
    pure $ KeymapOverride (listToMaybe [t | Left t <- entries]) bindings
    where
      -- Different spellings of one key, e.g. 1 and "1", are a duplicate
      -- that YAML doesn't see.
      insertUnique
        :: M.Map KeySpec Override -> (Node, KeySpec, Override) -> Parser (M.Map KeySpec Override)
      insertUnique m (node, k, v)
        | k `M.member` m =
            failAt node $ "the key " <> T.unpack (renderKeySpec k) <> " is bound twice"
        | otherwise = pure (M.insert k v m)

-- | Merge a user's changes into a keymap.
applyOverride :: KeymapOverride -> Keymap -> Keymap
applyOverride o keymap =
  Keymap
    { name = maybe keymap.name Just o.name
    , bindings = M.foldrWithKey apply keymap.bindings o.entries
    }
  where
    apply :: KeySpec -> Override -> M.Map KeySpec Binding -> M.Map KeySpec Binding
    apply k = \case
      OverrideAction a -> M.insert k (BindAction a)
      Remove -> M.delete k
      OverrideGroup g -> \bs ->
        let base = case M.lookup k bs of
              Just (BindPrefix group) -> group
              _ -> emptyKeymap
        in M.insert k (BindPrefix (applyOverride g base)) bs

----------------------------------------
-- Lookup

-- | The groups that the keys pressed so far lead to: in the screen's keymap
-- and in the global one. A layer is 'Nothing' once the keys left it.
data Layers = Layers
  { screen :: Maybe Keymap
  , global :: Maybe Keymap
  }
  deriving stock (Eq, Show)

data Lookup
  = Bound Action
  | -- | The keys so far are a prefix.
    Pending Layers
  | Unbound
  deriving stock (Eq, Show)

startLayers :: ScreenName -> Keymaps -> Layers
startLayers s keymaps = Layers (Just (screenKeymap s keymaps)) (Just keymaps.global)

-- | Look up the next key. If both layers bind the key to a prefix, the
-- lookup continues in both groups. Otherwise the screen's binding wins.
lookupKey :: Layers -> KeySpec -> Lookup
lookupKey layers k = case (find layers.screen, find layers.global) of
  (Just (BindAction a), _) -> Bound a
  (Just (BindPrefix s), Just (BindPrefix g)) -> Pending $ Layers (Just s) (Just g)
  (Just (BindPrefix s), _) -> Pending $ Layers (Just s) Nothing
  (Nothing, Just (BindAction a)) -> Bound a
  (Nothing, Just (BindPrefix g)) -> Pending $ Layers Nothing (Just g)
  (Nothing, Nothing) -> Unbound
  where
    find :: Maybe Keymap -> Maybe Binding
    find = (>>= M.lookup k . (.bindings))

-- | Look up a sequence of keys.
lookupKeys :: Layers -> [KeySpec] -> Lookup
lookupKeys layers = \case
  [] -> Pending layers
  k : ks -> case lookupKey layers k of
    Pending next -> lookupKeys next ks
    result
      | null ks -> result
      | otherwise -> Unbound

----------------------------------------
-- Which-key entries

data WhichKeyEntry = WhichKeyEntry
  { key :: KeySpec
  , description :: T.Text
  , isPrefix :: Bool
  , fromScreen :: Bool
  -- ^ The screen's keymap binds the key, not the global one.
  }
  deriving stock (Eq, Show)

-- | The name of the group the keys lead to, the screen's first.
layersName :: Layers -> Maybe T.Text
layersName layers = listToMaybe $ mapMaybe (>>= (.name)) [layers.screen, layers.global]

-- | The keys that can follow, in key order. The screen's bindings shadow
-- the global ones.
whichKeyEntries :: Layers -> [WhichKeyEntry]
whichKeyEntries layers = M.elems $ M.union (entries True layers.screen) (entries False layers.global)
  where
    entries :: Bool -> Maybe Keymap -> M.Map KeySpec WhichKeyEntry
    entries fromScreen = maybe M.empty $ \keymap -> M.mapWithKey (entry fromScreen) keymap.bindings

    entry :: Bool -> KeySpec -> Binding -> WhichKeyEntry
    entry fromScreen k = \case
      BindAction a -> WhichKeyEntry k (describeAction a) False fromScreen
      BindPrefix g -> WhichKeyEntry k (maybe "+prefix" ("+" <>) g.name) True fromScreen

----------------------------------------
-- Help

-- | A line of the help screen.
data HelpLine
  = Heading T.Text
  | -- | A key sequence and what it does.
    Entry T.Text T.Text
  | Blank
  deriving stock (Eq, Show)

-- | A section for the global keymap and for each screen's keymap that binds
-- a key. A section lists the keys of its keymap, then each group of a
-- prefix key with the whole key sequences, e.g. @ctrl-t r@.
helpLines :: Keymaps -> [HelpLine]
helpLines keymaps =
  drop 1 . concat $
    [ Blank : Heading title : keymapLines [] keymap
    | (title, keymap) <- ("Global", keymaps.global) : screens
    , not (M.null keymap.bindings)
    ]
  where
    screens :: [(T.Text, Keymap)]
    screens =
      [ (T.toTitle (T.replace "_" " " (screenName s)), screenKeymap s keymaps)
      | s <- screenNames
      ]

    keymapLines :: [KeySpec] -> Keymap -> [HelpLine]
    keymapLines prefix keymap =
      [ Entry (keysText (prefix <> [k])) (describeAction a)
      | (k, BindAction a) <- M.toList keymap.bindings
      ]
        <> concat
          [ Blank : Heading (keysText keys <> maybe "" (": " <>) group.name) : keymapLines keys group
          | (k, BindPrefix group) <- M.toList keymap.bindings
          , let keys = prefix <> [k]
          ]

    keysText :: [KeySpec] -> T.Text
    keysText = T.unwords . map renderKeySpec
