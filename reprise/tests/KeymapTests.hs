module KeymapTests (keymapTests) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Keymap
import Reprise.Keys

keymapTests :: TestTree
keymapTests =
  testGroup
    "Keymap"
    [ testCase "override replaces, merges and removes" test_override
    , testCase "screen keymap first" test_screenFirst
    , testCase "prefixes in both layers merge" test_prefixesMerge
    , testCase "screen action shadows global prefix" test_actionShadowsPrefix
    , testCase "sequences" test_sequences
    , testCase "which-key entries" test_whichKey
    ]

test_override :: Assertion
test_override = do
  let base =
        keymap
          [ ("p", BindAction Pause)
          , ("s", BindAction Stop)
          , ("ctrl-t", BindPrefix (named "toggle" [("r", BindAction (Toggle ToggleRepeat))]))
          , ("ctrl-d", BindPrefix (named "database" [("u", BindAction (Update UpdateCurrent))]))
          ]
      user =
        KeymapOverride Nothing $
          M.fromList
            [ (key "p", OverrideAction Next)
            , (key "s", Remove)
            ,
              ( key "ctrl-t"
              , OverrideGroup
                  (KeymapOverride Nothing (M.fromList [(key "y", OverrideAction (Toggle ToggleSingle))]))
              )
            , (key "ctrl-d", Remove)
            ]
  assertEqual
    "merged"
    ( keymap
        [ ("p", BindAction Next)
        ,
          ( "ctrl-t"
          , BindPrefix
              ( named
                  "toggle"
                  [ ("r", BindAction (Toggle ToggleRepeat))
                  , ("y", BindAction (Toggle ToggleSingle))
                  ]
              )
          )
        ]
    )
    (applyOverride user base)

test_screenFirst :: Assertion
test_screenFirst = do
  let layers =
        Layers
          (Just $ keymap [("space", BindAction (Select (SelectItem (Just MoveDown))))])
          (Just globalKeymap)
  assertEqual
    "screen binding"
    (Bound (Select (SelectItem (Just MoveDown))))
    (lookupKey layers (key "space"))
  assertEqual "global binding" (Bound Pause) (lookupKey layers (key "p"))
  assertEqual "unbound" Unbound (lookupKey layers (key "z"))

test_prefixesMerge :: Assertion
test_prefixesMerge = do
  let queueKeymap =
        keymap
          [ ("ctrl-q", BindPrefix (keymap [("m", BindAction (MoveSelection MoveSelectionToCursor))]))
          ]
      layers = Layers (Just queueKeymap) (Just globalKeymap)
  assertEqual
    "screen's key in the group"
    (Bound (MoveSelection MoveSelectionToCursor))
    (lookupKeys layers (keys ["ctrl-q", "m"]))
  assertEqual
    "global key in the group"
    (Bound Clear)
    (lookupKeys layers (keys ["ctrl-q", "c"]))

test_actionShadowsPrefix :: Assertion
test_actionShadowsPrefix = do
  let layers = Layers (Just $ keymap [("ctrl-q", BindAction Quit)]) (Just globalKeymap)
  assertEqual "action" (Bound Quit) (lookupKey layers (key "ctrl-q"))

test_sequences :: Assertion
test_sequences = do
  let layers = Layers Nothing (Just globalKeymap)
  case lookupKey layers (key "ctrl-q") of
    Pending next -> do
      assertEqual "group name" (Just "queue") (layersName next)
      assertEqual "next key" (Bound Clear) (lookupKey next (key "c"))
      assertEqual "unbound next key" Unbound (lookupKey next (key "z"))
    other -> assertFailure $ "expected a prefix, got " <> show other
  assertEqual "too many keys" Unbound (lookupKeys layers (keys ["p", "p"]))

test_whichKey :: Assertion
test_whichKey = do
  let queueGroup =
        named
          "queue extras"
          [("m", BindAction (MoveSelection MoveSelectionToCursor)), ("c", BindAction Crop)]
      layers = Layers (Just (keymap [("ctrl-q", BindPrefix queueGroup)])) (Just globalKeymap)
  case lookupKey layers (key "ctrl-q") of
    Pending next -> do
      assertEqual "the screen's name first" (Just "queue extras") (layersName next)
      assertEqual
        "entries"
        [ WhichKeyEntry (key "c") "crop to the selection" False True
        , WhichKeyEntry (key "m") "move selection above the cursor" False True
        , WhichKeyEntry (key "s") "shuffle" False False
        ]
        (whichKeyEntries next)
    other -> assertFailure $ "expected a prefix, got " <> show other

----------------------------------------
-- Helpers

globalKeymap :: Keymap
globalKeymap =
  keymap
    [ ("p", BindAction Pause)
    ,
      ( "ctrl-q"
      , BindPrefix (named "queue" [("c", BindAction Clear), ("s", BindAction Shuffle)])
      )
    ]

keymap :: [(T.Text, Binding)] -> Keymap
keymap bs = Keymap Nothing $ M.fromList [(key k, b) | (k, b) <- bs]

named :: T.Text -> [(T.Text, Binding)] -> Keymap
named n bs = Keymap (Just n) (keymap bs).bindings

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec

keys :: [T.Text] -> [KeySpec]
keys = map key
