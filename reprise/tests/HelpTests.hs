module HelpTests (helpTests) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Keymap
import Reprise.Keys
import Reprise.Screen.Help

helpTests :: TestTree
helpTests =
  testGroup
    "Help"
    [ testCase "sections and groups" test_sections
    , testCase "key column" test_keyColumn
    ]

test_sections :: Assertion
test_sections =
  assertEqual
    "lines"
    [ Heading "Global"
    , Entry "p" "pause or resume"
    , Blank
    , Heading "ctrl-t: toggle"
    , Entry "ctrl-t r" "toggle repeat"
    , Blank
    , Heading "ctrl-t ctrl-x"
    , Entry "ctrl-t ctrl-x x" "toggle crossfade 5"
    , Blank
    , Heading "Queue"
    , Entry "space" "toggle selection, move down"
    ]
    (helpLines keymaps)
  where
    keymaps :: Keymaps
    keymaps =
      Keymaps
        { global =
            keymap
              Nothing
              [ ("p", BindAction Pause)
              ,
                ( "ctrl-t"
                , BindPrefix $
                    keymap
                      (Just "toggle")
                      [ ("r", BindAction (Toggle ToggleRepeat))
                      , ("ctrl-x", BindPrefix (keymap Nothing [("x", BindAction (Toggle (ToggleCrossfade 5)))]))
                      ]
                )
              ]
        , screens =
            M.fromList
              [
                ( QueueScreen
                , keymap Nothing [("space", BindAction (Select (SelectItem (Just MoveDown))))]
                )
              , (BrowserScreen, emptyKeymap)
              ]
        }

test_keyColumn :: Assertion
test_keyColumn =
  assertEqual
    "width"
    8
    (keyColumnWidth [Heading "a long heading", Entry "ctrl-t r" "x", Entry "p" "y"])

----------------------------------------
-- Helpers

keymap :: Maybe T.Text -> [(T.Text, Binding)] -> Keymap
keymap name bs = Keymap name $ M.fromList [(key k, b) | (k, b) <- bs]

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec
