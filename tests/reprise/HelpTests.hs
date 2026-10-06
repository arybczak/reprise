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
    [ Heading 0 "Global"
    , Entry 1 "p" "pause or resume"
    , Blank
    , Heading 1 "ctrl-t: toggle"
    , Entry 2 "ctrl-t r" "toggle repeat"
    , Blank
    , Heading 2 "ctrl-t ctrl-x"
    , Entry 3 "ctrl-t ctrl-x x" "toggle crossfade 5"
    , Blank
    , Heading 0 "Queue"
    , Entry 1 "space" "toggle selection, move down"
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

-- | The widest key sequence with its indentation, which isn't the longest.
test_keyColumn :: Assertion
test_keyColumn =
  assertEqual
    "width"
    10
    ( keyColumnWidth
        [Heading 0 "a long heading", Entry 1 "ctrl-t r" "x", Entry 3 "t x" "y", Entry 1 "p" "z"]
    )

----------------------------------------
-- Helpers

keymap :: Maybe T.Text -> [(T.Text, Binding)] -> Keymap
keymap name bs = Keymap name $ M.fromList [(key k, b) | (k, b) <- bs]

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec
