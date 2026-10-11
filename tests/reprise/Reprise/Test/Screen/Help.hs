module Reprise.Test.Screen.Help (helpTests) where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Event
import Reprise.Keymap
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Help
import Reprise.State
import Reprise.Test.Handler.Helpers
import Reprise.Test.Helpers

helpTests :: TestTree
helpTests =
  testGroup
    "help"
    [ testCase "sections and groups" test_sections
    , testCase "key column" test_keyColumn
    , testCase "the help screen keeps the queue's position" test_helpKeepsPosition
    , testCase "back from the screens about a song or the keys" test_backKeys
    , testCase "the help screen scrolls" test_helpScrolls
    , testCase "a verb that the help screen lacks" test_helpLacksVerb
    , testCase "follow playing while the help screen shows" test_followBehindHelp
    , testCase "the help screen doesn't jump to playing" test_noJumpFromHelp
    ]

test_sections :: Assertion
test_sections =
  assertEqual
    "lines"
    [ Heading 0 "Global"
    , Entry 1 "ctrl-q" "quit, also in a prompt"
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

test_backKeys :: Assertion
test_backKeys = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  forM_ [["f1", "f1"], ["f1", "escape"], ["l", "escape"], ["i", "escape"]] $ \ks -> do
    r <- keys ks s
    assertEqual (T.unpack (T.unwords ks)) QueueScreen (focusedView r.state).screen

test_helpKeepsPosition :: Assertion
test_helpKeepsPosition = do
  s <- testState (80, 24) (statusOf Stopped Nothing 30) (songs 30)
  help <- keys ["end", "f1"] s
  assertEqual "help" HelpScreen (focusedView help.state).screen
  assertEqual "help starts at the top" 0 (focusedView help.state).offset
  back <- keys ["1"] help.state
  assertEqual "queue" QueueScreen (focusedView back.state).screen
  assertEqual "cursor" 29 (cursor back)
  assertEqual "offset" 10 (focusedView back.state).offset

test_helpScrolls :: Assertion
test_helpScrolls = do
  s <- keys ["f1"] =<< testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  let offsetAfter ks = snd . position <$> keys ks s.state
  assertEqual "down" 1 =<< offsetAfter ["down"]
  assertEqual "not above the top" 0 =<< offsetAfter ["up"]
  assertEqual "page" 20 =<< offsetAfter ["page_down"]
  end <- offsetAfter ["end"]
  assertEqual "not past the end" end =<< offsetAfter ["end", "down"]
  assertBool "the last page" (end > 20)

test_helpLacksVerb :: Assertion
test_helpLacksVerb = do
  s <- keys ["f1"] =<< testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  r <- keys ["delete"] s.state
  assertEqual "nothing deleted" [] r.requests
  assertEqual "message" (Just "The help screen has no delete") (message r)

test_followBehindHelp :: Assertion
test_followBehindHelp = do
  s <-
    keys ["t", "f", "f1"] =<< testState (80, 24) (statusOf Playing (Just 0) 5) (songs 5)
  r <- runEvents 0 [StatusFetched (statusOf Playing (Just 3) 5)] s.state
  assertEqual "help stays" HelpScreen (focusedView r.state).screen
  assertEqual "help doesn't move" 0 (focusedView r.state).offset
  assertEqual "the queue follows" 3 . cursor =<< keys ["1"] r.state

test_noJumpFromHelp :: Assertion
test_noJumpFromHelp = do
  s <- keys ["f1"] =<< testState (80, 24) (statusOf Playing (Just 4) 5) (songs 5)
  r <- keys ["o"] s.state
  assertEqual "help" HelpScreen (focusedView r.state).screen

----------------------------------------
-- Helpers

keymap :: Maybe T.Text -> [(T.Text, Binding)] -> Keymap
keymap name bs = Keymap name $ M.fromList [(key k, b) | (k, b) <- bs]
