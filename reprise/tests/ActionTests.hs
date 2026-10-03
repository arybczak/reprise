module ActionTests (actionTests) where

import Data.Either
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action

actionTests :: TestTree
actionTests =
  testGroup
    "Action"
    [ testCase "parse" test_parse
    , testCase "render then parse" test_renderParse
    , testCase "errors" test_errors
    ]

test_parse :: Assertion
test_parse = do
  assertEqual "relative volume" (Right $ Volume (VolumeBy 2)) (parseAction "volume +2")
  assertEqual "absolute volume" (Right $ Volume (VolumeTo 40)) (parseAction "volume 40")
  assertEqual "seek" (Right $ Seek (SeekBy (-10))) (parseAction "seek -10s")
  assertEqual "seek to a time" (Right $ Seek (SeekToSecond 90)) (parseAction "seek 1:30")
  assertEqual
    "seek to a percentage"
    (Right $ Seek (SeekToPercent 50))
    (parseAction "seek 50%")
  assertEqual
    "screen list"
    (Right $ NextScreen [BrowserScreen, MediaLibraryScreen])
    (parseAction "next_screen [browser, media_library]")
  assertEqual
    "argument with a number"
    (Right $ Toggle (ToggleCrossfade 5))
    (parseAction "toggle crossfade 5")
  assertEqual "spaces" (Right Pause) (parseAction "  pause  ")

test_renderParse :: Assertion
test_renderParse =
  mapM_
    (\a -> assertEqual (show a) (Right a) (parseAction (renderAction a)))
    [ Move MoveNextAlbum
    , Select (SelectItem Nothing)
    , Select (SelectItem (Just MoveUp))
    , Select SelectFound
    , Seek (SeekBy 1)
    , Seek (SeekBy (-1))
    , Seek (SeekToSecond 3725)
    , Seek (SeekToPercent 10)
    , Volume (VolumeBy (-2))
    , Volume (VolumeTo 0)
    , Show OutputsScreen
    , PreviousScreen [BrowserScreen]
    , Add AddNext
    , Toggle (ToggleCrossfade 5)
    , Toggle ToggleAlbumSeparators
    , Update UpdateAll
    , MoveSelection MoveSelectionToEnd
    , Priority Nothing
    , Priority (Just 5)
    , CommandPrompt
    ]

test_errors :: Assertion
test_errors = do
  assertEqual
    "suggestion"
    (Left "unknown action pasue, did you mean pause?")
    (parseAction "pasue")
  assertEqual
    "usage"
    (Left "volume: expected an argument; usage: volume +N | -N | N")
    (parseAction "volume")
  assertBool "volume out of range" (isLeft $ parseAction "volume 101")
  assertBool "priority out of range" (isLeft $ parseAction "priority 256")
  assertBool "unknown screen" (isLeft $ parseAction "show tag_editor")
  assertBool "seek without a unit" (isLeft $ parseAction "seek +5")
  assertBool "unclosed list" (isLeft $ parseAction "next_screen [browser")
  assertEqual "empty" (Left "expected an action") (parseAction "")
