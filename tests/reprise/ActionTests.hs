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
    , testCase "the hint of the : prompt" test_hint
    ]

test_parse :: Assertion
test_parse = do
  assertEqual "relative volume" (Right $ Volume (VolumeBy 2)) (parseAction "volume +2")
  assertEqual "absolute volume" (Right $ Volume (VolumeTo 40)) (parseAction "volume 40")
  assertEqual "seek" (Right $ Seek (SeekBy (-10))) (parseAction "seek -10s")
  assertEqual "seek to a time" (Right $ Seek (SeekToSecond 90)) (parseAction "seek 1:30")
  -- A time too long for an Int would wrap around to a negative one, which
  -- MPD reads as a seek back from where the song is. MPD gets the time as it
  -- is written instead, and tells what it makes of it.
  case parseAction "seek 153722867280912931:00" of
    Right (Seek (SeekToSecond n)) ->
      assertEqual "a time past an Int" (153722867280912931 * 60) (toInteger n)
    other -> assertFailure $ "not a seek to a time: " <> show other
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
    , MoveSongs MoveSongsToEnd
    , Priority 5
    , Crossfade 3
    , CommandPrompt ""
    , CommandPrompt "seek"
    , AddPath "A/[2001] B/01 c  d.flac"
    ]

test_hint :: Assertion
test_hint = do
  assertEqual "the usage" "seek +Ns | -Ns | [h:]m:ss | N%" (actionHint "seek ")
  assertEqual "an hour" "seek to 1:02:05" (actionHint "seek 1:02:05")
  assertEqual
    "written as it reads"
    (Right "seek 1:02:05")
    (renderAction <$> parseAction "seek 62:05")
  assertEqual "while the arguments are wrong" "volume +N | -N | N" (actionHint "volume 1x")
  assertEqual "what it will do" "seek to 1:30" (actionHint "seek 1:30")
  assertEqual "an action without arguments" "quit" (actionHint "quit")
  assertEqual "not an action yet" "" (actionHint "se")

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
  assertEqual
    "a screen of a song in a list"
    ( Left
        "next_screen: a list of screens takes only the screens with numbers, not lyrics; \
        \usage: next_screen [SCREEN, ...]"
    )
    (parseAction "next_screen [queue, lyrics]")
  assertBool "help in a list" (isLeft $ parseAction "previous_screen [help, queue]")
  assertEqual "empty" (Left "expected an action") (parseAction "")
