module CommandTests (commandTests) where

import Test.Tasty
import Test.Tasty.HUnit

import MPD.Command
import MPD.Protocol.Request
import MPD.Protocol.Response
import MPD.Types

commandTests :: TestTree
commandTests =
  testGroup
    "Command"
    [ testCase "pure sends nothing" test_pure
    , testCase "requests combine in order" test_requestsInOrder
    , testCase "each part goes to its command" test_partsInOrder
    , testCase "a command list reply needs list_OK" test_listNeedsListOk
    , testCase "too many parts" test_tooManyParts
    , testCase "arguments" test_arguments
    ]

test_pure :: Assertion
test_pure = do
  assertEqual "no requests" [] (commandRequests (pure ()))
  assertEqual "result without a reply" (Right 'x') (parseCommandReply (pure 'x') [])

test_requestsInOrder :: Assertion
test_requestsInOrder =
  assertEqual
    "requests"
    [Request "status" [], Request "play" ["3"], Request "currentsong" []]
    (commandRequests $ (,,) <$> status <*> play (Just 3) <*> currentSong)

test_partsInOrder :: Assertion
test_partsInOrder = do
  let cmd = (,) <$> addId "a.flac" Nothing <*> addId "b.flac" Nothing
  assertEqual
    "ids"
    (Right (SongId 1, SongId 2))
    (parseCommandReply cmd [[Field "Id" "1"], [Field "Id" "2"], []])

test_listNeedsListOk :: Assertion
test_listNeedsListOk = do
  let cmd = stop *> clear
  assertEqual
    "no trailing part"
    (Left $ ProtocolError "a command list reply doesn't end with list_OK")
    (parseCommandReply cmd [[], [Field "a" "b"]])

test_tooManyParts :: Assertion
test_tooManyParts =
  assertEqual
    "a single command with two parts"
    (Left $ ProtocolError "the reply has more parts than commands")
    (parseCommandReply stop [[], []])

test_arguments :: Assertion
test_arguments = do
  assertEqual
    "relative position"
    [Request "add" ["a", "+0"]]
    (commandRequests $ add "a" (Just $ AfterCurrent 0))
  assertEqual
    "range"
    [Request "delete" ["2:5"]]
    (commandRequests $ delete (Range 2 (Just 5)))
  assertEqual
    "open range"
    [Request "shuffle" ["2:"]]
    (commandRequests $ shuffle (Just $ Range 2 Nothing))
  assertEqual
    "seek"
    [Request "seekcur" ["-1.5"]]
    (commandRequests $ seekCur (SeekBackward 1.5))
  assertEqual "volume up" [Request "volume" ["+2"]] (commandRequests $ changeVolume 2)
  assertEqual "volume down" [Request "volume" ["-2"]] (commandRequests $ changeVolume (-2))
  assertEqual
    "single"
    [Request "single" ["oneshot"]]
    (commandRequests $ setSingle SingleOneshot)
