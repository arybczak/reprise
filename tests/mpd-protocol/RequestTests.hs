module RequestTests (requestTests) where

import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Mpd.Protocol.Request

requestTests :: TestTree
requestTests =
  testGroup
    "Request"
    [ testCase "quote" test_quote
    , testCase "one request" test_oneRequest
    , testCase "command list" test_commandList
    ]

test_quote :: Assertion
test_quote = do
  assertEqual "plain" "\"abc\"" (render $ quote "abc")
  assertEqual "spaces" "\"a b\"" (render $ quote "a b")
  assertEqual "quotes and backslashes" "\"a\\\"b\\\\c\"" (render $ quote "a\"b\\c")
  assertEqual "UTF-8" "\"\197\188\"" (render $ quote "ż")

test_oneRequest :: Assertion
test_oneRequest = do
  assertEqual "no arguments" "status\n" (render $ renderRequests [Request "status" []])
  assertEqual
    "arguments"
    "add \"a b.flac\" \"+0\"\n"
    (render $ renderRequests [Request "add" ["a b.flac", "+0"]])

test_commandList :: Assertion
test_commandList =
  assertEqual
    "requests in a command list"
    "command_list_ok_begin\nstatus\nplay \"1\"\ncommand_list_end\n"
    (render $ renderRequests [Request "status" [], Request "play" ["1"]])

----------------------------------------
-- Helpers

render :: B.Builder -> BL.ByteString
render = B.toLazyByteString
