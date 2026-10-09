module Main (main) where

import Test.Tasty

import CommandTests
import ConnectionTests
import RequestTests
import ResponseTests
import TestMain

main :: IO ()
main =
  testMain $
    testGroup
      "mpd-protocol"
      [ commandTests
      , connectionTests
      , closedTests
      , timeoutTests
      , requestTests
      , responseTests
      ]
