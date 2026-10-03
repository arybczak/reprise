module Main (main) where

import Test.Tasty

import CommandTests
import ConnectionTests
import RequestTests
import ResponseTests

main :: IO ()
main =
  defaultMain $
    testGroup
      "mpd-protocol"
      [ commandTests
      , connectionTests
      , requestTests
      , responseTests
      ]
