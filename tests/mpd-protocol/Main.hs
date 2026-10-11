module Main (main) where

import Test.Tasty

import Reprise.Test.Main
import Reprise.Test.Mpd.Protocol.Command
import Reprise.Test.Mpd.Protocol.Connection
import Reprise.Test.Mpd.Protocol.Request
import Reprise.Test.Mpd.Protocol.Response

main :: IO ()
main =
  testMain $
    testGroup
      "mpd-protocol"
      [ commandTests
      , connectionTests
      , requestTests
      , responseTests
      ]
