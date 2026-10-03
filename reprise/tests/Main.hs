module Main (main) where

import Test.Tasty

import ActionTests
import AddressTests
import ConfigTests
import FormatTests
import HandlerTests
import KeymapTests
import KeysTests
import LayoutTests
import MirrorTests
import StyleTests

main :: IO ()
main =
  defaultMain $
    testGroup
      "reprise"
      [ actionTests
      , addressTests
      , configTests
      , formatTests
      , handlerTests
      , keymapTests
      , keysTests
      , layoutTests
      , mirrorTests
      , styleTests
      ]
