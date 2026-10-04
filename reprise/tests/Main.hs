module Main (main) where

import Test.Tasty

import ActionTests
import AddressTests
import ConfigTests
import FormatTests
import HandlerTests
import HelpTests
import KeymapTests
import KeysTests
import LayoutTests
import MirrorTests
import StyleTests
import WidthTests
import WorkerTests

import Reprise.Width

main :: IO ()
main = do
  -- As reprise does at the start.
  installWidthTable
  defaultMain $
    testGroup
      "reprise"
      [ actionTests
      , addressTests
      , configTests
      , formatTests
      , handlerTests
      , helpTests
      , keymapTests
      , keysTests
      , layoutTests
      , mirrorTests
      , styleTests
      , widthTests
      , workerTests
      ]
