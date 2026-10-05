module Main (main) where

import Test.Tasty

import ActionTests
import AddressTests
import BrowserTests
import CollationTests
import ConfigTests
import FindTests
import FormatTests
import HandlerTests
import HelpTests
import KeymapTests
import KeysTests
import LayerTests
import LayoutTests
import LineEditTests
import MirrorTests
import QueueTests
import Reprise.Width
import StyleTests
import VisualizerTests
import WidthTests
import WorkerTests

main :: IO ()
main = do
  -- As reprise does at the start.
  installWidthTable
  defaultMain $
    testGroup
      "reprise"
      [ actionTests
      , addressTests
      , browserTests
      , collationTests
      , configTests
      , findTests
      , formatTests
      , handlerTests
      , helpTests
      , keymapTests
      , keysTests
      , layerTests
      , layoutTests
      , lineEditTests
      , mirrorTests
      , queueTests
      , styleTests
      , visualizerTests
      , widthTests
      , workerTests
      ]
