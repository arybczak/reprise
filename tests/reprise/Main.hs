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
import HistoryTests
import KeymapTests
import KeysTests
import LayerTests
import LayoutTests
import LineEditTests
import LrclibTests
import LyricsTests
import MirrorTests
import OutputsTests
import QueueTests
import Reprise.Width
import SaveTests
import SongInfoTests
import StyleTests
import TekstowoTests
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
      , historyTests
      , keymapTests
      , keysTests
      , layerTests
      , layoutTests
      , lineEditTests
      , lrclibTests
      , lyricsTests
      , mirrorTests
      , outputsTests
      , queueTests
      , saveTests
      , songInfoTests
      , styleTests
      , tekstowoTests
      , visualizerTests
      , widthTests
      , workerTests
      ]
