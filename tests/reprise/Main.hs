module Main (main) where

import Test.Tasty

import Reprise.Test.Action
import Reprise.Test.App
import Reprise.Test.Collation
import Reprise.Test.Config
import Reprise.Test.File
import Reprise.Test.Find
import Reprise.Test.Format
import Reprise.Test.Handler
import Reprise.Test.History
import Reprise.Test.Keymap
import Reprise.Test.Keys
import Reprise.Test.Layers
import Reprise.Test.LineEdit
import Reprise.Test.LocalFiles
import Reprise.Test.Lyrics
import Reprise.Test.Main
import Reprise.Test.Mpd.Address
import Reprise.Test.Mpd.Mirror
import Reprise.Test.Mpd.Worker
import Reprise.Test.Save
import Reprise.Test.Screen.Browser
import Reprise.Test.Screen.Help
import Reprise.Test.Screen.Lyrics
import Reprise.Test.Screen.Outputs
import Reprise.Test.Screen.Queue
import Reprise.Test.Screen.SongInfo
import Reprise.Test.Screen.Visualizer
import Reprise.Test.Selection
import Reprise.Test.SongInfo
import Reprise.Test.Style
import Reprise.Test.UI.Layout
import Reprise.Test.Visualizer.Spectrum
import Reprise.Test.Visualizer.Wave
import Reprise.Test.Visualizer.Worker
import Reprise.Test.Width
import Reprise.Width

main :: IO ()
main = do
  -- As reprise does at the start.
  installWidthTable
  testMain $
    testGroup
      "reprise"
      [ actionTests
      , appTests
      , collationTests
      , configTests
      , fileTests
      , findTests
      , formatTests
      , handlerTests
      , historyTests
      , keymapTests
      , keysTests
      , layerTests
      , lineEditTests
      , localFilesTests
      , lyricsTests
      , testGroup "mpd" [addressTests, mirrorTests, workerTests]
      , saveTests
      , testGroup
          "screen"
          [ browserTests
          , helpTests
          , lyricsScreenTests
          , outputsTests
          , queueTests
          , songInfoScreenTests
          , visualizerTests
          ]
      , selectionTests
      , songInfoTests
      , styleTests
      , testGroup "ui" [layoutTests]
      , testGroup "visualizer" [spectrumTests, waveTests, visualizerWorkerTests]
      , widthTests
      ]
