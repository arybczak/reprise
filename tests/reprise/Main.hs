module Main (main) where

import Test.Tasty

import Reprise.Test.Action
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
import Reprise.Test.Lyrics
import Reprise.Test.Lyrics.Http
import Reprise.Test.Lyrics.Lrclib
import Reprise.Test.Lyrics.Tekstowo
import Reprise.Test.Main
import Reprise.Test.Mpd.Address
import Reprise.Test.Mpd.Mirror
import Reprise.Test.Mpd.Worker
import Reprise.Test.Save
import Reprise.Test.Screen.Browser
import Reprise.Test.Screen.Help
import Reprise.Test.Screen.Outputs
import Reprise.Test.Screen.Queue
import Reprise.Test.SongInfo
import Reprise.Test.Style
import Reprise.Test.UI.Layout
import Reprise.Test.Visualizer
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
      , addressTests
      , browserTests
      , collationTests
      , configTests
      , fileTests
      , findTests
      , formatTests
      , handlerTests
      , helpTests
      , historyTests
      , httpTests
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
