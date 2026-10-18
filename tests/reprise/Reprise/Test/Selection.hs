module Reprise.Test.Selection (selectionTests) where

import Data.Set qualified as S
import Data.Text qualified as T
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Event
import Reprise.Mpd.Protocol.Types
import Reprise.Selection
import Reprise.State
import Reprise.Test.Handler.Helpers
import Reprise.Test.Helpers

selectionTests :: TestTree
selectionTests =
  testGroup
    "selection"
    [ testCase "select songs one by one" test_selectItem
    , testCase "select a range" test_selectRange
    , testCase "invert and clear the selection" test_selectInvertNone
    , testCase "select an album" test_selectAlbum
    , testCase "select an artist" test_selectArtist
    , testCase "select the found songs" test_selectFound
    , testCase "the help screen has no selection" test_selectOnHelp
    , testCase "songs that leave the queue leave the selection" test_selectionPruned
    , testCase "a new connection clears the selection" test_selectionReconnect
    ]

test_selectItem :: Assertion
test_selectItem = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  r <- keys ["space", "space"] s
  assertEqual "selected and moved" (ids [1, 2]) r.state.queueState.selection.keys
  assertEqual "cursor" 2 (focusedView r.state).cursor
  assertEqual "toggled off" (ids []) . selected =<< keys ["insert", "insert"] s

test_selectRange :: Assertion
test_selectRange = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual "filled" (ids [2 .. 5]) . selected
    =<< keys ["down", "insert", "down", "down", "down", "insert", "v", "r"] s
  assertEqual "selected while moving" (ids [2 .. 4]) . selected
    =<< keys ["down", "space", "down", "space", "v", "r"] s
  ten <- testState (80, 24) (statusOf Stopped Nothing 10) (songs 10)
  assertEqual "away from an earlier selection" (ids [1, 2, 6, 7, 8]) . selected
    =<< keys
      [ "space"
      , "space"
      , "down"
      , "down"
      , "down"
      , "insert"
      , "down"
      , "down"
      , "insert"
      , "v"
      , "r"
      ]
      ten
  assertEqual "between the first and the last without two ends" (ids [1, 2, 3]) . selected
    =<< keys
      ["insert", "down", "down", "insert", "down", "down", "insert", "insert", "v", "r"]
      ten
  assertEqual
    "without a selection"
    (Just "Select the first and the last item of the range first")
    . message
    =<< keys ["v", "r"] s

test_selectInvertNone :: Assertion
test_selectInvertNone = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual "inverted" (ids [2 .. 5]) . selected =<< keys ["insert", "v", "i"] s
  assertEqual "cleared" (ids []) . selected =<< keys ["space", "space", "V"] s

test_selectAlbum :: Assertion
test_selectAlbum = do
  let album a pos = song pos [(Artist, ["A"]), (Album, [a])] 60
      q = [album "x" 0, album "x" 1, album "y" 2, album "y" 3, album "x" 4]
  s <- testState (80, 24) (statusOf Stopped Nothing 5) q
  assertEqual "album" (ids [3, 4]) . selected
    =<< keys ["down", "down", "down", "v", "a"] s
  assertEqual "not the same album further on" (ids [1, 2]) . selected =<< keys ["v", "a"] s

test_selectFound :: Assertion
test_selectFound = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  r <- keys ("/" : typed "al" <> ["enter", "v", "f"]) s
  assertEqual "selected" (ids [1, 4]) (selected r)
  assertEqual "message" (Just "2 songs found and selected") (message r)

test_selectArtist :: Assertion
test_selectArtist = do
  let by a pos = song pos [(Artist, [a]), (Album, [T.pack (show pos)])] 60
      q = [by "x" 0, by "y" 1, by "y" 2, by "y" 3, by "x" 4]
  s <- testState (80, 24) (statusOf Stopped Nothing 5) q
  r <- keys ["down", "down", "v", "A"] s
  assertEqual "artist" (ids [2, 3, 4]) (selected r)
  assertEqual "message" (Just "Artist around the cursor selected") (message r)
  assertEqual "not the same artist further on" (ids [1]) . selected =<< keys ["v", "A"] s
  let various a pos = song pos [(AlbumArtist, ["Various"]), (Artist, [a])] 60
      compilation = [by "x" 0, various "y" 1, various "z" 2, various "w" 3, by "x" 4]
  c <- testState (80, 24) (statusOf Stopped Nothing 5) compilation
  assertEqual "a compilation" (ids [2, 3, 4]) . selected
    =<< keys ["down", "down", "v", "A"] c
  assertEqual "the next artist after a compilation" 4 . cursor =<< keys ["down", "}"] c

test_selectOnHelp :: Assertion
test_selectOnHelp = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  r <- keys ["f1", "insert"] s
  assertEqual "message" (Just "The help screen has no select") (message r)
  assertEqual "nothing selected" (ids []) (selected r)

-- | MPD gives the ids anew when it restarts, so a selected id could be of
-- another song after a new connection.
test_selectionReconnect :: Assertion
test_selectionReconnect = do
  s <- keys ["space", "space"] =<< testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual "selected" (ids [1, 2]) (selected s)
  assertEqual "cleared" (ids []) . selected
    =<< runEvents 0 [MpdConnected (Version 0 24 0)] s.state

test_selectionPruned :: Assertion
test_selectionPruned = do
  s <- keys ["space", "space"] =<< testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  let st = statusOf Stopped Nothing 1 & #playlistVersion .~ PlaylistVersion 2
  assertEqual "selection" (ids [1]) . selected
    =<< runEvents 0 [QueueChangesFetched (st, [])] s.state

----------------------------------------
-- Helpers

-- | The ids of the songs that 'songs' makes, one more than their positions.
ids :: [Int] -> S.Set SongId
ids = S.fromList . map SongId

selected :: Result -> S.Set SongId
selected r = r.state.queueState.selection.keys
