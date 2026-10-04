module HandlerTests (handlerTests) where

import Data.Foldable
import Data.Set qualified as S
import Data.Text qualified as T
import MPD.Protocol.Request
import MPD.Types
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.State
import Utils

handlerTests :: TestTree
handlerTests =
  testGroup
    "Handler"
    [ testCase "the cursor starts at the playing song" test_jumpAtStart
    , testCase "pause and resume" test_pause
    , testCase "a key sequence" test_keySequence
    , testCase "cancel a key sequence" test_cancelSequence
    , testCase "an unbound next key" test_unboundNextKey
    , testCase "clear asks first" test_clearConfirm
    , testCase "volume without a mixer" test_noMixer
    , testCase "seeking" test_seek
    , testCase "a stale seek timer" test_staleSeek
    , testCase "cursor movement" test_cursorMovement
    , testCase "album navigation" test_albumNavigation
    , testCase "activate plays the song under the cursor" test_activate
    , testCase "queue changes" test_queueChanges
    , testCase "follow the playing song" test_followPlaying
    , testCase "an MPD error shows in the status bar" test_mpdError
    , testCase "a redraw for the elapsed time" test_tick
    , testCase "a redraw for a stream without a length" test_tickWithoutDuration
    , testCase "a lost connection clears the player status" test_disconnectClears
    , testCase "jump to playing centers the cursor" test_jumpCenters
    , testCase "the help screen keeps the queue's position" test_helpKeepsPosition
    , testCase "the help screen scrolls" test_helpScrolls
    , testCase "a verb that the help screen lacks" test_helpLacksVerb
    , testCase "follow playing while the help screen shows" test_followBehindHelp
    , testCase "jump to playing from the help screen" test_jumpFromHelp
    , testCase "select songs one by one" test_selectItem
    , testCase "select a range" test_selectRange
    , testCase "invert and clear the selection" test_selectInvertNone
    , testCase "select an album" test_selectAlbum
    , testCase "select an artist" test_selectArtist
    , testCase "prompts for a value" test_promptAnswers
    , testCase "a prompt takes the keys" test_promptKeys
    , testCase "find as you type" test_findAsYouType
    , testCase "find the next and the previous match" test_findAgain
    , testCase "select the found songs" test_selectFound
    , testCase "the help screen has no selection" test_selectOnHelp
    , testCase "songs that leave the queue leave the selection" test_selectionPruned
    , testCase "delete the marked songs" test_delete
    , testCase "move the marked songs up and down" test_moveSelection
    , testCase "move the selection to the cursor and the end" test_moveSelectionTo
    , testCase "shuffle the selection" test_shuffleSelection
    ]

test_selectItem :: Assertion
test_selectItem = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
      r = keys ["space", "space"] s
  assertEqual "selected and moved" (ids [1, 2]) r.state.queueState.selection
  assertEqual "cursor" 2 (focusedView r.state).cursor
  assertEqual
    "toggled off"
    (ids [])
    (keys ["insert", "insert"] s).state.queueState.selection

test_selectRange :: Assertion
test_selectRange = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
      r = keys ["down", "insert", "down", "down", "down", "insert", "ctrl-s", "r"] s
  assertEqual "filled" (ids [2 .. 5]) r.state.queueState.selection
  let moving = keys ["down", "space", "down", "space", "ctrl-s", "r"] s
  assertEqual "selected while moving" (ids [2 .. 4]) moving.state.queueState.selection
  let ten = testState (80, 24) (statusOf Stopped Nothing 10) (songs 10)
      apart =
        keys
          [ "space"
          , "space"
          , "down"
          , "down"
          , "down"
          , "insert"
          , "down"
          , "down"
          , "insert"
          , "ctrl-s"
          , "r"
          ]
          ten
  assertEqual
    "away from an earlier selection"
    (ids [1, 2, 6, 7, 8])
    apart.state.queueState.selection
  let deselected =
        keys
          ["insert", "down", "down", "insert", "down", "down", "insert", "insert", "ctrl-s", "r"]
          ten
  assertEqual
    "between the first and the last without two ends"
    (ids [1, 2, 3])
    deselected.state.queueState.selection
  assertEqual
    "without a selection"
    (Just "Select the first and the last song of the range first")
    ((.text) <$> (keys ["ctrl-s", "r"] s).state.message)

test_selectInvertNone :: Assertion
test_selectInvertNone = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual
    "inverted"
    (ids [2 .. 5])
    (keys ["insert", "ctrl-s", "i"] s).state.queueState.selection
  assertEqual
    "cleared"
    (ids [])
    (keys ["space", "space", "ctrl-s", "c"] s).state.queueState.selection

test_selectAlbum :: Assertion
test_selectAlbum = do
  let album a pos = song pos [(Artist, ["A"]), (Album, [a])] 60
      q = [album "x" 0, album "x" 1, album "y" 2, album "y" 3, album "x" 4]
      s = testState (80, 24) (statusOf Stopped Nothing 5) q
      r = keys ["down", "down", "down", "ctrl-s", "a"] s
  assertEqual "album" (ids [3, 4]) r.state.queueState.selection

test_promptAnswers :: Assertion
test_promptAnswers = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      answer ks text = keys (ks <> typed text <> ["enter"]) s
      requested ks text = (answer ks text).requests
  assertEqual "set volume" [[Request "setvol" ["40"]]] (requested ["ctrl-p", "v"] "40")
  assertEqual "seek to" [[Request "seekcur" ["90"]]] (requested ["ctrl-p", "s"] "1:30")
  assertEqual "set crossfade" [[Request "crossfade" ["5"]]] (requested ["ctrl-p", "x"] "5")
  assertEqual "priority" [[Request "prioid" ["7", "1"]]] (requested ["ctrl-q", "p"] "7")
  assertEqual
    "add a path"
    [[Request "add" ["a/b.flac"]]]
    (requested ["ctrl-a", "/"] "a/b.flac")
  assertEqual "run a command" [[Request "setvol" ["30"]]] (requested [":"] "volume 30")
  assertEqual
    "the start of the line"
    (Just (Prompt ":" (Line (LineEdit "volume " "") ForCommand)))
    (keys ["ctrl-p", "v"] s).state.prompt
  let invalid = answer ["ctrl-p", "v"] "140"
  assertEqual "invalid" [] invalid.requests
  assertEqual
    "error"
    (Just ("volume: a volume is from 0 to 100; usage: volume +N | -N | N", True))
    ((\m -> (m.text, m.isError)) <$> invalid.state.message)
  assertEqual "closed" Nothing invalid.state.prompt
  assertEqual
    "unknown command"
    (Just True)
    ((.isError) <$> (answer [":"] "nope").state.message)
  assertEqual
    "empty"
    ([], Nothing)
    (let r = answer [":"] "" in (r.requests, r.state.message))

test_promptKeys :: Assertion
test_promptKeys = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      r = keys [":", "q", "p"] s
  assertEqual "no quit" [] [() | Halt <- r.commands]
  assertEqual "no pause" [] r.requests
  assertEqual
    "the line"
    (Just (Prompt ":" (Line (LineEdit "qp" "") ForCommand)))
    r.state.prompt
  let cancelled = keys ["escape"] r.state
  assertEqual "cancelled" (Nothing, []) (cancelled.state.prompt, cancelled.requests)

test_findAsYouType :: Assertion
test_findAsYouType = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) titled
      cursorAfter ks = (focusedView (keys ks s).state).cursor
      note ks = case (keys ks s).state.prompt of
        Just (Prompt _ (Line _ (ForFind f))) -> f.note
        _ -> Just "no find"
  assertEqual "a match after the cursor" 3 (cursorAfter ("/" : typed "al"))
  assertEqual "while typing" 2 (cursorAfter ("/" : typed "g"))
  assertEqual "diacritics" 4 (cursorAfter ("/" : typed "pokoj"))
  assertEqual "backward around the start" 3 (cursorAfter ("?" : typed "al"))
  assertEqual "the note" (Just "wrapped around to the bottom") (note ("?" : typed "al"))
  assertEqual
    "no match"
    (0, Just "no match")
    (cursorAfter ("/" : typed "x"), note ("/" : typed "x"))
  assertEqual "an incomplete pattern" (Just "incomplete pattern") (note ("/" : typed "("))
  assertEqual "a cancel goes back" 0 (cursorAfter ("/" : typed "g" <> ["escape"]))
  assertEqual "backspace goes back" 0 (cursorAfter ("/" : typed "g" <> ["backspace"]))
  let accepted = keys ("/" : typed "al" <> ["enter"]) s
  assertEqual
    "kept"
    (3, Nothing)
    ((focusedView accepted.state).cursor, accepted.state.prompt)
  assertEqual "the pattern" (Just "al") accepted.state.queueState.findPattern
  assertEqual
    "on the help screen"
    (Just "The help screen has no find forward")
    ((.text) <$> (keys ["f1", "/"] s).state.message)

test_findAgain :: Assertion
test_findAgain = do
  let s =
        keys ("/" : typed "al" <> ["enter"]) $
          testState (80, 24) (statusOf Stopped Nothing 5) titled
      findAfter ks = let r = keys ks s.state in ((focusedView r.state).cursor, (.text) <$> r.state.message)
  assertEqual "next" (0, Just "Wrapped around to the top") (findAfter ["."])
  assertEqual "previous" (0, Nothing) (findAfter [","])
  assertEqual
    "previous twice"
    (3, Just "Wrapped around to the bottom")
    (findAfter [",", ","])
  assertEqual
    "an empty find repeats"
    (0, Just "Wrapped around to the top")
    (findAfter ["/", "enter"])
  let fresh = testState (80, 24) (statusOf Stopped Nothing 5) titled
  assertEqual
    "nothing yet"
    (Just "Nothing was found yet")
    ((.text) <$> (keys ["."] fresh).state.message)

test_selectFound :: Assertion
test_selectFound = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) titled
      r = keys ("/" : typed "al" <> ["enter", "ctrl-s", "f"]) s
  assertEqual "selected" (ids [1, 4]) r.state.queueState.selection
  assertEqual "message" (Just "2 songs found and selected") ((.text) <$> r.state.message)

-- | Songs with titles to find.
titled :: [Song]
titled =
  [ song i [(Artist, ["A"]), (Title, [t])] 60
  | (i, t) <- zip [0 ..] ["alpha", "beta", "Gamma", "alpha two", "Pokój"]
  ]

-- | The keys that type the text.
typed :: T.Text -> [T.Text]
typed = map (\c -> if c == ' ' then "space" else T.singleton c) . T.unpack

test_selectArtist :: Assertion
test_selectArtist = do
  let by a pos = song pos [(Artist, [a]), (Album, [T.pack (show pos)])] 60
      q = [by "x" 0, by "y" 1, by "y" 2, by "y" 3, by "x" 4]
      s = testState (80, 24) (statusOf Stopped Nothing 5) q
      r = keys ["down", "down", "ctrl-s", "A"] s
  assertEqual "artist" (ids [2, 3, 4]) r.state.queueState.selection
  assertEqual
    "message"
    (Just "Artist around the cursor selected")
    ((.text) <$> r.state.message)
  let various a pos = song pos [(AlbumArtist, ["Various"]), (Artist, [a])] 60
      compilation = [by "x" 0, various "y" 1, various "z" 2, various "w" 3, by "x" 4]
      c = testState (80, 24) (statusOf Stopped Nothing 5) compilation
  assertEqual
    "a compilation"
    (ids [2, 3, 4])
    (keys ["down", "down", "ctrl-s", "A"] c).state.queueState.selection
  assertEqual
    "the next artist after a compilation"
    4
    (focusedView (keys ["down", "}"] c).state).cursor

test_selectOnHelp :: Assertion
test_selectOnHelp = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
      r = keys ["f1", "insert"] s
  assertEqual "message" (Just "The help screen has no select") ((.text) <$> r.state.message)
  assertEqual "nothing selected" (ids []) r.state.queueState.selection

test_selectionPruned :: Assertion
test_selectionPruned = do
  let s = keys ["space", "space"] $ testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
      st = statusOf Stopped Nothing 1 & #playlistVersion .~ PlaylistVersion 2
      r = runEvents 0 [QueueChangesFetched (st, [])] s.state
  assertEqual "selection" (ids [1]) r.state.queueState.selection

test_delete :: Assertion
test_delete = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual
    "selected, from the end"
    [[Request "delete" ["2:3"], Request "delete" ["0:1"]]]
    (keys ["space", "down", "space", "delete"] s).requests
  assertEqual
    "under the cursor"
    [[Request "delete" ["1:2"]]]
    (keys ["down", "delete"] s).requests

test_moveSelection :: Assertion
test_moveSelection = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
      up = keys ["down", "insert", "m"] s
  assertEqual "up" [[Request "move" ["0:1", "1"]]] up.requests
  assertEqual "the cursor follows up" 0 (focusedView up.state).cursor
  let down = keys ["down", "insert", "n"] s
  assertEqual "down" [[Request "move" ["2:3", "1"]]] down.requests
  assertEqual "the cursor follows down" 2 (focusedView down.state).cursor
  let elsewhere = keys ["down", "insert", "down", "down", "m"] s
  assertEqual "the cursor stays" 3 (focusedView elsewhere.state).cursor
  let unselected = keys ["down", "down", "m"] s
  assertEqual "under the cursor" [[Request "move" ["1:2", "2"]]] unselected.requests
  let top = keys ["insert", "m"] s
  assertEqual "the top stays" [] top.requests
  assertEqual "with the cursor" 0 (focusedView top.state).cursor

test_moveSelectionTo :: Assertion
test_moveSelectionTo = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
      message ks = (.text) <$> (keys ks s).state.message
  assertEqual
    "above the cursor"
    [[Request "move" ["0:1", "3"]]]
    (keys ["insert", "end", "ctrl-q", "m"] s).requests
  assertEqual
    "among the selected songs"
    (Just "The cursor is among the selected songs")
    (message ["insert", "down", "down", "insert", "up", "ctrl-q", "m"])
  assertEqual
    "without a selection"
    (Just "Select the songs to move first")
    (message ["ctrl-q", "m"])
  assertEqual
    "end"
    [[Request "move" ["0:1", "4"]]]
    (keys ["insert", "ctrl-q", "e"] s).requests

test_shuffleSelection :: Assertion
test_shuffleSelection = do
  let s = testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
      r = keys ["space", "space", "ctrl-q", "s"] s
  assertEqual "next to each other" [[Request "shuffle" ["0:2"]]] r.requests
  assertEqual "message" (Just "Shuffled 2 songs") ((.text) <$> r.state.message)
  let apart = keys ["space", "down", "space", "ctrl-q", "s"] s
  assertEqual "apart" [] apart.requests
  assertEqual "error" (Just True) ((.isError) <$> apart.state.message)

test_disconnectClears :: Assertion
test_disconnectClears = do
  let s = keys ["f"] $ testState (80, 24) (statusOf Playing (Just 1) 3) (songs 3)
      r = runEvents 0 [MpdDisconnected "gone"] s.state
  assertEqual "no status" Nothing r.state.mirror.status
  assertEqual "no seek" Nothing ((.target) <$> r.state.seek)
  assertEqual "the queue stays" 3 (length r.state.mirror.queue)
  let paused = keys ["p"] r.state
  assertEqual "no request" [] paused.requests
  assertEqual "message" (Just "Not connected to MPD") ((.text) <$> paused.state.message)

test_jumpCenters :: Assertion
test_jumpCenters = do
  -- 24 rows leave 20 for the list.
  let playingAt p = testState (80, 24) (statusOf Playing (Just p) 50) (songs 50)
      position r = ((focusedView r.state).cursor, (focusedView r.state).offset)
  assertEqual "at the start" (30, 20) (position (keys [] (playingAt 30)))
  assertEqual "after moving away" (30, 20) (position (keys ["home", "o"] (playingAt 30)))
  assertEqual "near the top" (3, 0) (position (keys ["end", "o"] (playingAt 3)))
  assertEqual "near the bottom" (48, 30) (position (keys ["home", "o"] (playingAt 48)))
  let behindHelp =
        runEvents 0 [StatusFetched (statusOf Playing (Just 40) 50)] $
          (keys ["ctrl-t", "f", "f1"] (playingAt 30)).state
  assertEqual "behind the help screen" (40, 30) (position (keys ["1"] behindHelp.state))

test_helpKeepsPosition :: Assertion
test_helpKeepsPosition = do
  let s = testState (80, 24) (statusOf Stopped Nothing 30) (songs 30)
      help = keys ["end", "f1"] s
  assertEqual "help" HelpScreen (focusedView help.state).screen
  assertEqual "help starts at the top" 0 (focusedView help.state).offset
  let back = keys ["1"] help.state
  assertEqual "queue" QueueScreen (focusedView back.state).screen
  assertEqual "cursor" 29 (focusedView back.state).cursor
  assertEqual "offset" 10 (focusedView back.state).offset

test_helpScrolls :: Assertion
test_helpScrolls = do
  let s = keys ["f1"] $ testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
      offsetAfter ks = (focusedView (keys ks s.state).state).offset
  assertEqual "down" 1 (offsetAfter ["down"])
  assertEqual "not above the top" 0 (offsetAfter ["up"])
  assertEqual "page" 20 (offsetAfter ["page_down"])
  let end = offsetAfter ["end"]
  assertEqual "not past the end" end (offsetAfter ["end", "down"])
  assertBool "the last page" (end > 20)

test_helpLacksVerb :: Assertion
test_helpLacksVerb = do
  let s = keys ["f1"] $ testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      r = keys ["delete"] s.state
  assertEqual "nothing deleted" [] r.requests
  assertEqual "message" (Just "The help screen has no delete") ((.text) <$> r.state.message)

test_followBehindHelp :: Assertion
test_followBehindHelp = do
  let s = keys ["ctrl-t", "f", "f1"] $ testState (80, 24) (statusOf Playing (Just 0) 5) (songs 5)
      r = runEvents 0 [StatusFetched (statusOf Playing (Just 3) 5)] s.state
  assertEqual "help stays" HelpScreen (focusedView r.state).screen
  assertEqual "help doesn't move" 0 (focusedView r.state).offset
  assertEqual "the queue follows" 3 (focusedView (keys ["1"] r.state).state).cursor

test_jumpFromHelp :: Assertion
test_jumpFromHelp = do
  let s = keys ["f1"] $ testState (80, 24) (statusOf Playing (Just 4) 5) (songs 5)
      r = keys ["up", "o"] s.state
  assertEqual "queue" QueueScreen (focusedView r.state).screen
  assertEqual "cursor" 4 (focusedView r.state).cursor

test_jumpAtStart :: Assertion
test_jumpAtStart = do
  let s = testState (80, 24) (statusOf Playing (Just 3) 5) (songs 5)
  assertEqual "cursor" 3 (focusedView s).cursor

test_pause :: Assertion
test_pause = do
  let playing = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      paused = testState (80, 24) (statusOf Paused (Just 0) 3) (songs 3)
      stopped = testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  assertEqual "pause" [[Request "pause" ["1"]]] (keys ["p"] playing).requests
  assertEqual "resume" [[Request "pause" ["0"]]] (keys ["p"] paused).requests
  assertEqual "play" [[Request "play" []]] (keys ["p"] stopped).requests

test_keySequence :: Assertion
test_keySequence = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      pending = keys ["ctrl-t"] s
  assertEqual "pending" (Just [key "ctrl-t"]) ((.keys) <$> pending.state.pendingKeys)
  assertEqual "nothing yet" [] pending.requests
  let done = keys ["ctrl-t", "r"] s
  assertEqual "toggle repeat" [[Request "repeat" ["1"]]] done.requests
  assertEqual "no longer pending" Nothing ((.keys) <$> done.state.pendingKeys)

test_cancelSequence :: Assertion
test_cancelSequence = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      r = keys ["ctrl-t", "escape", "r"] s
  assertEqual "nothing ran" [] r.requests
  assertEqual "no longer pending" Nothing ((.keys) <$> r.state.pendingKeys)

test_unboundNextKey :: Assertion
test_unboundNextKey = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      r = keys ["ctrl-t", "j"] s
  assertEqual "message" (Just "ctrl-t j is not bound") ((.text) <$> r.state.message)

test_clearConfirm :: Assertion
test_clearConfirm = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      asked = keys ["ctrl-q", "c"] s
  assertEqual
    "question"
    (Just "Clear 3 songs from the queue?")
    ((.question) <$> asked.state.prompt)
  assertEqual "nothing yet" [] asked.requests
  assertEqual "other keys wait" [] (keys ["x"] asked.state).requests
  assertEqual "yes" [[Request "clear" []]] (keys ["y"] asked.state).requests
  let no = keys ["n"] asked.state
  assertEqual "no" [] no.requests
  assertEqual "closed" Nothing no.state.prompt

test_noMixer :: Assertion
test_noMixer = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3 & #volume .~ Nothing) (songs 3)
      r = keys ["+"] s
  assertEqual "no request" [] r.requests
  assertEqual "error" (Just True) ((.isError) <$> r.state.message)

test_seek :: Assertion
test_seek = do
  let s = testState (80, 24) (statusOf Paused (Just 0) 3) (songs 3)
      r = runEvents 0 [key' "f", key' "f", key' "f"] s
  assertEqual "no seek while the key is held" [] r.requests
  assertEqual "the target moves" (Just 13) ((.target) <$> r.state.seek)
  let timers = [(d, e) | After d e@(SeekCommit _) <- r.commands]
  case reverse timers of
    (_, lastTimer) : _ -> do
      let done = runEvents 0 [lastTimer] r.state
      assertEqual "one seek" [[Request "seekcur" ["13"]]] done.requests
      assertEqual "finished" Nothing done.state.seek
    [] -> assertFailure "no timer"

test_staleSeek :: Assertion
test_staleSeek = do
  let s = testState (80, 24) (statusOf Paused (Just 0) 3) (songs 3)
      r = runEvents 0 [key' "f", key' "f"] s
  case [e | After _ e@(SeekCommit _) <- r.commands] of
    first : _ : _ -> assertEqual "stale" [] (runEvents 0 [first] r.state).requests
    _ -> assertFailure "expected two timers"

test_cursorMovement :: Assertion
test_cursorMovement = do
  -- 24 rows leave 20 for the list.
  let s = testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
      cursorAfter ks = (focusedView (keys ks s).state).cursor
  assertEqual "down" 1 (cursorAfter ["down"])
  assertEqual "up stops at the top" 0 (cursorAfter ["up"])
  assertEqual "page down" 20 (cursorAfter ["page_down"])
  assertEqual "end" 49 (cursorAfter ["end"])
  assertEqual "down stops at the bottom" 49 (cursorAfter ["end", "down"])
  let v = focusedView (keys ["end"] s).state
  assertEqual "scrolled" 30 v.offset

test_albumNavigation :: Assertion
test_albumNavigation = do
  let album a pos = song pos [(Artist, ["A"]), (Album, [a])] 60
      q = [album "x" 0, album "x" 1, album "y" 2, album "y" 3, album "z" 4]
      s = testState (80, 24) (statusOf Stopped Nothing 5) q
      cursorAfter ks = (focusedView (keys ks s).state).cursor
  assertEqual "next" 2 (cursorAfter ["]"])
  assertEqual "next twice" 4 (cursorAfter ["]", "]"])
  assertEqual "start of the album" 2 (cursorAfter ["down", "down", "down", "["])
  assertEqual "previous album" 0 (cursorAfter ["down", "down", "["])
  -- 24 rows leave 20 for the list.
  let albums = [album (T.pack (show (i `div` 5))) i | i <- [0 .. 49]]
      far =
        focusedView . (.state) . keys (replicate 6 "]") $
          testState (80, 24) (statusOf Stopped Nothing 50) albums
  assertEqual "a jump centers the cursor" (30, 20) (far.cursor, far.offset)

test_activate :: Assertion
test_activate = do
  let s = testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  assertEqual "play by id" [[Request "playid" ["2"]]] (keys ["down", "enter"] s).requests

test_queueChanges :: Assertion
test_queueChanges = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      changed = runEvents 0 [MpdChanged [PlaylistSubsystem]] s
  assertEqual "request" [[Request "status" [], Request "plchanges" ["1"]]] changed.requests
  let st = statusOf Playing (Just 0) 2 & #playlistVersion .~ PlaylistVersion 2
      moved = song 2 [] 60 & #position ?~ SongPos 1
      applied = runEvents 0 [QueueChangesFetched (st, [moved])] changed.state
  assertEqual
    "queue"
    [Just (SongId 1), Just (SongId 3)]
    (map (.songId) (toList applied.state.mirror.queue))
  assertEqual "version" (Just (PlaylistVersion 2)) applied.state.mirror.queueVersion

test_followPlaying :: Assertion
test_followPlaying = do
  let s = keys ["ctrl-t", "f"] $ testState (80, 24) (statusOf Playing (Just 0) 5) (songs 5)
      r = runEvents 0 [StatusFetched (statusOf Playing (Just 3) 5)] s.state
  assertEqual "cursor" 3 (focusedView r.state).cursor

test_mpdError :: Assertion
test_mpdError = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      ack = Ack AckArg 0 "play" "Bad song index"
      r = runEvents 0 [MpdFailed [Request "play" ["99"]] (AckError ack)] s
  assertEqual "message" (Just "play: Bad song index") ((.text) <$> r.state.message)

test_tick :: Assertion
test_tick = do
  let s = testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
      r = runEvents 0 [StatusFetched (statusOf Playing (Just 0) 3 & #elapsed ?~ 10.25)] s
      ticks = [d | After d (Tick _) <- r.commands]
  -- A 60 s song on an 80 column bar moves a cell every 0.75 s: the next
  -- cell starts at 10.5 s, before the next second.
  case ticks of
    [d] -> assertBool ("delay " <> show d) (abs (d - 0.25) < 1e-9)
    _ -> assertFailure $ "expected one tick, got " <> show ticks

test_tickWithoutDuration :: Assertion
test_tickWithoutDuration = do
  let s = testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
      stream = statusOf Playing (Just 0) 3 & #elapsed ?~ 10.25 & #duration .~ Nothing
      r = runEvents 0 [StatusFetched stream] s
      ticks = [d | After d (Tick _) <- r.commands]
  -- Without a progress bar to move, the next change is the next second.
  case ticks of
    [d] -> assertBool ("delay " <> show d) (abs (d - 0.75) < 1e-9)
    _ -> assertFailure $ "expected one tick, got " <> show ticks

----------------------------------------
-- Helpers

songs :: Int -> [Song]
songs n = [song i [(Artist, ["A"]), (Title, [T.pack (show i)])] 60 | i <- [0 .. n - 1]]

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec

key' :: T.Text -> AppEvent
key' = KeyPressed . key

keys :: [T.Text] -> AppState -> Result
keys ks = runEvents 0 (map key' ks)

-- | The ids of the songs that 'songs' makes, one more than their positions.
ids :: [Int] -> S.Set SongId
ids = S.fromList . map SongId
