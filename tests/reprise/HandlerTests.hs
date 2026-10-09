module HandlerTests (handlerTests) where

import Data.Foldable
import Data.Set qualified as S
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Selection
import Reprise.State
import Reprise.UI.Layout
import Utils

handlerTests :: TestTree
handlerTests =
  testGroup
    "Handler"
    [ testCase "the cursor starts at the playing song" test_jumpAtStart
    , testCase "pause and resume" test_pause
    , testCase "replay" test_replay
    , testCase "a key sequence" test_keySequence
    , testCase "cancel a key sequence" test_cancelSequence
    , testCase "an unbound next key" test_unboundNextKey
    , testCase "next and previous screen" test_nextScreen
    , testCase "the startup screen shows as a key shows it" test_startupScreen
    , testCase "a priority for an empty queue" test_priorityEmpty
    , testCase "clear asks first" test_clearConfirm
    , testCase "volume without a mixer" test_noMixer
    , testCase "seeking" test_seek
    , testCase "a seek is of its song" test_seekOfItsSong
    , testCase "a stale seek timer" test_staleSeek
    , testCase "one timer hides the cursor" test_cursorTimer
    , testCase
        "following the playing song in the queue and the lyrics"
        test_followPlayingScreens
    , testCase "the display of the queue and the browser" test_displayScreens
    , testCase "stale timers keep the screen" test_staleTimers
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
    , testCase "the mouse wheel" test_wheel
    , testCase "the help screen keeps the queue's position" test_helpKeepsPosition
    , testCase "back from the screens about a song or the keys" test_backKeys
    , testCase "the help screen scrolls" test_helpScrolls
    , testCase "a verb that the help screen lacks" test_helpLacksVerb
    , testCase "follow playing while the help screen shows" test_followBehindHelp
    , testCase "the help screen doesn't jump to playing" test_noJumpFromHelp
    , testCase "select songs one by one" test_selectItem
    , testCase "select a range" test_selectRange
    , testCase "invert and clear the selection" test_selectInvertNone
    , testCase "select an album" test_selectAlbum
    , testCase "select an artist" test_selectArtist
    , testCase "prompts for a value" test_promptAnswers
    , testCase "ctrl-q always quits" test_alwaysQuit
    , testCase "a prompt takes the keys" test_promptKeys
    , testCase "ask for the password" test_passwordPrompt
    , testCase "the prompts share a history" test_promptHistory
    , testCase "find as you type" test_findAsYouType
    , testCase "find the next and the previous match" test_findAgain
    , testCase "find after the rows change" test_findRowsChange
    , testCase "select the found songs" test_selectFound
    , testCase "the help screen has no selection" test_selectOnHelp
    , testCase "songs that leave the queue leave the selection" test_selectionPruned
    , testCase "delete the marked songs" test_delete
    , testCase "move the marked songs up and down" test_moveSongs
    , testCase "move songs to a place" test_moveSongsTo
    , testCase "shuffle the selection" test_shuffleSelection
    , testCase "shuffling the whole queue asks first" test_shuffleConfirm
    , testCase "update current outside the browser" test_updateCurrentElsewhere
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

test_promptAnswers :: Assertion
test_promptAnswers = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  let answer ks text = keys (ks <> typed text <> ["enter"]) s
      requested ks text = (.requests) <$> answer ks text
  assertEqual "seek to" [[Request "seekcur" ["90"]]] =<< requested ["g", "s"] "1:30"
  assertEqual "set crossfade" [[Request "crossfade" ["5"]]]
    =<< requested ["t", "X"] "5"
  assertEqual "priority" [[Request "prioid" ["7", "1"]]] =<< requested ["e", "p"] "7"
  assertEqual "add a path" [[Request "add" ["a/b.flac"]]]
    =<< requested ["a", "/"] "a/b.flac"
  assertEqual "run a command" [[Request "setvol" ["30"]]] =<< requested [":"] "volume 30"
  assertEqual
    "the start of the line"
    (Just (Prompt ":" (Line (LineEdit "seek " "") ForCommand Nothing)))
    . (.state.prompt)
    =<< keys ["g", "s"] s
  invalid <- answer [":"] "volume 140"
  assertEqual "invalid" [] invalid.requests
  assertEqual
    "error"
    (Just ("volume: a volume is from 0 to 100; usage: volume +N | -N | N", True))
    ((\m -> (m.text, m.isError)) <$> invalid.state.message)
  assertEqual "closed" Nothing invalid.state.prompt
  assertEqual "unknown command" (Just True) . isError =<< answer [":"] "nope"
  assertEqual "empty" ([], Nothing) . (\r -> (r.requests, r.state.message))
    =<< answer [":"] ""

-- | ctrl-q quits on a screen, in a prompt and after a prefix alike.
test_alwaysQuit :: Assertion
test_alwaysQuit = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  forM_ [[], [":", "x"], ["t"]] $ \before -> do
    r <- keys (before <> ["ctrl-q"]) s
    assertEqual (show before) [()] [() | Halt <- r.commands]

test_passwordPrompt :: Assertion
test_passwordPrompt = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  let refusal = AckError $ Ack AckPermission 0 "pause" "you don't have permission for \"pause\""
  asked <- runEvents 0 [PasswordNeeded refusal] =<< (.state) <$> keys ["t"] s
  assertEqual
    "the question"
    (Just "MPD refused pause. Password: ")
    ((.question) <$> asked.state.prompt)
  assertEqual "the pending keys end" Nothing asked.state.pendingKeys
  typing <- keys (typed "secret") asked.state
  assertEqual
    "stars"
    (Just "MPD refused pause. Password: ******")
    (fmap T.stripEnd . lastMaybe . imageLines $ renderScreen testAppEnv typing.state)
  answered <- keys ["enter"] typing.state
  assertEqual "the answer" [Just "secret"] (passwordAnswers answered)
  assertEqual "closed" Nothing answered.state.prompt
  cancelled <- keys ["escape"] typing.state
  assertEqual "a cancel" [Nothing] (passwordAnswers cancelled)
  assertEqual "closed by a cancel" Nothing cancelled.state.prompt
  wrong <-
    runEvents
      0
      [PasswordNeeded . AckError $ Ack AckPassword 0 "password" "incorrect password"]
      s
  assertEqual
    "a wrong password"
    (Just "Wrong password. Password: ")
    ((.question) <$> wrong.state.prompt)
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe = fmap snd . unsnoc

test_promptHistory :: Assertion
test_promptHistory = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  used <-
    keys
      ( [":"]
          <> typed "volume 30"
          <> ["enter", "g", "s"]
          <> typed "1:30"
          <> ["enter"]
          <> ["/"]
          <> typed "song"
          <> ["enter"]
      )
      s
  assertEqual "newest first" ["song", "seek 1:30", "volume 30"] used.state.history
  let lineOf r = case r.state.prompt of
        Just (Prompt _ (Line edit _ _)) -> Just (lineEditText edit)
        _ -> Nothing
  assertEqual "shared" (Just "volume 30") . lineOf
    =<< keys [":", "up", "up", "up"] used.state
  assertEqual "only the lines that start with the typed one" (Just "seek 1:30") . lineOf
    =<< keys ["g", "s", "up", "up"] used.state
  assertEqual "ctrl-p and ctrl-n" (Just "song") . lineOf
    =<< keys ["/", "ctrl-p", "ctrl-p", "ctrl-n"] used.state
  assertEqual "back to the typed line" (Just "vol") . lineOf
    =<< keys ([":"] <> typed "vol" <> ["up", "down"]) used.state
  assertEqual "page up to the oldest" (Just "volume 30") . lineOf
    =<< keys [":", "page_up"] used.state
  assertEqual "page down to the typed line" (Just "s") . lineOf
    =<< keys ([":"] <> typed "s" <> ["up", "up", "page_down"]) used.state
  edited <- keys ([":", "up"] <> typed "x" <> ["up"]) used.state
  assertEqual "an edit starts from the edited line" (Just "songx") (lineOf edited)
  finding <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  assertEqual "a recalled find finds" 3 . cursor
    =<< keys ["/", "up"] (finding & #history .~ ["al"])
  password <-
    keys (typed "secret" <> ["up", "enter"])
      =<< (.state)
        <$> runEvents 0 [PasswordNeeded (AckError (Ack AckPermission 0 "pause" ""))] used.state
  assertEqual "no password in the history" used.state.history password.state.history
  assertEqual "no password in the file" [] [l | SaveToHistory l <- password.commands]
  assertEqual "no history in the password" [Just "secret"] (passwordAnswers password)
  assertEqual
    "the lines go to the file"
    ["volume 30", "seek 1:30", "song"]
    [l | SaveToHistory l <- used.commands]

test_promptKeys :: Assertion
test_promptKeys = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  r <- keys [":", "q", "p"] s
  assertEqual "no quit" [] [() | Halt <- r.commands]
  assertEqual "no pause" [] r.requests
  assertEqual
    "the line"
    (Just (Prompt ":" (Line (LineEdit "qp" "") ForCommand Nothing)))
    r.state.prompt
  cancelled <- keys ["escape"] r.state
  assertEqual "cancelled" (Nothing, []) (cancelled.state.prompt, cancelled.requests)

test_findAsYouType :: Assertion
test_findAsYouType = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  let cursorAfter ks = cursor <$> keys ks s
      note ks = do
        r <- keys ks s
        pure $ case r.state.prompt of
          Just (Prompt _ (Line _ (ForFind f) _)) -> f.note
          _ -> Just "no find"
  assertEqual "a match after the cursor" 3 =<< cursorAfter ("/" : typed "al")
  assertEqual "while typing" 2 =<< cursorAfter ("/" : typed "g")
  assertEqual "diacritics" 4 =<< cursorAfter ("/" : typed "pokoj")
  assertEqual "backward around the start" 3 =<< cursorAfter ("?" : typed "al")
  assertEqual "the note" (Just "wrapped around to the bottom") =<< note ("?" : typed "al")
  assertEqual "no match" (0, Just "no match")
    =<< ((,) <$> cursorAfter ("/" : typed "x") <*> note ("/" : typed "x"))
  assertEqual "an incomplete pattern" (Just "incomplete pattern") =<< note ("/" : typed "(")
  assertEqual "a cancel goes back" 0 =<< cursorAfter ("/" : typed "g" <> ["escape"])
  assertEqual "backspace goes back" 0 =<< cursorAfter ("/" : typed "g" <> ["backspace"])
  accepted <- keys ("/" : typed "al" <> ["enter"]) s
  assertEqual
    "kept"
    (3, Nothing)
    ((focusedView accepted.state).cursor, accepted.state.prompt)
  assertEqual "the pattern" (Just "al") accepted.state.findPattern
  assertEqual "on the help screen" (Just "The help screen has no find forward") . message
    =<< keys ["f1", "/"] s

test_findAgain :: Assertion
test_findAgain = do
  s <-
    keys ("/" : typed "al" <> ["enter"])
      =<< testState (80, 24) (statusOf Stopped Nothing 5) titled
  let findAfter ks = (\r -> (cursor r, message r)) <$> keys ks s.state
  assertEqual "next" (0, Just "Wrapped around to the top") =<< findAfter ["."]
  assertEqual "previous" (0, Nothing) =<< findAfter [","]
  assertEqual "previous twice" (3, Just "Wrapped around to the bottom")
    =<< findAfter [",", ","]
  assertEqual "an empty find repeats" (0, Just "Wrapped around to the top")
    =<< findAfter ["/", "enter"]
  fresh <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  assertEqual "nothing yet" (Just "Nothing was found yet") . message =<< keys ["."] fresh

-- | A find matches the rows of the queue and the display of now, not the
-- ones of an earlier find.
test_findRowsChange :: Assertion
test_findRowsChange = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  missed <- keys ("/" : typed "delta" <> ["enter"]) s
  let added = statusOf Stopped Nothing 6 & #playlistVersion .~ PlaylistVersion 2
  changed <-
    runEvents
      0
      [QueueFetched (added, titled <> [song 5 [(Title, ["delta"])] 60])]
      missed.state
  assertEqual "a song added since" 5 . cursor =<< keys ["."] changed.state
  -- Only the classic display joins the artist and the title with " - ".
  assertEqual "another display" 3 . cursor
    =<< keys ("/" : typed "A - alpha" <> ["enter", "t", "d", "."]) s

test_selectFound :: Assertion
test_selectFound = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) titled
  r <- keys ("/" : typed "al" <> ["enter", "v", "f"]) s
  assertEqual "selected" (ids [1, 4]) (selected r)
  assertEqual "message" (Just "2 songs found and selected") (message r)

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
  s <- testState (80, 24) (statusOf Stopped Nothing 5) q
  r <- keys ["down", "down", "v", "A"] s
  assertEqual "artist" (ids [2, 3, 4]) (selected r)
  assertEqual "message" (Just "Artist around the cursor selected") (message r)
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

test_selectionPruned :: Assertion
test_selectionPruned = do
  s <- keys ["space", "space"] =<< testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  let st = statusOf Stopped Nothing 1 & #playlistVersion .~ PlaylistVersion 2
  assertEqual "selection" (ids [1]) . selected
    =<< runEvents 0 [QueueChangesFetched (st, [])] s.state

test_delete :: Assertion
test_delete = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual
    "selected, from the end"
    [[Request "delete" ["2:3"], Request "delete" ["0:1"]]]
    . (.requests)
    =<< keys ["space", "down", "space", "delete"] s
  assertEqual "under the cursor" [[Request "delete" ["1:2"]]] . (.requests)
    =<< keys ["down", "delete"] s

test_moveSongs :: Assertion
test_moveSongs = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  up <- keys ["down", "insert", "m"] s
  assertEqual "up" [[Request "move" ["0:1", "1"]]] up.requests
  assertEqual "the cursor follows up" 0 (cursor up)
  down <- keys ["down", "insert", "n"] s
  assertEqual "down" [[Request "move" ["2:3", "1"]]] down.requests
  assertEqual "the cursor follows down" 2 (cursor down)
  assertEqual "the cursor stays" 3 . cursor
    =<< keys ["down", "insert", "down", "down", "m"] s
  assertEqual "under the cursor" [[Request "move" ["1:2", "2"]]] . (.requests)
    =<< keys ["down", "down", "m"] s
  top <- keys ["insert", "m"] s
  assertEqual "the top stays" [] top.requests
  assertEqual "with the cursor" 0 (cursor top)

test_moveSongsTo :: Assertion
test_moveSongsTo = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  assertEqual "above the cursor" [[Request "move" ["0:1", "3"]]] . (.requests)
    =<< keys ["insert", "end", "M"] s
  assertEqual "among the selected songs" (Just "The cursor is among the selected songs")
    . message
    =<< keys ["insert", "down", "down", "insert", "up", "M"] s
  assertEqual "without a selection" (Just "Select the songs to move first") . message
    =<< keys ["M"] s
  assertEqual "end" [[Request "move" ["0:1", "4"]]] . (.requests)
    =<< keys ["insert", "e", "m", "e"] s
  assertEqual "beginning" [[Request "move" ["2:3", "0"]]] . (.requests)
    =<< keys ["down", "down", "insert", "e", "m", "b"] s
  assertEqual "next, without a current song" (Just "There is no current song") . message
    =<< keys ["end", "e", "m", "n"] s
  playing <- testState (80, 24) (statusOf Playing (Just 1) 5) (songs 5)
  assertEqual "next, the song under the cursor" [[Request "move" ["4:5", "2"]]]
    . (.requests)
    =<< keys ["end", "e", "m", "n"] playing
  assertEqual
    "the playing song among them"
    (Just "The current song is among the songs to move")
    . message
    =<< keys ["home", "down", "insert", "e", "m", "n"] playing

test_shuffleSelection :: Assertion
test_shuffleSelection = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  r <- keys ["space", "space", "e", "s"] s
  assertEqual "next to each other" [[Request "shuffle" ["0:2"]]] r.requests
  assertEqual "message" (Just "Shuffled 2 songs") (message r)
  apart <- keys ["space", "down", "space", "e", "s"] s
  assertEqual "apart" [] apart.requests
  assertEqual "error" (Just True) (isError apart)

test_shuffleConfirm :: Assertion
test_shuffleConfirm = do
  s <- testState (80, 24) (statusOf Stopped Nothing 5) (songs 5)
  asked <- keys ["e", "s"] s
  assertEqual
    "question"
    (Just "Shuffle 5 songs in the queue?")
    ((.question) <$> asked.state.prompt)
  assertEqual "nothing yet" [] asked.requests
  assertEqual "yes" [[Request "shuffle" []]] . (.requests) =<< keys ["y"] asked.state
  assertEqual "no" [] . (.requests) =<< keys ["n"] asked.state
  elsewhere <- keys ["space", "space", "8", "e", "s"] s
  assertEqual
    "the whole queue from another screen"
    (Just "Shuffle 5 songs in the queue?")
    ((.question) <$> elsewhere.state.prompt)

-- | Outside the browser, there is no current directory.
test_updateCurrentElsewhere :: Assertion
test_updateCurrentElsewhere = do
  r <- keys ["d", "u"] =<< testState (80, 24) (statusOf Stopped Nothing 1) (songs 1)
  assertEqual "the whole database" [[Request "update" []]] r.requests

test_disconnectClears :: Assertion
test_disconnectClears = do
  s <- keys ["f"] =<< testState (80, 24) (statusOf Playing (Just 1) 3) (songs 3)
  r <- runEvents 0 [MpdDisconnected "gone"] s.state
  assertEqual "no status" Nothing r.state.mirror.status
  assertEqual "no seek" Nothing ((.target) <$> r.state.seek)
  assertEqual "the queue stays" 3 (length r.state.mirror.queue)
  paused <- keys ["p"] r.state
  assertEqual "no request" [] paused.requests
  assertEqual "message" (Just "Not connected to MPD") (message paused)

test_jumpCenters :: Assertion
test_jumpCenters = do
  -- 24 rows leave 20 for the list.
  let playingAt p = testState (80, 24) (statusOf Playing (Just p) 50) (songs 50)
      positionAfter ks p = position <$> (keys ks =<< playingAt p)
  assertEqual "at the start" (30, 20) =<< positionAfter [] 30
  assertEqual "after moving away" (30, 20) =<< positionAfter ["home", "o"] 30
  assertEqual "near the top" (3, 0) =<< positionAfter ["end", "o"] 3
  assertEqual "near the bottom" (48, 30) =<< positionAfter ["home", "o"] 48
  helpShown <- keys ["t", "f", "f1"] =<< playingAt 30
  behindHelp <- runEvents 0 [StatusFetched (statusOf Playing (Just 40) 50)] helpShown.state
  assertEqual "behind the help screen" (40, 30) . position =<< keys ["1"] behindHelp.state
  stopped <- testState (80, 24) (statusOf Stopped (Just 30) 50) (songs 50)
  assertEqual "stopped" (30, 20) . position =<< keys ["home", "o"] stopped
  none <- keys ["end", "o"] =<< testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
  assertEqual "no current song" (49, 30) (position none)
  assertEqual "its message" (Just "There is no current song") (message none)

-- | As ncmpcpp's, a step of the wheel moves the cursor of a list by
-- @mouse.scroll_lines@, and scrolls text by them.
test_wheel :: Assertion
test_wheel = do
  s <- testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
  let wheel ts = runEvents 0 (map MouseWheel ts)
  assertEqual "down" (8, 0) . position =<< wheel [MoveDown, MoveDown] s
  assertEqual "up" (4, 0) . position =<< wheel [MoveDown, MoveDown, MoveUp] s
  assertEqual "the cursor shows" 3 . (.state.lastInput)
    =<< runEvents 3 [MouseWheel MoveDown] s
  let two = testAppEnv & #config % #mouse % #scrollLines .~ ScrollLines 2
  assertEqual "the config's lines" (2, 0) . position
    =<< runEventsWith two 0 [MouseWheel MoveDown] s
  help <- keys ["f1"] s
  assertEqual "text scrolls" 4 . (.offset) . focusedView . (.state)
    =<< runEvents 0 [MouseWheel MoveDown] help.state
  prompt <- keys [":"] s
  assertEqual "not in a prompt" (0, 0) . position =<< wheel [MoveDown] prompt.state
  visualizer <- keys ["8"] s
  r <- wheel [MoveDown] visualizer.state
  assertEqual "nothing on a screen without moves" Nothing (message r)
  assertEqual "nor a redraw" [KeepScreen] r.commands

test_backKeys :: Assertion
test_backKeys = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  forM_ [["f1", "f1"], ["f1", "escape"], ["l", "escape"], ["i", "escape"]] $ \ks -> do
    r <- keys ks s
    assertEqual (T.unpack (T.unwords ks)) QueueScreen (focusedView r.state).screen

test_helpKeepsPosition :: Assertion
test_helpKeepsPosition = do
  s <- testState (80, 24) (statusOf Stopped Nothing 30) (songs 30)
  help <- keys ["end", "f1"] s
  assertEqual "help" HelpScreen (focusedView help.state).screen
  assertEqual "help starts at the top" 0 (focusedView help.state).offset
  back <- keys ["1"] help.state
  assertEqual "queue" QueueScreen (focusedView back.state).screen
  assertEqual "cursor" 29 (cursor back)
  assertEqual "offset" 10 (focusedView back.state).offset

test_helpScrolls :: Assertion
test_helpScrolls = do
  s <- keys ["f1"] =<< testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  let offsetAfter ks = snd . position <$> keys ks s.state
  assertEqual "down" 1 =<< offsetAfter ["down"]
  assertEqual "not above the top" 0 =<< offsetAfter ["up"]
  assertEqual "page" 20 =<< offsetAfter ["page_down"]
  end <- offsetAfter ["end"]
  assertEqual "not past the end" end =<< offsetAfter ["end", "down"]
  assertBool "the last page" (end > 20)

test_helpLacksVerb :: Assertion
test_helpLacksVerb = do
  s <- keys ["f1"] =<< testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  r <- keys ["delete"] s.state
  assertEqual "nothing deleted" [] r.requests
  assertEqual "message" (Just "The help screen has no delete") (message r)

test_followBehindHelp :: Assertion
test_followBehindHelp = do
  s <-
    keys ["t", "f", "f1"] =<< testState (80, 24) (statusOf Playing (Just 0) 5) (songs 5)
  r <- runEvents 0 [StatusFetched (statusOf Playing (Just 3) 5)] s.state
  assertEqual "help stays" HelpScreen (focusedView r.state).screen
  assertEqual "help doesn't move" 0 (focusedView r.state).offset
  assertEqual "the queue follows" 3 . cursor =<< keys ["1"] r.state

test_noJumpFromHelp :: Assertion
test_noJumpFromHelp = do
  s <- keys ["f1"] =<< testState (80, 24) (statusOf Playing (Just 4) 5) (songs 5)
  r <- keys ["o"] s.state
  assertEqual "help" HelpScreen (focusedView r.state).screen

test_jumpAtStart :: Assertion
test_jumpAtStart = do
  s <- testState (80, 24) (statusOf Playing (Just 3) 5) (songs 5)
  assertEqual "cursor" 3 (focusedView s).cursor

test_pause :: Assertion
test_pause = do
  playing <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  paused <- testState (80, 24) (statusOf Paused (Just 0) 3) (songs 3)
  stopped <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  assertEqual "pause" [[Request "pause" ["1"]]] . (.requests) =<< keys ["p"] playing
  assertEqual "resume" [[Request "pause" ["0"]]] . (.requests) =<< keys ["p"] paused
  assertEqual "play" [[Request "play" []]] . (.requests) =<< keys ["p"] stopped

test_replay :: Assertion
test_replay = do
  playing <- testState (80, 24) (statusOf Playing (Just 1) 3) (songs 3)
  stopped <- testState (80, 24) (statusOf Stopped (Just 1) 3) (songs 3)
  let replayed = [[Request "seekcur" ["0"]]]
  assertEqual "the queue" replayed . (.requests) =<< keys ["backspace"] playing
  assertEqual "another screen" replayed . (.requests) =<< keys ["f1", "backspace"] playing
  assertEqual "stopped" [[Request "play" ["1"]]] . (.requests)
    =<< keys ["backspace"] stopped

test_keySequence :: Assertion
test_keySequence = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  pending <- keys ["t"] s
  assertEqual "pending" (Just [key "t"]) ((.keys) <$> pending.state.pendingKeys)
  assertEqual "nothing yet" [] pending.requests
  done <- keys ["t", "r"] s
  assertEqual "toggle repeat" [[Request "repeat" ["1"]]] done.requests
  assertEqual "no longer pending" Nothing ((.keys) <$> done.state.pendingKeys)

test_cancelSequence :: Assertion
test_cancelSequence = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  r <- keys ["t", "escape", "z"] s
  assertEqual "nothing ran" [] r.requests
  assertEqual "no longer pending" Nothing ((.keys) <$> r.state.pendingKeys)

test_unboundNextKey :: Assertion
test_unboundNextKey = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  assertEqual "message" (Just "t j is not bound") . message =<< keys ["t", "j"] s

-- | The default list names the screens with numbers, and those that aren't
-- built yet are skipped.
test_nextScreen :: Assertion
test_nextScreen = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  let screenAfter ks = (.screen) . focusedView . (.state) <$> keys ks s
  assertEqual
    "forward, around to the queue"
    [BrowserScreen, OutputsScreen, VisualizerScreen, QueueScreen]
    =<< traverse (\n -> screenAfter (replicate n "tab")) [1 .. 4]
  assertEqual "back" VisualizerScreen =<< screenAfter ["shift-tab"]
  assertEqual "from a screen outside the list" QueueScreen =<< screenAfter ["f1", "tab"]

test_startupScreen :: Assertion
test_startupScreen = do
  browser <- started BrowserScreen
  assertEqual "the browser" BrowserScreen (focusedView browser.state).screen
  assertEqual "its root listed" [[Request "lsinfo" []]] browser.requests
  outputs <- started OutputsScreen
  assertEqual "the outputs fetched" [[Request "outputs" []]] outputs.requests
  unbuilt <- started MediaLibraryScreen
  assertEqual "not a screen to come" QueueScreen (focusedView unbuilt.state).screen
  assertEqual
    "why not"
    (Just "The media library screen isn't available yet")
    ((.text) <$> unbuilt.state.message)
  where
    started :: ScreenName -> IO Result
    started screen =
      runEventsWith
        (testAppEnv & #config % #startupScreen .~ screen)
        0
        [Resized 80 24, Started]
        (initialState defaultConfig)

test_priorityEmpty :: Assertion
test_priorityEmpty = do
  s <- testState (80, 24) (statusOf Stopped Nothing 0) []
  r <- keys (":" : map T.singleton "priority" <> ["space", "5", "enter"]) s
  assertEqual "nothing sent" [] r.requests
  assertEqual "why" (Just "The queue is empty") (message r)

test_clearConfirm :: Assertion
test_clearConfirm = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  asked <- keys ["e", "c"] s
  assertEqual
    "question"
    (Just "Clear 3 songs from the queue?")
    ((.question) <$> asked.state.prompt)
  -- The header's title is bold too, but it isn't a letter.
  assertEqual
    "the letters that pick the options in bold"
    ["y", "n"]
    [ t
    | (a, t) <- imageSpans (renderScreen testAppEnv asked.state)
    , T.length t == 1
    , V.SetTo st <- [V.attrStyle a]
    , V.hasStyle st V.bold
    ]
  assertEqual "nothing yet" [] asked.requests
  assertEqual "other keys wait" [] . (.requests) =<< keys ["x"] asked.state
  assertEqual "yes" [[Request "clear" []]] . (.requests) =<< keys ["y"] asked.state
  no <- keys ["n"] asked.state
  assertEqual "no" [] no.requests
  assertEqual "closed" Nothing no.state.prompt

test_noMixer :: Assertion
test_noMixer = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3 & #volume .~ Nothing) (songs 3)
  r <- keys ["+"] s
  assertEqual "no request" [] r.requests
  assertEqual "error" (Just True) (isError r)

test_seek :: Assertion
test_seek = do
  s <- testState (80, 24) (statusOf Paused (Just 0) 3) (songs 3)
  r <- runEvents 0 [key' "f", key' "f", key' "f"] s
  assertEqual "no seek while the key is held" [] r.requests
  assertEqual "the target moves" (Just 13) ((.target) <$> r.state.seek)
  let timers = [(d, e) | After d e@(SeekCommit _) <- r.commands]
  case reverse timers of
    (_, lastTimer) : _ -> do
      done <- runEvents 0 [lastTimer] r.state
      assertEqual "one seek" [[Request "seekcur" ["13"]]] done.requests
      assertEqual "finished" Nothing done.state.seek
    [] -> assertFailure "no timer"

-- | A seek that waits for its keys to end is of its song: another song, a
-- stop or a replay drops it.
test_seekOfItsSong :: Assertion
test_seekOfItsSong = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  r <- runEvents 0 [key' "f"] s
  commit <- case [e | After _ e@(SeekCommit _) <- r.commands] of
    [e] -> pure e
    _ -> assertFailure "expected one timer"
  next <- runEvents 0 [StatusFetched (statusOf Playing (Just 1) 3)] r.state
  assertEqual "dropped for the next song" Nothing next.state.seek
  assertEqual "nothing sent for it" [] . (.requests) =<< runEvents 0 [commit] next.state
  stopped <- runEvents 0 [key' "s", commit] r.state
  assertEqual "a stop drops it" [[Request "stop" []]] stopped.requests
  replayed <- runEvents 0 [key' "backspace", commit] r.state
  assertEqual "a replay replaces it" [[Request "seekcur" ["0"]]] replayed.requests

test_staleSeek :: Assertion
test_staleSeek = do
  s <- testState (80, 24) (statusOf Paused (Just 0) 3) (songs 3)
  r <- runEvents 0 [key' "f", key' "f"] s
  case [e | After _ e@(SeekCommit _) <- r.commands] of
    first : _ : _ -> assertEqual "stale" [] . (.requests) =<< runEvents 0 [first] r.state
    _ -> assertFailure "expected two timers"

test_cursorTimer :: Assertion
test_cursorTimer = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  pressed <- runEvents 0 [key' "down"] s
  held <- runEvents 3 [key' "down", key' "up"] pressed.state
  assertEqual
    "one timer for a run of keys"
    [5]
    [d | After d HideCursor <- pressed.commands <> held.commands]
  early <- runEvents 5 [HideCursor] held.state
  assertEqual
    "waits for the rest of the delay"
    [After 3 HideCursor, KeepScreen]
    early.commands
  late <- runEvents 8 [HideCursor] early.state
  assertEqual "hides without waiting again" [] late.commands
  assertBool "hidden" (not (cursorVisible late.state))
  next <- runEvents 9 [key' "down"] late.state
  assertEqual "a new timer for the next key" [5] [d | After d HideCursor <- next.commands]

-- | Following the playing song is the queue's and the lyrics screen's.
-- Elsewhere its keys aren't bound, and the action says that the screen has
-- none.
test_followPlayingScreens :: Assertion
test_followPlayingScreens = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  queue <- keys ["t", "f"] s
  assertBool "the queue follows" queue.state.toggles.followPlaying
  browser <- keys ["2"] s
  assertEqual "the browser's keys" (Just "t f is not bound") . message
    =<< keys ["t", "f"] browser.state
  ran <- keys ([":"] <> typed "toggle follow_playing" <> ["enter"]) browser.state
  assertEqual
    "the action in the browser"
    (Just "The browser screen has no toggle follow_playing")
    (message ran)
  assertBool "the queue still doesn't follow" (not ran.state.toggles.followPlaying)

-- | The display is the queue's and the browser's. Elsewhere its keys aren't
-- bound, and the action says that the screen has none.
test_displayScreens :: Assertion
test_displayScreens = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  queue <- keys ["t", "d"] s
  assertEqual "the queue's" Classic queue.state.toggles.queueDisplay
  outputs <- keys ["7"] s
  assertEqual "the outputs' keys" (Just "t d is not bound") . message
    =<< keys ["t", "d"] outputs.state
  ran <- keys ([":"] <> typed "toggle display" <> ["enter"]) outputs.state
  assertEqual
    "the action in the outputs"
    (Just "The outputs screen has no toggle display")
    (message ran)
  assertEqual "the queue's stays" Columns ran.state.toggles.queueDisplay

test_staleTimers :: Assertion
test_staleTimers = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  r <- keys ["t", "f", "t", "f"] s
  case [e | After _ e@(MessageExpired _) <- r.commands] of
    first : second : _ -> do
      assertEqual "stale" [KeepScreen] . (.commands) =<< runEvents 5 [first] r.state
      assertEqual "current" [] . (.commands) =<< runEvents 5 [second] r.state
    _ -> assertFailure "expected two messages"
  assertEqual "a stale tick" [KeepScreen] . (.commands) =<< runEvents 0 [Tick (-1)] s

test_cursorMovement :: Assertion
test_cursorMovement = do
  -- 24 rows leave 20 for the list.
  s <- testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
  let cursorAfter ks = cursor <$> keys ks s
  assertEqual "down" 1 =<< cursorAfter ["down"]
  assertEqual "up stops at the top" 0 =<< cursorAfter ["up"]
  assertEqual "page down" (20, 20) . position =<< keys ["page_down"] s
  let fifthRow = replicate 5 "down"
  assertEqual "the row stays" (25, 20) . position =<< keys (fifthRow <> ["page_down"]) s
  assertEqual "page up" (5, 0) . position =<< keys (fifthRow <> ["page_down", "page_up"]) s
  assertEqual "the start stops it" (0, 0) . position =<< keys (fifthRow <> ["page_up"]) s
  assertEqual
    "the end stops the scrolling"
    (45, 30)
    . position
    =<< keys (fifthRow <> ["page_down", "page_down"]) s
  assertEqual "the last item" (49, 30) . position =<< keys (replicate 3 "page_down") s
  assertEqual "end" 49 =<< cursorAfter ["end"]
  assertEqual "down stops at the bottom" 49 =<< cursorAfter ["end", "down"]
  assertEqual "scrolled" 30 . snd . position =<< keys ["end"] s

test_albumNavigation :: Assertion
test_albumNavigation = do
  let album a pos = song pos [(Artist, ["A"]), (Album, [a])] 60
      q = [album "x" 0, album "x" 1, album "y" 2, album "y" 3, album "z" 4]
  s <- testState (80, 24) (statusOf Stopped Nothing 5) q
  let cursorAfter ks = cursor <$> keys ks s
  assertEqual "next" 2 =<< cursorAfter ["]"]
  assertEqual "next twice" 4 =<< cursorAfter ["]", "]"]
  assertEqual "start of the album" 2 =<< cursorAfter ["down", "down", "down", "["]
  assertEqual "previous album" 0 =<< cursorAfter ["down", "down", "["]
  -- 24 rows leave 20 for the list.
  let albums = [album (T.pack (show (i `div` 5))) i | i <- [0 .. 49]]
  assertEqual "a jump centers the cursor" (30, 20) . position
    =<< keys (replicate 6 "]")
    =<< testState (80, 24) (statusOf Stopped Nothing 50) albums

test_activate :: Assertion
test_activate = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  assertEqual "play by id" [[Request "playid" ["2"]]] . (.requests)
    =<< keys ["down", "enter"] s

test_queueChanges :: Assertion
test_queueChanges = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  changed <- runEvents 0 [MpdChanged [PlaylistSubsystem]] s
  assertEqual "request" [[Request "status" [], Request "plchanges" ["1"]]] changed.requests
  let st = statusOf Playing (Just 0) 2 & #playlistVersion .~ PlaylistVersion 2
      moved = song 2 [] 60 & #position ?~ SongPos 1
  applied <- runEvents 0 [QueueChangesFetched (st, [moved])] changed.state
  assertEqual
    "queue"
    [Just (SongId 1), Just (SongId 3)]
    (map (.songId) (toList applied.state.mirror.queue))
  assertEqual "version" (Just (PlaylistVersion 2)) applied.state.mirror.queueVersion

test_followPlaying :: Assertion
test_followPlaying = do
  s <- keys ["t", "f"] =<< testState (80, 24) (statusOf Playing (Just 0) 5) (songs 5)
  assertEqual "cursor" 3 . cursor
    =<< runEvents 0 [StatusFetched (statusOf Playing (Just 3) 5)] s.state

test_mpdError :: Assertion
test_mpdError = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  let ack = Ack AckArg 0 "play" "Bad song index"
  assertEqual "message" (Just "play: Bad song index") . message
    =<< runEvents 0 [MpdFailed [Request "play" ["99"]] (AckError ack)] s

test_tick :: Assertion
test_tick = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  r <- runEvents 0 [StatusFetched (statusOf Playing (Just 0) 3 & #elapsed ?~ 10.25)] s
  let ticks = [d | After d (Tick _) <- r.commands]
  -- A 60 s song on an 80 column bar moves a cell every 0.75 s: the next
  -- cell starts at 10.5 s, before the next second.
  case ticks of
    [d] -> assertBool ("delay " <> show d) (abs (d - 0.25) < 1e-9)
    _ -> assertFailure $ "expected one tick, got " <> show ticks

test_tickWithoutDuration :: Assertion
test_tickWithoutDuration = do
  s <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  let stream = statusOf Playing (Just 0) 3 & #elapsed ?~ 10.25 & #duration .~ Nothing
  r <- runEvents 0 [StatusFetched stream] s
  let ticks = [d | After d (Tick _) <- r.commands]
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

keys :: [T.Text] -> AppState -> IO Result
keys ks = runEvents 0 (map key' ks)

-- | The ids of the songs that 'songs' makes, one more than their positions.
ids :: [Int] -> S.Set SongId
ids = S.fromList . map SongId

selected :: Result -> S.Set SongId
selected r = r.state.queueState.selection.keys

cursor :: Result -> Int
cursor r = (focusedView r.state).cursor

-- | The cursor and the offset of the focused view.
position :: Result -> (Int, Int)
position r = ((focusedView r.state).cursor, (focusedView r.state).offset)

message :: Result -> Maybe T.Text
message r = (.text) <$> r.state.message

isError :: Result -> Maybe Bool
isError r = (.isError) <$> r.state.message
