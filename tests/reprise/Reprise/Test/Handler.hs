module Reprise.Test.Handler (handlerTests) where

import Data.Foldable
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.Save
import Reprise.State
import Reprise.Test.Handler.Helpers
import Reprise.Test.Helpers
import Reprise.UI.Layout

handlerTests :: TestTree
handlerTests =
  testGroup
    "handler"
    [ testCase "pause and resume" test_pause
    , testCase "replay" test_replay
    , testCase "a key sequence" test_keySequence
    , testCase "cancel a key sequence" test_cancelSequence
    , testCase "an unbound next key" test_unboundNextKey
    , testCase "next and previous screen" test_nextScreen
    , testCase "the startup screen shows as a key shows it" test_startupScreen
    , testCase "clear asks first" test_clearConfirm
    , testCase "volume without a mixer" test_noMixer
    , testCase "seeking" test_seek
    , testCase "seeking without a length" test_seekWithoutLength
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
    , testCase "queue changes" test_queueChanges
    , testCase "an MPD error shows in the status bar" test_mpdError
    , testCase "a redraw for the elapsed time" test_tick
    , testCase "a redraw for a stream without a length" test_tickWithoutDuration
    , testCase "a lost connection clears the player status" test_disconnectClears
    , testCase "the mouse wheel" test_wheel
    , testCase "the mouse wheel over the volume" test_wheelVolume
    , testCase "a click on the player's state pauses" test_clickPause
    , testCase "a click on the progress bar seeks" test_clickSeek
    , testCase "a click on a list" test_clickList
    , testCase "prompts for a value" test_promptAnswers
    , testCase "ctrl-q always quits" test_alwaysQuit
    , testCase "a quit waits for the commands before it" test_quit
    , testCase "a prompt takes the keys" test_promptKeys
    , testCase "ask for the password" test_passwordPrompt
    , testCase "a choice ends a key sequence" test_choiceEndsSequence
    , testCase "the prompts share a history" test_promptHistory
    , testCase "the password prompt waits for an open prompt" test_passwordWaits
    , testCase "update current outside the browser" test_updateCurrentElsewhere
    ]

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
  assertEqual "yes" (editRequests [Request "clear" []]) . (.requests)
    =<< keys ["y"] asked.state
  no <- keys ["n"] asked.state
  assertEqual "no" [] no.requests
  assertEqual "closed" Nothing no.state.prompt
  elsewhere <- keys ["8", "e", "c"] s
  assertEqual "unbound on another screen" (Just "e c is not bound") (message elsewhere)
  typedElsewhere <- keys (["8", ":"] <> typed "clear" <> ["enter"]) s
  assertEqual
    "no question on another screen"
    Nothing
    ((.question) <$> typedElsewhere.state.prompt)
  assertEqual
    "a message on another screen"
    (Just "The visualizer screen has no clear")
    (message typedElsewhere)

test_noMixer :: Assertion
test_noMixer = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3 & #volume .~ Nothing) (songs 3)
  r <- keys ["+"] s
  assertEqual "no request" [] r.requests
  assertEqual "error" (Just True) (isError r)

-- | A seek needs a song that plays and its length: a stopped player has no
-- song to seek in, and a stream has no length.
test_seekWithoutLength :: Assertion
test_seekWithoutLength = do
  stopped <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  assertEqual "stopped" (Just "Nothing plays") . message =<< keys ["f"] stopped
  let stream = statusOf Playing (Just 0) 3 & #duration .~ Nothing
  playing <- testState (80, 24) stream (songs 3)
  assertEqual "a stream" (Just "The current song has no length") . message
    =<< keys ["f"] playing

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
  let stream =
        statusOf Playing (Just 0) 3
          & #elapsed ?~ 10.25
          & #duration .~ Nothing
  r <- runEvents 0 [StatusFetched stream] s
  let ticks = [d | After d (Tick _) <- r.commands]
  -- Without a progress bar to move, the next change is the next second.
  case ticks of
    [d] -> assertBool ("delay " <> show d) (abs (d - 0.75) < 1e-9)
    _ -> assertFailure $ "expected one tick, got " <> show ticks

-- | Outside the browser, there is no current directory.
test_updateCurrentElsewhere :: Assertion
test_updateCurrentElsewhere = do
  r <- keys ["u"] =<< testState (80, 24) (statusOf Stopped Nothing 1) (songs 1)
  assertEqual "the whole database" [[Request "update" []]] r.requests
  assertBool "the whole database, also in the browser"
    . elem [Request "update" []]
    . (.requests)
    =<< keys ["2", "U"] r.state

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

-- | As ncmpcpp's, a step of the wheel moves the cursor of a list by
-- @mouse.scroll_lines@, and scrolls text by them.
test_wheel :: Assertion
test_wheel = do
  s <- testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
  -- The list of 24 rows is from row 2 to row 21.
  let wheelAt col row ts = runEvents 0 [MouseWheel t col row | t <- ts]
      wheel = wheelAt 10 5
  assertEqual "down" (8, 0) . position =<< wheel [MoveDown, MoveDown] s
  assertEqual "up" (4, 0) . position =<< wheel [MoveDown, MoveDown, MoveUp] s
  assertEqual "at the list's corner" (4, 0) . position =<< wheelAt 79 21 [MoveDown] s
  assertEqual "the cursor shows" 3 . (.state.lastInput)
    =<< runEvents 3 [MouseWheel MoveDown 10 5] s
  let two = testAppEnv & #config % #mouse % #scrollLines .~ ScrollLines 2
  assertEqual "the config's lines" (2, 0) . position
    =<< runEventsWith two 0 [MouseWheel MoveDown 10 5] s
  help <- keys ["f1"] s
  assertEqual "text scrolls" 4 . (.offset) . focusedView . (.state)
    =<< runEvents 0 [MouseWheel MoveDown 10 5] help.state
  forM_ [("the title", 10, 0), ("the header's line", 10, 1), ("the status bar", 10, 23)] $
    \(what, col, row) -> do
      r <- wheelAt col row [MoveDown] s
      assertEqual ("not over " <> what) (0, 0) (position r)
      assertEqual ("no request over " <> what) [] r.requests
  prompt <- keys [":"] s
  assertEqual "not in a prompt" (0, 0) . position =<< wheel [MoveDown] prompt.state
  visualizer <- keys ["8"] s
  r <- wheel [MoveDown] visualizer.state
  assertEqual "nothing on a screen without moves" Nothing (message r)
  assertEqual "nor a redraw" [KeepScreen] r.commands

-- | The progress bar of the 60 s song takes row 22, and each of its 80
-- columns 0.75 s.
test_clickSeek :: Assertion
test_clickSeek = do
  playing <- testState (80, 24) (statusOf Playing (Just 0) 1) (songs 1)
  paused <- testState (80, 24) (statusOf Paused (Just 0) 1) (songs 1)
  stopped <- testState (80, 24) (statusOf Stopped (Just 0) 1) (songs 1)
  let click col row s = runEvents 0 [MouseClick LeftButton col row] s
  r <- click 20 22 playing
  assertEqual "a quarter" [[Request "seekcur" ["15"]]] r.requests
  assertEqual "shown at once" (Just 15) (displayedElapsed r.state)
  assertEqual "the start" [[Request "seekcur" ["0"]]] . (.requests) =<< click 0 22 playing
  assertEqual "paused" [[Request "seekcur" ["30"]]] . (.requests) =<< click 40 22 paused
  assertEqual "stopped" [] . (.requests) =<< click 20 22 stopped
  assertEqual "past the bar" [] . (.requests) =<< click 80 22 playing
  seeking <- keys ["f"] playing
  sought <- click 20 22 seeking.state
  assertEqual "ends a seek of the keys" Nothing sought.state.seek
  assertEqual "the keys' seek isn't sent after it" [] . (.requests)
    =<< runEvents 0 [SeekCommit 0, SeekCommit 1] sought.state

-- | "Playing: " takes the first 9 columns of the status bar, and "Paused: "
-- the first 8.
test_clickPause :: Assertion
test_clickPause = do
  playing <- testState (80, 24) (statusOf Playing (Just 0) 1) (songs 1)
  paused <- testState (80, 24) (statusOf Paused (Just 0) 1) (songs 1)
  stopped <- testState (80, 24) (statusOf Stopped (Just 0) 1) (songs 1)
  let click col row s = (.requests) <$> runEvents 0 [MouseClick LeftButton col row] s
  assertEqual "pause" [[Request "pause" ["1"]]] =<< click 0 23 playing
  assertEqual "the label's end" [[Request "pause" ["1"]]] =<< click 8 23 playing
  assertEqual "resume" [[Request "pause" ["0"]]] =<< click 7 23 paused
  assertEqual "beside the label" [] =<< click 8 23 paused
  assertEqual "another row" [] =<< click 0 21 playing
  assertEqual "stopped" [] =<< click 0 23 stopped
  toggled <- keys ["t", "f"] playing
  assertBool "a message shows" (message toggled /= Nothing)
  assertEqual "not over a message" [] =<< click 0 23 toggled.state

-- | The list of 24 rows is from row 2 to row 21, below the titles of the
-- columns when they show.
test_clickList :: Assertion
test_clickList = do
  s <- testState (80, 24) (statusOf Stopped Nothing 50) (songs 50)
  let click button col row st = runEvents 0 [MouseClick button col row] st
  left <- click LeftButton 10 5 s
  assertEqual "the cursor on the item" (3, 0) (position left)
  assertEqual "nothing else" [] left.requests
  scrolled <- keys ["end"] s
  assertEqual "an item of a scrolled list" (30, 30) . position
    =<< click LeftButton 10 2 scrolled.state
  right <- click RightButton 10 3 s
  assertEqual "a right click moves the cursor" (1, 0) (position right)
  assertEqual "and plays" [[Request "playid" ["2"]]] right.requests
  few <- testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
  assertEqual "below the last item" (0, 0) . position =<< click LeftButton 10 10 few
  let titles = testAppEnv & #config % #songs % #columns % #titles .~ True
  assertEqual "not the titles" (0, 0) . position
    =<< runEventsWith titles 0 [MouseClick LeftButton 10 2] s
  assertEqual "the first item under them" (2, 0) . position
    =<< runEventsWith titles 0 [MouseClick LeftButton 10 5] s
  help <- keys ["f1"] s
  assertEqual "not in text" [KeepScreen] . (.commands) =<< click LeftButton 10 5 help.state
  prompt <- keys [":"] s
  assertEqual "not with a prompt" (0, 0) . position =<< click LeftButton 10 5 prompt.state
  pending <- keys ["g"] s
  assertEqual "not with a key sequence" (0, 0) . position
    =<< click LeftButton 10 5 pending.state

-- | "Volume: 50%" takes the last 11 columns of the title's row.
test_wheelVolume :: Assertion
test_wheelVolume = do
  s <- testState (80, 24) (statusOf Stopped Nothing 1) (songs 1)
  let volume t col = (.requests) <$> runEvents 0 [MouseWheel t col 0] s
  assertEqual "up" [[Request "volume" ["+2"]]] =<< volume MoveUp 79
  assertEqual "down" [[Request "volume" ["-2"]]] =<< volume MoveDown 69
  assertEqual "beside it" [] =<< volume MoveUp 68
  let four = testAppEnv & #config % #mouse % #volumeStep .~ VolumeStep 4
  assertEqual "the config's step" [[Request "volume" ["+4"]]] . (.requests)
    =<< runEventsWith four 0 [MouseWheel MoveUp 79 0] s
  disconnected <- runEvents 0 [MpdDisconnected "gone"] s
  assertEqual "not while disconnected" [] . (.requests)
    =<< runEvents 0 [MouseWheel MoveUp 79 0] disconnected.state

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
    assertEqual (show before) [[]] r.requests
    assertEqual (show before) [()] . halts =<< runEvents 0 [QuitReady] r.state

-- | A quit waits for the requests before it, e.g. of the key right before
-- it, as a command without requests after them. A second quit, or a quit
-- while MPD waits for the password, quits at once.
test_quit :: Assertion
test_quit = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  paused <- keys ["p", "q"] s
  assertEqual "after the pause" [[Request "pause" ["1"]], []] paused.requests
  assertEqual "not yet" [] (halts paused)
  assertEqual "once MPD took the pause" [()] . halts
    =<< runEvents 0 [QuitReady] paused.state
  assertEqual "again" [()] . halts =<< keys ["q"] paused.state
  let refusal = AckError $ Ack AckPermission 0 "pause" "you don't have permission for \"pause\""
  asked <- runEvents 0 [PasswordNeeded refusal] s
  assertEqual "while MPD waits for the password" [()] . halts
    =<< keys ["ctrl-q"] asked.state

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

-- | A choice that MPD's reply opens in the middle of a key sequence ends
-- the sequence, so that a key after the choice doesn't complete it.
test_choiceEndsSequence :: Assertion
test_choiceEndsSequence = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  asked <- runEvents 0 [SaveChecked "list" SaveQueue True] =<< (.state) <$> keys ["t"] s
  assertEqual
    "the choice"
    (Just "The playlist list exists.")
    ((.question) <$> asked.state.prompt)
  assertEqual "the pending keys end" Nothing asked.state.pendingKeys
  replaced <- keys ["r", "r"] asked.state
  assertEqual "no repeat" [] [r | r@(Request "repeat" _) <- concat replaced.requests]

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

-- | The prompt for the password waits for a prompt that is open. The open
-- prompt's request waits for the password with MPD's others, so nothing is
-- lost.
test_passwordWaits :: Assertion
test_passwordWaits = do
  s <- testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
  let refusal = AckError $ Ack AckPermission 0 "pause" "you don't have permission for \"pause\""
      password = Just "MPD refused pause. Password: "
      question r = (.question) <$> r.state.prompt
      saves r = [() | Request "save" _ <- concat r.requests]
  -- A choice that MPD's reply opened.
  choosing <- runEvents 0 [SaveChecked "list" SaveQueue True, PasswordNeeded refusal] s
  assertEqual "the choice stays" (Just "The playlist list exists.") (question choosing)
  replaced <- keys ["r"] choosing.state
  assertEqual "the save" [()] (saves replaced)
  assertEqual "then the password" password (question replaced)
  -- A line that the user types.
  finding <- runEvents 0 [PasswordNeeded refusal] =<< (.state) <$> keys ["/", "x"] s
  assertBool "the find stays" (question finding /= password)
  assertEqual "then the password" password . question =<< keys ["escape"] finding.state

----------------------------------------
-- Helpers

halts :: Result -> [()]
halts r = [() | Halt <- r.commands]
