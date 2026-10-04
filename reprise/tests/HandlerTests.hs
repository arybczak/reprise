module HandlerTests (handlerTests) where

import Data.Foldable
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
    , testCase "jump to playing centers the cursor" test_jumpCenters
    , testCase "the help screen keeps the queue's position" test_helpKeepsPosition
    , testCase "the help screen scrolls" test_helpScrolls
    , testCase "a verb that the help screen lacks" test_helpLacksVerb
    , testCase "follow playing while the help screen shows" test_followBehindHelp
    , testCase "jump to playing from the help screen" test_jumpFromHelp
    ]

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
        runEvents 0 [StatusFetched (Right (statusOf Playing (Just 40) 50))] $
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
      r = runEvents 0 [StatusFetched (Right (statusOf Playing (Just 3) 5))] s.state
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
      applied = runEvents 0 [QueueChangesFetched (Right (st, [moved]))] changed.state
  assertEqual
    "queue"
    [Just (SongId 1), Just (SongId 3)]
    (map (.songId) (toList applied.state.mirror.queue))
  assertEqual "version" (Just (PlaylistVersion 2)) applied.state.mirror.queueVersion

test_followPlaying :: Assertion
test_followPlaying = do
  let s = keys ["ctrl-t", "f"] $ testState (80, 24) (statusOf Playing (Just 0) 5) (songs 5)
      r = runEvents 0 [StatusFetched (Right (statusOf Playing (Just 3) 5))] s.state
  assertEqual "cursor" 3 (focusedView r.state).cursor

test_mpdError :: Assertion
test_mpdError = do
  let s = testState (80, 24) (statusOf Playing (Just 0) 3) (songs 3)
      ack = Ack AckArg 0 "play" "Bad song index"
      r = runEvents 0 [MpdDone (Left (AckError ack))] s
  assertEqual "message" (Just "play: Bad song index") ((.text) <$> r.state.message)

test_tick :: Assertion
test_tick = do
  let s = testState (80, 24) (statusOf Stopped Nothing 3) (songs 3)
      r = runEvents 0 [StatusFetched (Right (statusOf Playing (Just 0) 3 & #elapsed ?~ 10.25))] s
      ticks = [d | After d (Tick _) <- r.commands]
  -- A 60 s song on an 80 column bar moves a cell every 0.75 s: the next
  -- cell starts at 10.5 s, before the next second.
  case ticks of
    [d] -> assertBool ("delay " <> show d) (abs (d - 0.25) < 1e-9)
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
