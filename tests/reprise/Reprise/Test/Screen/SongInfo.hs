module Reprise.Test.Screen.SongInfo (songInfoScreenTests) where

import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Test.Helpers
import Reprise.UI.Layout

songInfoScreenTests :: TestTree
songInfoScreenTests =
  testGroup
    "song info"
    [ testCase "i shows the song under the cursor" test_show
    , testCase "the comments of a song that the screen left are dropped" test_stale
    , testCase "a stream has no comments to ask for" test_stream
    , testCase "no bitrate, also of the song that plays" test_bitrate
    ]

test_show :: Assertion
test_show = do
  r <- runEvents 0 [key "i"] =<< queueShown
  assertEqual "the screen" SongInfoScreen (focusedView r.state).screen
  assertEqual "the comments" [[Request "readcomments" ["dir/0.flac"]]] r.requests
  case screenLines r.state of
    title : _ : first : _ -> do
      assertBool ("the title: " <> T.unpack title) ("Song info: A - One" `T.isPrefixOf` title)
      assertEqual "the file, by the widest label, Album Artist" "Filename:     0.flac" first
    ls -> assertFailure $ "too few lines: " <> show ls
  back <- runEvents 0 [key "i"] r.state
  assertEqual "back" QueueScreen (focusedView back.state).screen
  visualizer <- runEvents 0 [key "8", key "i"] =<< queueShown
  assertEqual
    "no songs"
    (Just "The visualizer screen has no songs")
    ((.text) <$> visualizer.state.message)

test_stale :: Assertion
test_stale = do
  r <- runEvents 0 [key "i", key "i", key "down", key "i"] =<< queueShown
  case r.pending of
    [first, second] -> do
      stale <- runEvents 0 [replyTo ["REPLAYGAIN_TRACK_GAIN: -1 dB"] first] r.state
      assertEqual "dropped" [] stale.state.songInfo.comments
      fresh <- runEvents 0 [replyTo ["REPLAYGAIN_TRACK_GAIN: -2 dB"] second] stale.state
      assertEqual "taken" [("REPLAYGAIN_TRACK_GAIN", "-2 dB")] fresh.state.songInfo.comments
      failed <-
        runEvents
          0
          [ failureOf
              (AckError (Ack {code = AckNoExist, index = 0, command = "readcomments", message = "x"}))
              second
          ]
          stale.state
      assertEqual "a failure is no comments" [] failed.state.songInfo.comments
      assertEqual "a failure shows no error" Nothing failed.state.message
    ps -> assertFailure $ "two requests, not " <> show (length ps)

test_stream :: Assertion
test_stream = do
  let stream = (song 0 [(Title, ["Radio"])] 0) {file = "http://radio.example/stream"}
  r <- runEvents 0 [key "i"] =<< testState (40, 10) (statusOf Stopped Nothing 1) [stream]
  assertEqual "the screen" SongInfoScreen (focusedView r.state).screen
  assertEqual "no request" [] r.requests

-- | MPD has the bitrate only of the song that plays, and it changes as the
-- song plays, so the song info has none, also of the song that plays.
test_bitrate :: Assertion
test_bitrate = do
  let playing = (statusOf Playing (Just 0) 2) {bitrate = Just 320}
  s <- testState (40, 30) playing songs
  shown <- runEvents 0 [key "i"] s
  assertBool "no bitrate" (not (any ("320" `T.isInfixOf`) (screenLines shown.state)))

----------------------------------------
-- Helpers

songs :: [Song]
songs =
  [ song 0 [(Artist, ["A"]), (Title, ["One"])] 60
  , song 1 [(Artist, ["A"]), (Title, ["Two"])] 60
  ]

-- | The queue of two songs on a terminal of 40 by 30.
queueShown :: IO AppState
queueShown = testState (40, 30) (statusOf Stopped Nothing 2) songs

screenLines :: AppState -> [T.Text]
screenLines = imageLines . renderScreen testAppEnv

key :: T.Text -> AppEvent
key = KeyPressed . either (error . T.unpack) id . parseKeySpec
