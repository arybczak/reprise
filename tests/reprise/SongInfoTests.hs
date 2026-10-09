module SongInfoTests (songInfoTests) where

import Data.Text qualified as T
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.SongInfo
import Reprise.State
import Reprise.UI.Layout
import Utils

songInfoTests :: TestTree
songInfoTests =
  testGroup
    "Song info"
    [ testCase "the lines of a song" test_lines
    , testCase "the formats of MPD" test_formats
    , testCase "the rows at a width" test_rows
    , testCase "i shows the song under the cursor" test_show
    , testCase "the comments of a song that the screen left are dropped" test_stale
    , testCase "a stream has no comments to ask for" test_stream
    , testCase "no bitrate, also of the song that plays" test_bitrate
    ]

test_lines :: Assertion
test_lines = do
  let s =
        (song 0 [(Artist, ["A", "B"]), (Title, ["T"]), (MusicBrainzTrackId, ["id"])] 125)
          { file = "dir/sub/t.flac"
          , format = Just "44100:16:2"
          , lastModified = Just (UTCTime (fromGregorian 2014 11 4) 3600)
          , range = Just (SongRange 60 (Just 90))
          }
      comments = [("replaygain_track_gain", "-8.84 dB"), ("TITLE", "T")]
  assertEqual
    "the lines"
    [ InfoField "Filename" (Just "t.flac")
    , InfoField "Directory" (Just "dir/sub")
    , InfoField "Part" (Just "1:00 to 1:30")
    , InfoBlank
    , InfoField "Length" (Just "2:05")
    , InfoField "Sample rate" (Just "44100 Hz")
    , InfoField "Sample format" (Just "16 bit")
    , InfoField "Channels" (Just "Stereo")
    , InfoField "Last modified" (Just "2014-11-04 01:00:00 UTC")
    , InfoBlank
    , InfoField "Track gain" (Just "-8.84 dB")
    , InfoBlank
    , InfoField "Title" (Just "T")
    , InfoField "Artist" (Just "A | B")
    , InfoField "Album Artist" Nothing
    , InfoField "Album" Nothing
    , InfoField "Date" Nothing
    , InfoField "Track" Nothing
    , InfoField "Genre" Nothing
    , InfoField "Composer" Nothing
    , InfoField "Performer" Nothing
    , InfoField "Disc" Nothing
    , InfoField "Comment" Nothing
    , InfoField (tagName MusicBrainzTrackId) (Just "id")
    ]
    (songInfoLines " | " comments s)
  let bare = (song 0 [] 10) {file = "t.flac", duration = Nothing}
  assertEqual
    "without a directory, a length, ReplayGain and the rest"
    [ InfoField "Filename" (Just "t.flac")
    , InfoField "Directory" Nothing
    , InfoBlank
    , InfoField "Length" Nothing
    ]
    (take 4 (songInfoLines " | " [] bare))

test_formats :: Assertion
test_formats = do
  let audio f =
        [ l
        | l@(InfoField label _) <- songInfoLines "" [] ((song 0 [] 1) {format = Just f})
        , label `elem` ["Sample format", "Channels", "Format"]
        ]
  assertEqual
    "floating point, mono"
    [ InfoField "Sample format" (Just "32 bit floating point")
    , InfoField "Channels" (Just "Mono")
    ]
    (audio "48000:f:1")
  assertEqual
    "six channels"
    [InfoField "Sample format" (Just "24 bit"), InfoField "Channels" (Just "6 channels")]
    (audio "96000:24:6")
  assertEqual "DSD as it is" [InfoField "Format" (Just "dsd64:2")] (audio "dsd64:2")

test_rows :: Assertion
test_rows = do
  assertEqual
    "aligned, wrapped"
    [ ("Title:  ", Just "a b")
    , ("        ", Just "c")
    , ("", Just "")
    , ("Artist: ", Nothing)
    ]
    (infoRows 12 [InfoField "Title" (Just "a b c"), InfoBlank, InfoField "Artist" Nothing])
  assertEqual
    "a tab as a space"
    [("Comment: ", Just "a b")]
    (infoRows 20 [InfoField "Comment" (Just "a\tb")])

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
          [failureOf (AckError (Ack AckNoExist 0 "readcomments" "x")) second]
          stale.state
      assertEqual "a failure is no comments" [] failed.state.songInfo.comments
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
  assertBool "no bitrate" (not (any ("Bitrate" `T.isPrefixOf`) (screenLines shown.state)))

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
