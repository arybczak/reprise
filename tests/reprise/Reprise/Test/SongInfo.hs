module Reprise.Test.SongInfo (songInfoTests) where

import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Mpd.Protocol.Types
import Reprise.SongInfo
import Reprise.Test.Helpers

songInfoTests :: TestTree
songInfoTests =
  testGroup
    "song info"
    [ testCase "the lines of a song" test_lines
    , testCase "the formats of MPD" test_formats
    , testCase "the rows at a width" test_rows
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
    , InfoField "MUSICBRAINZ_TRACKID" (Just "id")
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
