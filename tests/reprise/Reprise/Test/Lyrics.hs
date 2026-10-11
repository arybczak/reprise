module Reprise.Test.Lyrics (lyricsTests) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Lyrics
import Reprise.Mpd.Protocol.Types
import Reprise.Test.Helpers
import Reprise.Test.Lyrics.Http
import Reprise.Test.Lyrics.Lrclib
import Reprise.Test.Lyrics.Tekstowo
import Reprise.Test.Lyrics.Worker

lyricsTests :: TestTree
lyricsTests =
  testGroup
    "lyrics"
    [ testCase "the file names are ncmpcpp's" test_fileNames
    , testCase "a name too long for a file is cut" test_longFileNames
    , testCase "the name of a long title stays" test_longName
    , testCase "timed lyrics from LRC" test_parseLrc
    , httpTests
    , lrclibTests
    , tekstowoTests
    , lyricsWorkerTests
    ]

-- | The first is the name of a file in the author's lyrics from ncmpcpp.
test_fileNames :: Assertion
test_fileNames = do
  let named artists titles = lyricsFileName (song 3 [(Artist, artists), (Title, titles)] 60)
  assertEqual
    "artist and title"
    "Akira Yamaoka - Letter - from the Lost Days.txt"
    (named ["Akira Yamaoka"] ["Letter - from the Lost Days"])
  assertEqual "the first artist" "A - T.txt" (named ["A", "B"] ["T"])
  assertEqual
    "without the characters that Windows forbids"
    "ACDC - Whats Next Re Stacks.txt"
    (named ["AC/DC"] ["Whats Next? Re: Stacks"])
  assertEqual "without a title, the file" "3.txt" (named ["A"] [])
  assertEqual "with an empty artist, the file" "3.txt" (named [""] ["T"])
  assertEqual
    "the times, with a point in the title"
    "A - Mr. Blue.lrc"
    (timedLyricsFileName (song 3 [(Artist, ["A"]), (Title, ["Mr. Blue"])] 60))

-- | The longest name in the author's library, of 285 bytes, and names of
-- characters of two bytes and of four.
test_longFileNames :: Assertion
test_longFileNames = do
  let handel =
        "14. Recitative (Soprano) There were shepherds abiding in the field, keeping "
          <> "watch over their flock by night. And lo, the angel of the Lord came upon "
          <> "them, and the glory of the Lord shone round about them, and they were sore "
          <> "afraid (Messiah, HWV 56)"
      named title = lyricsFileName (song 3 [(Artist, ["George Frideric Handel"]), (Title, [title])] 60)
      bytes = BS.length . T.encodeUtf8 . T.pack
      long = named handel
  assertBool ("cut: " <> show (bytes long)) (bytes long <= 255)
  assertBool
    "the start"
    ("George Frideric Handel - 14. Recitative (Soprano)" `L.isPrefixOf` long)
  assertEqual
    "the same for the times"
    (dropExtension long)
    ( dropExtension
        (timedLyricsFileName (song 3 [(Artist, ["George Frideric Handel"]), (Title, [handel])] 60))
    )
  assertBool "apart from a name that begins alike" (named (handel <> " 2") /= long)
  forM_ [T.replicate 200 "ż", T.replicate 100 "🎵"] $ \title -> do
    let name = named title
    assertBool
      ("cut: " <> show (bytes name))
      (bytes name <= 255 && ".txt" `L.isSuffixOf` name)

-- | The name of a long title is the same in every version, as stored
-- lyrics are found by it.
test_longName :: Assertion
test_longName =
  assertEqual
    "the name"
    ("A - " <> replicate 230 'a' <> " c98f27d3cedf8c0d.txt")
    (lyricsFileName (song 3 [(Artist, ["A"]), (Title, [T.replicate 300 "a"])] 60))

test_parseLrc :: Assertion
test_parseLrc = do
  assertEqual
    "lines"
    [(1, "First"), (2.5, "Second")]
    (parseLrc "[00:01.00] First\n[00:02.50]Second\n")
  assertEqual
    "a line at several times"
    [(10, "Chorus"), (20, "Verse"), (30, "Chorus")]
    (parseLrc "[00:10.00][00:30.00]Chorus\n[00:20.00]Verse")
  assertEqual "tags" [(1, "Line")] (parseLrc "[ar:Artist]\n[ti:Title]\n[00:01]Line")
  assertEqual
    "times"
    [(62, "a"), (62.345, "b"), (62.5, "c")]
    (parseLrc "[1:02]a\n[01:02.345]b\n[01:02:50]c")
  assertEqual "a pause" [(5, "")] (parseLrc "[00:05.00]")
  assertEqual "Windows' line ends" [(1, "A")] (parseLrc "[00:01.00]A\r\n")
  assertEqual "spaces before a time" [(1, "A")] (parseLrc "  [00:01.00]A")
  -- A positive offset, in milliseconds, shows the lines sooner.
  assertEqual
    "an offset"
    [(0.5, "A"), (1.5, "B")]
    (parseLrc "[offset:+500]\n[00:01.00]A\n[00:02.00]B")
  assertEqual "a negative offset" [(1.5, "A")] (parseLrc "[offset: -500]\r\n[00:01.00]A")
  assertEqual "not before the start" [(0, "A")] (parseLrc "[offset:2000]\n[00:01.00]A")
  assertEqual "not timed" Nothing (timedLyrics "Just words\nand more")
  assertEqual
    "the text of timed lyrics"
    (Just "First\nSecond\n")
    ((.plain) <$> timedLyrics "[00:01.00] First\n[00:02.50]Second")
