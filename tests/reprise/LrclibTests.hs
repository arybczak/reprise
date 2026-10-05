module LrclibTests (lrclibTests) where

import Data.ByteString qualified as BS
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Yamlet

import Reprise.Lyrics
import Reprise.Lyrics.Lrclib
import Reprise.Mpd.Protocol.Types
import Utils

lrclibTests :: TestTree
lrclibTests =
  testGroup
    "LRCLIB"
    [ testCase "a title without what follows in brackets" test_cleanTitle
    , testCase "LRCLIB's answers decode" test_decode
    , testCase "the result of a search" test_chooseTrack
    , testCase "the lyrics of a song" test_found
    , testCase "a search after a miss" test_search
    , testCase "the title without brackets after a miss" test_cleaned
    , testCase "an instrumental" test_instrumental
    , testCase "failures" test_failures
    , testCase "a song without an artist isn't looked up" test_noArtist
    ]

test_cleanTitle :: Assertion
test_cleanTitle = do
  assertEqual "brackets" "Song" (cleanTitle "Song (Bonus Track)")
  assertEqual "square brackets and more" "Song" (cleanTitle "Song [Film Score] (Remix)")
  assertEqual "nested" "Song" (cleanTitle "Song (Live (2001))")
  assertEqual "all brackets" "(Intro)" (cleanTitle "(Intro)")
  assertEqual "a movement" "Concerto - 1. Allegro" (cleanTitle "Concerto - 1. Allegro")
  assertEqual "not closed" "Song)" (cleanTitle "Song)")

-- | The answers that LRCLIB gave, with other lyrics in them, and their
-- keys that reprise doesn't read.
test_decode :: Assertion
test_decode = do
  track <- either (assertFailure . show) pure . decode @LrclibTrack =<< answer "get.json"
  assertEqual "the track" "Karma Police" track.trackName
  assertEqual "the length" 264 track.duration
  assertEqual "the lyrics" (Just "First line\nSecond line") track.plainLyrics
  tracks <-
    either (assertFailure . show) pure . decode @[LrclibTrack] =<< answer "search.json"
  assertEqual "the results" 4 (length tracks)
  assertEqual "instrumentals" [False, True, True, True] (map (.instrumental) tracks)

-- | Of the results of a search for Alex Theme, of 295 s: the one with
-- lyrics, and three instrumentals, of 300 s and 295 s.
test_chooseTrack :: Assertion
test_chooseTrack = do
  tracks <-
    either (assertFailure . show) pure . decode @[LrclibTrack] =<< answer "search.json"
  let chosen d ts = (\t -> (t.duration, t.instrumental)) <$> chooseTrack d ts
  assertEqual "with lyrics" (Just (295, False)) (chosen (Just 295) tracks)
  assertEqual
    "with lyrics, also last"
    (Just (295, False))
    (chosen (Just 295) (reverse tracks))
  assertEqual "of the length" (Just True) (snd <$> chosen (Just 300) tracks)
  assertEqual "none of the length" Nothing (chosen (Just 200) tracks)
  assertEqual "without a length" (Just (295, False)) (chosen Nothing (reverse tracks))

test_found :: Assertion
test_found = do
  got <- answer "get.json"
  (asked, result) <- lookedUp karmaPolice [("/api/get", Right (200, got))]
  assertEqual "the lyrics" (LyricsFound (Fetched "LRCLIB") "First line\nSecond line") result
  assertEqual
    "the request"
    [
      ( "/api/get"
      ,
        [ ("artist_name", "Radiohead")
        , ("track_name", "Karma Police")
        , ("album_name", "OK Computer")
        , ("duration", "264")
        ]
      )
    ]
    asked

test_search :: Assertion
test_search = do
  missing <- answer "missing.json"
  found <- answer "search.json"
  (asked, result) <-
    lookedUp
      (alexTheme "Alex Theme" 295)
      [("/api/get", Right (404, missing)), ("/api/search", Right (200, found))]
  assertEqual "the lyrics" (LyricsFound (Fetched "LRCLIB") "First line\nSecond line") result
  assertEqual
    "the search"
    [("artist_name", "Akira Yamaoka"), ("track_name", "Alex Theme")]
    (fromMaybe [] (lookup "/api/search" asked))

test_cleaned :: Assertion
test_cleaned = do
  missing <- answer "missing.json"
  got <- answer "get.json"
  calls <- newIORef []
  let s = alexTheme "Alex Theme (Bonus Track)" 295
      get apiPath params = do
        modifyIORef' calls (<> [(apiPath, params)])
        pure $ case (apiPath, lookup "track_name" params) of
          ("/api/get", Just "Alex Theme") -> Right (200, got)
          ("/api/get", _) -> Right (404, missing)
          _ -> Right (200, "[]")
  result <- lrclibLyrics get s
  assertEqual "the lyrics" (LyricsFound (Fetched "LRCLIB") "First line\nSecond line") result
  asked <- readIORef calls
  assertEqual
    "the titles"
    [ ("/api/get", "Alex Theme (Bonus Track)")
    , ("/api/search", "Alex Theme (Bonus Track)")
    , ("/api/get", "Alex Theme")
    ]
    [(p, fromMaybe "" (lookup "track_name" ps)) | (p, ps) <- asked]

test_instrumental :: Assertion
test_instrumental = do
  missing <- answer "missing.json"
  found <- answer "search.json"
  let longer = alexTheme "Alex Theme" 300
  (_, result) <-
    lookedUp longer [("/api/get", Right (404, missing)), ("/api/search", Right (200, found))]
  assertEqual "instrumental" LyricsInstrumental result

test_failures :: Assertion
test_failures = do
  busy <- answer "busy.json"
  (_, overloaded) <- lookedUp karmaPolice [("/api/get", Right (503, busy))]
  assertEqual
    "busy"
    (LyricsFailed "LRCLIB: The server is busy, please retry in a moment")
    overloaded
  (_, unreachable) <- lookedUp karmaPolice [("/api/get", Left "LRCLIB can't be reached: x")]
  assertEqual "unreachable" (LyricsFailed "LRCLIB can't be reached: x") unreachable
  (_, unknown) <- lookedUp karmaPolice [("/api/get", Right (500, "<html>"))]
  assertEqual "unknown" (LyricsFailed "LRCLIB answered with the status 500") unknown
  (_, garbled) <- lookedUp karmaPolice [("/api/get", Right (200, "{\"id\": 1}"))]
  assertEqual "garbled" (LyricsFailed "LRCLIB's answer can't be read") garbled

test_noArtist :: Assertion
test_noArtist = do
  (asked, result) <- lookedUp (song 0 [(Title, ["T"])] 60) []
  assertEqual "missing" LyricsMissing result
  assertEqual "nothing asked" [] asked

-- | The lyrics of a song from answers by the path, and what was asked.
lookedUp
  :: Song
  -> [(BS.ByteString, Either T.Text (Int, BS.ByteString))]
  -> IO ([(BS.ByteString, [(T.Text, T.Text)])], LyricsResult)
lookedUp s answers = do
  calls <- newIORef []
  let get apiPath params = do
        modifyIORef' calls (<> [(apiPath, params)])
        pure . fromMaybe (Left "no answer") $ lookup apiPath answers
  result <- lrclibLyrics get s
  (,result) <$> readIORef calls

karmaPolice :: Song
karmaPolice =
  song 0 [(Artist, ["Radiohead"]), (Title, ["Karma Police"]), (Album, ["OK Computer"])] 264

alexTheme :: T.Text -> Seconds -> Song
alexTheme title = song 0 [(Artist, ["Akira Yamaoka"]), (Title, [title])]

answer :: FilePath -> IO BS.ByteString
answer name = BS.readFile ("tests" </> "reprise" </> "lrclib" </> name)
