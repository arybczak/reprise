module TekstowoTests (tekstowoTests) where

import Data.ByteString qualified as BS
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Lyrics
import Reprise.Lyrics.Tekstowo
import Reprise.Mpd.Protocol.Types
import Utils

tekstowoTests :: TestTree
tekstowoTests =
  testGroup
    "tekstowo.pl"
    [ testCase "the songs that a search found" test_searchResults
    , testCase "the lyrics on the page of a song" test_songLyrics
    , testCase "the lyrics of a song" test_found
    , testCase "a title without brackets" test_cleaned
    , testCase "a search without the song" test_notTheSong
    , testCase "a search that finds nothing" test_nothing
    , testCase "failures" test_failures
    ]

-- | Of the search for Kult Arahja: the songs, not the popular ones before
-- them or the artists after them.
test_searchResults :: Assertion
test_searchResults = do
  results <- searchResults <$> page "search.html"
  assertEqual
    "the first songs"
    [ ("/kult/arahja", "Kult - Arahja")
    , ("/kult/gdy-nie-ma-dzieci", "Kult - Gdy nie ma dzieci")
    , ("/kult/baranek", "Kult - Baranek")
    ]
    (take 3 results)
  assertBool "only songs" $ all (\(p, _) -> T.count "/" p == 2) results
  assertBool "not the popular ones" $ all (\(p, _) -> "/kult/" `T.isPrefixOf` p) results

-- | The page of Arahja, with other lyrics and another translation in it.
test_songLyrics :: Assertion
test_songLyrics = do
  lyrics <- songLyrics <$> page "song.html"
  assertEqual "the lyrics" (Just "First line\nSecond line & more…\n\nThird line") lyrics
  assertEqual "no lyrics" Nothing (songLyrics "<html><body>No text</body></html>")

test_found :: Assertion
test_found = do
  (asked, result) <-
    lookedUp
      (kult "Arahja")
      [("/szukaj", 200, "search.html"), ("/kult/arahja", 200, "song.html")]
  assertEqual "the lyrics" (fetched "First line\nSecond line & more…\n\nThird line") result
  assertEqual
    "the requests"
    [("/szukaj", [("search-query", "Kult Arahja")]), ("/kult/arahja", [])]
    asked

test_cleaned :: Assertion
test_cleaned = do
  (asked, result) <-
    lookedUp
      (kult "baranek (Live)")
      [("/szukaj", 200, "search.html"), ("/kult/baranek", 200, "song.html")]
  assertBool "the lyrics" (isFound result)
  assertEqual
    "the search"
    [("search-query", "Kult baranek")]
    (fromMaybe [] (lookup "/szukaj" asked))
  assertEqual "the song" ["/szukaj", "/kult/baranek"] (map fst asked)

-- | A song that is only close isn't taken.
test_notTheSong :: Assertion
test_notTheSong = do
  (asked, result) <- lookedUp (kult "Arahja 2") [("/szukaj", 200, "search.html")]
  assertEqual "missing" FetchedNothing result
  assertEqual "no song" ["/szukaj"] (map fst asked)

-- | Its redirect isn't followed, as robots must not read where it goes.
test_nothing :: Assertion
test_nothing = do
  (asked, result) <- lookedUp (kult "Nothing") [("/szukaj", 302, "nothing.html")]
  assertEqual "missing" FetchedNothing result
  assertEqual "only the search" ["/szukaj"] (map fst asked)

test_failures :: Assertion
test_failures = do
  (_, busy) <- lookedUp (kult "Arahja") [("/szukaj", 503, "nothing.html")]
  assertEqual "a status" (FetchFailed "tekstowo.pl answered with the status 503") busy
  calls <- newIORef []
  let unreachable p params = do
        modifyIORef' calls (<> [(p, params)])
        pure (Left "tekstowo.pl can't be reached: x")
  result <- tekstowoLyrics unreachable (kult "Arahja")
  assertEqual "unreachable" (FetchFailed "tekstowo.pl can't be reached: x") result
  (_, pageless) <-
    lookedUp
      (kult "Arahja")
      [("/szukaj", 200, "search.html"), ("/kult/arahja", 200, "nothing.html")]
  assertEqual "a page without lyrics" FetchedNothing pageless

-- | The lyrics of a song from pages by their paths, and what was asked.
lookedUp
  :: Song
  -> [(BS.ByteString, Int, FilePath)]
  -> IO ([(BS.ByteString, [(T.Text, T.Text)])], FetchResult)
lookedUp s answers = do
  calls <- newIORef []
  let get p params = do
        modifyIORef' calls (<> [(p, params)])
        case [(status, file) | (p', status, file) <- answers, p' == p] of
          (status, file) : _ -> Right . (status,) <$> BS.readFile (dir </> file)
          [] -> pure (Left "no answer")
  result <- tekstowoLyrics get s
  (,result) <$> readIORef calls

kult :: T.Text -> Song
kult title = song 0 [(Artist, ["Kult"]), (Title, [title])] 200

fetched :: T.Text -> FetchResult
fetched = FetchedLyrics . plainLyrics

isFound :: FetchResult -> Bool
isFound = \case
  FetchedLyrics _ -> True
  _ -> False

page :: FilePath -> IO T.Text
page file = T.decodeUtf8Lenient <$> BS.readFile (dir </> file)

dir :: FilePath
dir = "tests" </> "reprise" </> "tekstowo"
