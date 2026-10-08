module SaveTests (saveTests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.UI.Layout
import Utils

saveTests :: TestTree
saveTests =
  testGroup
    "Save"
    [ testCase "the queue saves as a new playlist" test_newPlaylist
    , testCase "a playlist of the name is replaced or appended to" test_existing
    , testCase "escape cancels the choice" test_cancel
    , testCase "the selected songs of the queue" test_selectedSongs
    , testCase "a playlist of items is replaced" test_replaceItems
    , testCase "the items of the browser" test_browserItems
    , testCase "parts of files are left out" test_parts
    , testCase "an empty queue saves nothing" test_emptyQueue
    , testCase "an empty name saves nothing" test_emptyName
    ]

test_newPlaylist :: Assertion
test_newPlaylist = do
  asked <- press ["e", "w"] =<< queue
  assertEqual
    "the question"
    (Just "Save the queue as: ")
    ((.question) <$> asked.state.prompt)
  checked <- named "mix" asked.state
  assertEqual "the playlists" [[Request "listplaylists" []]] checked.requests
  saved <- answer ["playlist: other", "Last-Modified: 2024-01-01T00:00:00Z"] checked
  assertEqual "the save" [[Request "save" ["mix", "create"]]] saved.requests
  assertEqual "the message" (Just "Saved the queue as mix") (message saved.state)

test_existing :: Assertion
test_existing = do
  asked <-
    answer ["playlist: mix"] =<< named "mix" . (.state) =<< press ["e", "w"] =<< queue
  assertEqual
    "the choice"
    (Just "The playlist mix exists. [replace/append]")
    (statusLine asked.state)
  replaced <- press ["r"] asked.state
  assertEqual "replaced" [[Request "save" ["mix", "replace"]]] replaced.requests
  assertEqual "the message" (Just "Replaced mix with the queue") (message replaced.state)
  appended <- press ["a"] asked.state
  assertEqual "appended" [[Request "save" ["mix", "append"]]] appended.requests
  assertEqual "nothing else picks" [] . (.requests) =<< press ["y"] asked.state

test_cancel :: Assertion
test_cancel = do
  asked <-
    answer ["playlist: mix"] =<< named "mix" . (.state) =<< press ["e", "w"] =<< queue
  r <- press ["escape"] asked.state
  assertEqual "no save" [] r.requests
  assertEqual "the message" (Just "Cancelled") (message r.state)

test_selectedSongs :: Assertion
test_selectedSongs = do
  asked <- press ["insert", "down", "insert", "e", "w"] =<< queue
  assertEqual "the question" (Just "Save 2 songs as: ") ((.question) <$> asked.state.prompt)
  saved <- answer [] =<< named "mix" asked.state
  assertEqual
    "the songs"
    [ [Request "playlistadd" ["mix", "dir/0.flac"], Request "playlistadd" ["mix", "dir/1.flac"]]
    ]
    saved.requests
  assertEqual "the message" (Just "Saved 2 songs as mix") (message saved.state)

test_replaceItems :: Assertion
test_replaceItems = do
  asked <-
    answer ["playlist: mix"]
      =<< named "mix" . (.state)
      =<< press ["insert", "e", "w"]
      =<< queue
  replaced <- press ["r"] asked.state
  assertEqual
    "cleared first"
    [[Request "playlistclear" ["mix"], Request "playlistadd" ["mix", "dir/0.flac"]]]
    replaced.requests

-- | A directory goes in with its songs, and a stored playlist with the
-- songs that MPD lists for it.
test_browserItems :: Assertion
test_browserItems = do
  root <- browserRoot
  asked <- press ["insert", "end", "insert", "e", "w"] root
  assertEqual "the question" (Just "Save 2 items as: ") ((.question) <$> asked.state.prompt)
  checked <- named "mix" asked.state
  assertEqual
    "the playlists and the songs of p"
    [[Request "listplaylists" [], Request "listplaylistinfo" ["p"]]]
    checked.requests
  saved <-
    answer ["playlist: p", "list_OK", "file: x.flac", "file: y.flac", "list_OK"] checked
  assertEqual
    "the save"
    [
      [ Request "searchaddpl" ["mix", "(base \"a\")"]
      , Request "playlistadd" ["mix", "x.flac"]
      , Request "playlistadd" ["mix", "y.flac"]
      ]
    ]
    saved.requests
  assertEqual "the message" (Just "Saved 3 items as mix") (message saved.state)

-- | The second song is a track of a cue sheet, a part of a file, which a
-- stored playlist would get whole.
test_parts :: Assertion
test_parts = do
  let songs = [song 0 [] 60, (song 1 [] 60) {range = Just (SongRange 60 (Just 90))}]
  s <- testState (80, 12) (statusOf Stopped Nothing 0) songs
  asked <- press ["insert", "down", "insert", "e", "w"] s
  assertEqual "the question" (Just "Save 1 song as: ") ((.question) <$> asked.state.prompt)
  saved <- answer [] =<< named "mix" asked.state
  assertEqual "the song" [[Request "playlistadd" ["mix", "dir/0.flac"]]] saved.requests
  assertEqual
    "the message"
    (Just "Saved 1 song as mix, without 1 part of a file")
    (message saved.state)
  onlyPart <- press ["down", "insert", "e", "w"] s
  assertEqual "no prompt" Nothing onlyPart.state.prompt
  assertEqual
    "why"
    (Just "Parts of files, e.g. the tracks of a cue sheet, can't be saved")
    (message onlyPart.state)

test_emptyQueue :: Assertion
test_emptyQueue = do
  r <- press ["e", "w"] =<< testState (80, 12) (statusOf Stopped Nothing 0) []
  assertEqual "no prompt" Nothing r.state.prompt
  assertEqual "the message" (Just "The queue is empty") (message r.state)

test_emptyName :: Assertion
test_emptyName = do
  r <- press ["e", "w", "enter"] =<< queue
  assertEqual "no request" [] r.requests

----------------------------------------
-- Helpers

queue :: IO AppState
queue = testState (80, 12) (statusOf Stopped Nothing 0) [song p [] 60 | p <- [0 .. 2]]

-- | The browser at the root, with the cursor on its first entry.
browserRoot :: IO AppState
browserRoot = do
  r <- press ["2"] =<< queue
  (.state) <$> answer ["directory: a", "directory: b", "playlist: p"] r

press :: [T.Text] -> AppState -> IO Result
press ks = runEvents 0 (map (KeyPressed . either (error . T.unpack) id . parseKeySpec) ks)

-- | Type a name in the prompt and enter it.
named :: T.Text -> AppState -> IO Result
named name = press (map T.singleton (T.unpack name) <> ["enter"])

-- | Answer the last request of a result.
answer :: [BS.ByteString] -> Result -> IO Result
answer ls r = case reverse r.pending of
  p : _ -> runEvents 0 [replyTo ls p] r.state
  [] -> assertFailure "no request to answer"

message :: AppState -> Maybe T.Text
message s = (.text) <$> s.message

-- | The status bar, the last line of the screen.
statusLine :: AppState -> Maybe T.Text
statusLine s = case reverse (imageLines (renderScreen testAppEnv s)) of
  l : _ -> Just l
  [] -> Nothing
