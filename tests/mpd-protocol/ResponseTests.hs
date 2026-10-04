module ResponseTests (responseTests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time qualified as Time
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Text.Pretty.Simple

import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Response
import Reprise.Mpd.Protocol.Types
import Thunks

responseTests :: TestTree
responseTests =
  testGroup
    "Response"
    [ testGroup
        "recorded replies"
        [ golden "status-stopped" status
        , golden "status-playing" status
        , golden "currentsong" currentSong
        , golden "playlistinfo" playlistInfo
        , golden "stats" stats
        , golden "outputs" outputs
        , golden "command-list" $ (,) <$> status <*> currentSong
        , golden "ack" $ play (Just 5)
        , golden "command-list-ack" $ setRepeat True *> play (Just 5) *> setRandom True
        , goldenIdle
        ]
    , testCase "parseAck" test_parseAck
    , testCase "readSeconds" test_readSeconds
    , testCase "readTime" test_readTime
    , testCase "malformed replies" test_malformedReplies
    , testCase "empty value" test_emptyValue
    , testCase "unknown subsystem" test_unknownSubsystem
    , testCase "volume without a mixer" test_volumeWithoutMixer
    , testCase "parsed songs hold no thunks" test_songsEvaluated
    ]

-- | A thunk in a parsed song would keep its slice of the reply, and with it
-- the whole buffer that the slice is in, until something forces it.
test_songsEvaluated :: Assertion
test_songsEvaluated = do
  reply <- BS.readFile (replyFile "playlistinfo" ".txt")
  songs <-
    either (assertFailure . show) pure $
      parseCommandReply playlistInfo =<< parseReply (BS8.lines reply)
  found <- thunks songs
  assertEqual "thunks" [] found

test_parseAck :: Assertion
test_parseAck = do
  assertEqual
    "full"
    (Just $ Ack AckNoExist 2 "play" "No such song")
    (parseAck "ACK [50@2] {play} No such song")
  assertEqual "no command" (Just $ Ack AckUnknown 0 "" "x") (parseAck "ACK [5@0] {} x")
  assertEqual
    "unknown code"
    (Just $ Ack (AckOther 99) 0 "a" "b")
    (parseAck "ACK [99@0] {a} b")
  assertEqual "malformed" Nothing (parseAck "ACK 50 play")

test_readSeconds :: Assertion
test_readSeconds = do
  assertEqual "whole" (Just 3) (readSeconds "3")
  assertEqual "milliseconds" (Just 2.345) (readSeconds "2.345")
  assertEqual "short fraction" (Just 2.5) (readSeconds "2.5")
  assertEqual "long fraction" (Just 2.345) (readSeconds "2.3456")
  assertEqual "empty" Nothing (readSeconds "")
  assertEqual "no digits after the dot" Nothing (readSeconds "2.")
  assertEqual "negative" Nothing (readSeconds "-2")

test_readTime :: Assertion
test_readTime = do
  assertEqual "MPD's form" (Just (at 2024 1 2 11045)) (readTime "2024-01-02T03:04:05Z")
  assertEqual "an offset" (Just (at 2024 1 2 7445)) (readTime "2024-01-02T03:04:05+01:00")
  assertEqual "not a time" Nothing (readTime "yesterday")
  where
    at :: Integer -> Int -> Int -> Time.DiffTime -> Time.UTCTime
    at y m d = Time.UTCTime (Time.fromGregorian y m d)

test_malformedReplies :: Assertion
test_malformedReplies = do
  assertEqual
    "no OK"
    (Left $ ProtocolError "the reply ended without OK or ACK")
    (parseReply ["a: b"])
  assertEqual
    "no separator"
    (Left $ ProtocolError "malformed line: ab")
    (parseReply ["ab", "OK"])
  assertEqual
    "after OK"
    (Left $ ProtocolError "the reply continues after OK")
    (parseReply ["OK", "a: b"])

test_emptyValue :: Assertion
test_emptyValue =
  assertEqual
    "a key with an empty value"
    (Right [[Field "lastloadedplaylist" ""]])
    (parseReply ["lastloadedplaylist: ", "OK"])

test_unknownSubsystem :: Assertion
test_unknownSubsystem =
  assertEqual
    "a subsystem of a newer MPD"
    (Right [PlayerSubsystem, OtherSubsystem "future"])
    (parseSubsystems [Field "changed" "player", Field "changed" "future"])

test_volumeWithoutMixer :: Assertion
test_volumeWithoutMixer = do
  let fields =
        [ Field "repeat" "0"
        , Field "random" "0"
        , Field "single" "0"
        , Field "consume" "0"
        , Field "playlist" "1"
        , Field "playlistlength" "0"
        , Field "state" "stop"
        ]
  assertEqual "no volume key" (Right Nothing) ((.volume) <$> parseStatus fields)
  assertEqual
    "volume -1"
    (Right Nothing)
    ((.volume) <$> parseStatus (Field "volume" "-1" : fields))

----------------------------------------
-- Helpers

-- | Parse a reply recorded from a real server with the parser of a command,
-- and compare the result with a golden file.
golden :: Show a => String -> Command a -> TestTree
golden name cmd = goldenVsString name (replyFile name ".golden") $ do
  reply <- BS.readFile (replyFile name ".txt")
  pure . render $ parseCommandReply cmd =<< parseReply (BS8.lines reply)

-- | @idle@ isn't a 'Command', because MPD doesn't allow it in a command list.
goldenIdle :: TestTree
goldenIdle = goldenVsString "idle" (replyFile "idle" ".golden") $ do
  reply <- BS.readFile (replyFile "idle" ".txt")
  pure . render $
    parseReply (BS8.lines reply) >>= \case
      [fields] -> either (Left . ProtocolError) Right $ parseSubsystems fields
      _ -> Left $ ProtocolError "more than one part"

render :: Show a => a -> BL.ByteString
render = TL.encodeUtf8 . (<> "\n") . pShowNoColor

replyFile :: String -> String -> FilePath
replyFile name ext = "tests" </> "mpd-protocol" </> "replies" </> name <> ext
