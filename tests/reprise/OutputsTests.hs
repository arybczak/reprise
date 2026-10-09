module OutputsTests (outputsTests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Action
import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.UI.Layout
import Utils

outputsTests :: TestTree
outputsTests =
  testGroup
    "Outputs"
    [ testCase "showing the outputs fetches them once" test_shown
    , testCase "enter enables or disables an output" test_toggle
    , testCase "changes fetch the outputs once they showed" test_changes
    , testCase "a new connection fetches what a lost one failed" test_fetchLost
    , testCase "fewer outputs keep the cursor on one" test_fewer
    ]

test_shown :: Assertion
test_shown = do
  r <- press ["7"] =<< queueShown
  assertEqual "the screen" OutputsScreen (focusedView r.state).screen
  assertEqual "the request" [[Request "outputs" []]] r.requests
  s <- answer twoOutputs r
  assertEqual
    "the names"
    ["Speakers", "Headphones"]
    (take 2 . drop 2 $ imageLines (renderScreen testAppEnv s))
  assertEqual
    "the enabled one is bold"
    ["Speakers"]
    [ T.strip t
    | (a, t) <- imageSpans (renderScreen testAppEnv s)
    , V.SetTo st <- [V.attrStyle a]
    , V.hasStyle st V.bold
    , T.strip t `elem` ["Speakers", "Headphones"]
    ]
  again <- press ["1", "7"] s
  assertEqual "nothing fetched again" [] again.requests

test_toggle :: Assertion
test_toggle = do
  s <- answer twoOutputs =<< press ["7"] =<< queueShown
  disabled <- press ["enter"] s
  assertEqual "disabled" [[Request "disableoutput" ["0"]]] disabled.requests
  assertEqual "its message" (Just "Output \"Speakers\" disabled") (message disabled.state)
  enabled <- press ["down", "enter"] s
  assertEqual "enabled" [[Request "enableoutput" ["1"]]] enabled.requests
  assertEqual "its message" (Just "Output \"Headphones\" enabled") (message enabled.state)

test_changes :: Assertion
test_changes = do
  s <- queueShown
  let changed = runEvents 0 [MpdChanged [OutputSubsystem]]
  assertEqual "not before they showed" [] . (.requests) =<< changed s
  shown <- answer twoOutputs =<< press ["7"] s
  assertEqual "a change" [[Request "outputs" []]] . (.requests) =<< changed shown
  assertBool "a new connection" . elem [Request "outputs" []] . (.requests)
    =<< runEvents 0 [MpdConnected (Version 0 24 0)] shown

test_fetchLost :: Assertion
test_fetchLost = do
  r <- press ["7"] =<< queueShown
  lost <- case r.pending of
    [p] -> runEvents 0 [failureOf (ConnectionError (Broken "reset")) p] r.state
    _ -> assertFailure "not one request"
  again <- runEvents 0 [MpdDisconnected "reset", MpdConnected (Version 0 24 0)] lost.state
  assertBool "fetched again" (elem [Request "outputs" []] again.requests)

test_fewer :: Assertion
test_fewer = do
  s <- press ["down"] =<< answer twoOutputs =<< press ["7"] =<< queueShown
  assertEqual "on the second" 1 (focusedView s.state).cursor
  fewer <- runEvents 0 [MpdChanged [OutputSubsystem]] s.state
  r <- answer (take 4 twoOutputs) fewer
  assertEqual "on the one left" 0 (focusedView r).cursor

----------------------------------------
-- Helpers

-- | An enabled output and a disabled one, as MPD lists them.
twoOutputs :: [BS.ByteString]
twoOutputs =
  [ "outputid: 0"
  , "outputname: Speakers"
  , "plugin: pulse"
  , "outputenabled: 1"
  , "outputid: 1"
  , "outputname: Headphones"
  , "plugin: alsa"
  , "outputenabled: 0"
  ]

queueShown :: IO AppState
queueShown = testState (80, 12) (statusOf Stopped Nothing 1) [song 0 [] 60]

press :: [T.Text] -> AppState -> IO Result
press ks = runEvents 0 (map (KeyPressed . key) ks)

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec

-- | Answer the last request of a result.
answer :: [BS.ByteString] -> Result -> IO AppState
answer ls r = case reverse r.pending of
  p : _ -> (.state) <$> runEvents 0 [replyTo ls p] r.state
  [] -> assertFailure "no request to answer"

message :: AppState -> Maybe T.Text
message s = (.text) <$> s.message
