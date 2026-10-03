module KeysTests (keysTests) where

import Data.Either
import Data.Set qualified as S
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Keys

keysTests :: TestTree
keysTests =
  testGroup
    "Keys"
    [ testCase "parse" test_parse
    , testCase "keys a terminal can't tell apart" test_aliases
    , testCase "render" test_render
    , testCase "from vty" test_fromVty
    ]

test_parse :: Assertion
test_parse = do
  assertEqual "character" (Right $ KeySpec S.empty (CharKey 'a')) (parseKeySpec "a")
  assertEqual
    "ctrl"
    (Right $ KeySpec (S.fromList [Ctrl]) (CharKey 'x'))
    (parseKeySpec "ctrl-x")
  assertEqual
    "alt-shift-tab"
    (Right $ KeySpec (S.fromList [Alt, Shift]) Tab)
    (parseKeySpec "alt-shift-tab")
  assertEqual "minus" (Right $ KeySpec S.empty (CharKey '-')) (parseKeySpec "-")
  assertEqual
    "ctrl minus"
    (Right $ KeySpec (S.fromList [Ctrl]) (CharKey '-'))
    (parseKeySpec "ctrl--")
  assertEqual "function key" (Right $ KeySpec S.empty (Function 12)) (parseKeySpec "f12")
  assertEqual "page down" (Right $ KeySpec S.empty PageDown) (parseKeySpec "page_down")
  assertEqual "the letter f" (Right $ KeySpec S.empty (CharKey 'f')) (parseKeySpec "f")
  assertBool "unknown name" (isLeft $ parseKeySpec "pagedown")
  assertBool "repeated modifier" (isLeft $ parseKeySpec "ctrl-ctrl-a")
  assertBool "function key out of range" (isLeft $ parseKeySpec "f64")

test_aliases :: Assertion
test_aliases = do
  assertEqual
    "ctrl-i"
    (Left "ctrl-i is the same key as tab in a terminal; use tab")
    (parseKeySpec "ctrl-i")
  assertBool "ctrl-m" (isLeft $ parseKeySpec "ctrl-m")
  assertBool "ctrl-[" (isLeft $ parseKeySpec "ctrl-[")
  assertBool "ctrl-h" (isLeft $ parseKeySpec "ctrl-h")
  assertBool "ctrl-A" (isLeft $ parseKeySpec "ctrl-A")
  assertBool "shift-a" (isLeft $ parseKeySpec "shift-a")

test_render :: Assertion
test_render =
  mapM_
    (\t -> assertEqual (show t) (Right t) (renderKeySpec <$> parseKeySpec t))
    ["a", "ctrl-x", "ctrl-alt-shift-tab", "page_up", "f1", "space", "ctrl--", "U"]

test_fromVty :: Assertion
test_fromVty = do
  assertEqual "ctrl-a" (spec "ctrl-a") (fromVtyKey (V.KChar 'a') [V.MCtrl])
  assertEqual "tab" (spec "tab") (fromVtyKey (V.KChar '\t') [])
  assertEqual "shift-tab" (spec "shift-tab") (fromVtyKey V.KBackTab [])
  assertEqual "uppercase" (spec "A") (fromVtyKey (V.KChar 'A') [V.MShift])
  assertEqual "meta" (spec "alt-x") (fromVtyKey (V.KChar 'x') [V.MMeta])
  assertEqual "shift-up" (spec "shift-up") (fromVtyKey V.KUp [V.MShift])
  assertEqual "space" (spec "space") (fromVtyKey (V.KChar ' ') [])
  where
    spec :: T.Text -> Maybe KeySpec
    spec = either (const Nothing) Just . parseKeySpec
