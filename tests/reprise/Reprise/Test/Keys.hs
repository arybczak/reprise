module Reprise.Test.Keys (keysTests) where

import Control.Monad
import Data.Either
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix.Input.Terminfo
import Graphics.Vty.Platform.Unix.Input.Terminfo.ANSIVT qualified as ANSIVT
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Keys

keysTests :: TestTree
keysTests =
  testGroup
    "keys"
    [ testCase "parse" test_parse
    , testCase "keys a terminal can't tell apart" test_aliases
    , testCase "the keys that a terminal sends" test_terminalKeys
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
    "alt-shift-up"
    (Right $ KeySpec (S.fromList [Alt, Shift]) ArrowUp)
    (parseKeySpec "alt-shift-up")
  assertEqual "minus" (Right $ KeySpec S.empty (CharKey '-')) (parseKeySpec "-")
  assertEqual
    "alt minus"
    (Right $ KeySpec (S.fromList [Alt]) (CharKey '-'))
    (parseKeySpec "alt--")
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
  assertEqual
    "ctrl-space"
    (Left "ctrl-space is the same key as ctrl-@ in a terminal; use ctrl-@")
    (parseKeySpec "ctrl-space")
  assertEqual
    "ctrl-alt-i"
    (Left "ctrl-alt-i is the same key as alt-tab in a terminal; use alt-tab")
    (parseKeySpec "ctrl-alt-i")
  assertEqual
    "a character that ctrl doesn't go with"
    ( Left
        "ctrl-1 doesn't reach reprise, as a terminal sends ctrl only with a letter or one of @ \\ ] ^ _"
    )
    (parseKeySpec "ctrl-1")
  assertBool "alt with a character that isn't ASCII" (isLeft $ parseKeySpec "alt-ł")
  assertBool "a character that isn't ASCII" (isRight $ parseKeySpec "ł")

-- | A key parses if and only if vty-unix reads it from what a terminal
-- sends, so that every binding can run. This covers the keys that are
-- control characters, with every modifier, and the ASCII characters.
test_terminalKeys :: Assertion
test_terminalKeys =
  forM_ specs $ \spec ->
    assertEqual
      (T.unpack (renderKeySpec spec))
      (spec `S.member` sent)
      (isRight (parseKeySpec (renderKeySpec spec)))
  where
    -- vty-unix's tables in its order, where a later entry wins. It maps a
    -- carriage return to a line feed, so a carriage return never comes.
    -- The bytes of terminfo's keys differ between terminals.
    sent :: S.Set KeySpec
    sent =
      S.fromList . mapMaybe key $
        map snd keysFromCapsTable
          <> M.elems
            ( M.filterWithKey (\bytes _ -> '\r' `notElem` bytes) . M.fromList $
                universalTable <> concat ANSIVT.classifyTable
            )

    key :: V.Event -> Maybe KeySpec
    key = \case
      V.EvKey k mods -> fromVtyKey k mods
      _ -> Nothing

    specs :: [KeySpec]
    specs =
      [ KeySpec (S.fromList mods) k
      | k <- [Space, Enter, Escape, Backspace, Tab] <> map CharKey ['!' .. '~']
      , mods <- L.subsequences [minBound .. maxBound]
      ]

test_render :: Assertion
test_render =
  mapM_
    (\t -> assertEqual (show t) (Right t) (renderKeySpec <$> parseKeySpec t))
    ["a", "ctrl-x", "ctrl-alt-shift-up", "page_up", "f1", "space", "alt--", "U"]

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
    spec = either (error . T.unpack) Just . parseKeySpec
