-- | Key specs, e.g. @ctrl-x@, @alt-shift-up@ or @page_down@.
module Reprise.Keys
  ( -- * Keys
    KeySpec (..)
  , Key (..)
  , Modifier (..)
  , parseKeySpec
  , renderKeySpec
  , plain
  , char
  , ctrl
  , shift

    -- * Conversion from vty
  , fromVtyKey
  ) where

import Data.Char hiding (Space)
import Data.List qualified as L
import Data.Set qualified as S
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Yamlet

import Reprise.Number

-- | A key with modifiers, as a terminal reports it.
data KeySpec = KeySpec
  { modifiers :: S.Set Modifier
  , key :: Key
  }
  deriving stock (Eq, Ord, Show)

data Key
  = -- | A printable character. @A@ is shift-a.
    CharKey Char
  | Space
  | Enter
  | Escape
  | Backspace
  | Tab
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | Home
  | End
  | PageUp
  | PageDown
  | InsertKey
  | DeleteKey
  | Function Int
  deriving stock (Eq, Ord, Show)

data Modifier = Ctrl | Alt | Shift
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A key without modifiers.
plain :: Key -> KeySpec
plain = KeySpec S.empty

-- | A character without modifiers.
char :: Char -> KeySpec
char = plain . CharKey

-- | A character with ctrl.
ctrl :: Char -> KeySpec
ctrl = KeySpec (S.singleton Ctrl) . CharKey

-- | A key with shift. A character has its shift in itself, e.g. @A@.
shift :: Key -> KeySpec
shift = KeySpec (S.singleton Shift)

-- | Parse a key spec: modifiers, each followed by @-@, then a key name or a
-- character.
parseKeySpec :: T.Text -> Either T.Text KeySpec
parseKeySpec input = do
  (mods, name) <- splitModifiers S.empty input
  k <- parseKey name
  check (KeySpec mods k)
  where
    splitModifiers :: S.Set Modifier -> T.Text -> Either T.Text (S.Set Modifier, T.Text)
    splitModifiers mods t = case L.find
      (\(prefix, _) -> prefix `T.isPrefixOf` t && T.length t > T.length prefix)
      modifierPrefixes of
      Just (prefix, m)
        | m `S.member` mods -> Left $ "the modifier " <> T.dropEnd 1 prefix <> " is given twice"
        | otherwise -> splitModifiers (S.insert m mods) (T.drop (T.length prefix) t)
      Nothing -> Right (mods, t)

    parseKey :: T.Text -> Either T.Text Key
    parseKey name
      | Just k <- lookup name keyNames = Right k
      | Just n <- T.stripPrefix "f" name
      , Right fn <- readFunction n =
          Right (Function fn)
      | [c] <- T.unpack name, isPrint c, c /= ' ' = Right (CharKey c)
      | otherwise =
          Left $
            "unknown key "
              <> name
              <> ", expected a character, f1 to f63, or one of: "
              <> T.intercalate ", " (map fst keyNames)

    readFunction :: T.Text -> Either T.Text Int
    readFunction = maybe (Left "not a function key") Right . decimalIn 1 maxFunctionKey

    -- terminfo names the function keys up to kf63.
    maxFunctionKey :: Int
    maxFunctionKey = 63

    -- Reject keys that a terminal sends as other keys, so that two entries
    -- that look different can't be the same key, and keys that a terminal
    -- doesn't send as one key, whose bindings would never run.
    check :: KeySpec -> Either T.Text KeySpec
    check spec = case (spec.key, sentAs spec) of
      (CharKey c, _)
        | Shift `S.member` spec.modifiers ->
            Left $
              "shift-"
                <> T.singleton c
                <> " can't be told apart from other keys in a terminal; write the character it types, e.g. "
                <> T.singleton (toUpper c)
      (_, Right same)
        | same == spec -> Right spec
        | otherwise ->
            Left $
              renderKeySpec spec
                <> " is the same key as "
                <> renderKeySpec same
                <> " in a terminal; use "
                <> renderKeySpec same
      (_, Left why) -> Left $ renderKeySpec spec <> " " <> why

    -- The key that vty-unix reads from the bytes that a terminal sends for
    -- a key, or why it reads no such key. Its tables are the same for
    -- every terminal. It maps a carriage return to a line feed.
    sentAs :: KeySpec -> Either T.Text KeySpec
    sentAs spec =
      let has m = m `S.member` spec.modifiers
          without ms = S.difference spec.modifiers (S.fromList ms)
      in case spec.key of
           CharKey c
             | has Ctrl -> case toLower c of
                 'i' -> Right $ KeySpec (without [Ctrl]) Tab
                 'j' -> Right $ KeySpec (without [Ctrl]) Enter
                 'm' -> Right $ KeySpec (without [Ctrl]) Enter
                 '[' -> Right $ KeySpec (without [Ctrl]) Escape
                 '?' -> Right $ KeySpec (without [Ctrl]) Backspace
                 'h'
                   | has Alt -> Left "reaches reprise as escape and then backspace"
                   | otherwise -> Right $ KeySpec (without [Ctrl]) Backspace
                 l
                   | l `elem` ('@' : ['a' .. 'z'] <> "\\]^_") ->
                       Right $ KeySpec spec.modifiers (CharKey l)
                   | otherwise ->
                       Left
                         "doesn't reach reprise, as a terminal sends ctrl only with a letter or one of @ \\ ] ^ _"
             | has Alt
             , c == '[' ->
                 Left "reaches reprise as escape and then [, which begins the bytes of other keys"
             | has Alt
             , not (isAscii c) ->
                 Left $
                   "reaches reprise as escape and then "
                     <> T.singleton c
                     <> ", as a terminal sends alt as one key only with an ASCII character"
           Space
             | has Ctrl -> Right $ KeySpec (without [Shift]) (CharKey '@')
             | otherwise -> Right $ KeySpec (without [Shift]) Space
           Tab
             | has Alt && has Shift -> Left "doesn't reach reprise as one key"
             | otherwise -> Right $ KeySpec (without [Ctrl]) Tab
           k | k `elem` [Enter, Backspace, Escape] -> Right $ KeySpec (without [Ctrl, Shift]) k
           _ -> Right spec

-- | The text of a key spec, which 'parseKeySpec' reads back.
renderKeySpec :: KeySpec -> T.Text
renderKeySpec spec =
  T.concat [prefix | (prefix, m) <- modifierPrefixes, m `S.member` spec.modifiers]
    <> renderKey spec.key

-- | The text of a key, which 'parseKeySpec' reads back.
renderKey :: Key -> T.Text
renderKey = \case
  CharKey c -> T.singleton c
  Function n -> "f" <> T.pack (show n)
  Space -> "space"
  Enter -> "enter"
  Escape -> "escape"
  Backspace -> "backspace"
  Tab -> "tab"
  ArrowUp -> "up"
  ArrowDown -> "down"
  ArrowLeft -> "left"
  ArrowRight -> "right"
  Home -> "home"
  End -> "end"
  PageUp -> "page_up"
  PageDown -> "page_down"
  InsertKey -> "insert"
  DeleteKey -> "delete"

-- | An unquoted digit is an integer, so it is accepted as text.
instance FromYaml KeySpec where
  parseYaml n = case view n of
    StringView t -> spec t
    IntView i -> spec . T.pack $ show i
    NullView -> failAt n "~ is null in YAML; quote it to bind the ~ key: \"~\""
    _ -> typeMismatch "a key, e.g. ctrl-x" n
    where
      spec :: T.Text -> Parser KeySpec
      spec = either (failAt n . T.unpack) pure . parseKeySpec

modifierPrefixes :: [(T.Text, Modifier)]
modifierPrefixes = [("ctrl-", Ctrl), ("alt-", Alt), ("shift-", Shift)]

-- | The keys with names, by their names. A character and a function key
-- have none.
keyNames :: [(T.Text, Key)]
keyNames =
  [ (renderKey k, k)
  | k <-
      [ Space
      , Enter
      , Escape
      , Backspace
      , Tab
      , ArrowUp
      , ArrowDown
      , ArrowLeft
      , ArrowRight
      , Home
      , End
      , PageUp
      , PageDown
      , InsertKey
      , DeleteKey
      ]
  ]

----------------------------------------
-- Conversion from vty

-- | The key spec of a key event from vty, if it is a key that a spec can
-- name.
fromVtyKey :: V.Key -> [V.Modifier] -> Maybe KeySpec
fromVtyKey vkey vmods = do
  (k, extra) <- case vkey of
    V.KChar '\t' -> Just (Tab, [])
    V.KChar ' ' -> Just (Space, [])
    -- The shift of a character is in the character itself.
    V.KChar c -> Just (CharKey c, [])
    V.KEnter -> Just (Enter, [])
    V.KEsc -> Just (Escape, [])
    V.KBS -> Just (Backspace, [])
    V.KBackTab -> Just (Tab, [Shift])
    V.KUp -> Just (ArrowUp, [])
    V.KDown -> Just (ArrowDown, [])
    V.KLeft -> Just (ArrowLeft, [])
    V.KRight -> Just (ArrowRight, [])
    V.KHome -> Just (Home, [])
    V.KEnd -> Just (End, [])
    V.KPageUp -> Just (PageUp, [])
    V.KPageDown -> Just (PageDown, [])
    V.KIns -> Just (InsertKey, [])
    V.KDel -> Just (DeleteKey, [])
    V.KFun n -> Just (Function n, [])
    _ -> Nothing
  let mods = S.fromList (extra <> map modifier vmods)
      mods' = case k of
        CharKey _ -> S.delete Shift mods
        _ -> mods
  pure $ KeySpec mods' k
  where
    modifier :: V.Modifier -> Modifier
    modifier = \case
      V.MShift -> Shift
      V.MCtrl -> Ctrl
      V.MMeta -> Alt
      V.MAlt -> Alt
