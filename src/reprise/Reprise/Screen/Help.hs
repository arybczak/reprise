-- | The help screen: the bindings of the global keymap and of each screen's
-- keymap, generated from the keymaps.
module Reprise.Screen.Help
  ( HelpLine (..)
  , helpLines
  , renderHelpLine
  , keyColumnWidth
  ) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Format
import Reprise.Keymap
import Reprise.Keys
import Reprise.Style
import Reprise.UI.SongList

data HelpLine
  = Heading T.Text
  | -- | A key sequence and what it does.
    Entry T.Text T.Text
  | Blank
  deriving stock (Eq, Show)

-- | A section for the global keymap and for each screen's keymap that binds
-- a key. A section lists the keys of its keymap, then each group of a
-- prefix key with the whole key sequences, e.g. @ctrl-t r@.
helpLines :: Keymaps -> [HelpLine]
helpLines keymaps =
  drop 1 . concat $
    [ Blank : Heading title : keymapLines [] keymap
    | (title, keymap) <- ("Global", keymaps.global) : screens
    , not (M.null keymap.bindings)
    ]
  where
    screens :: [(T.Text, Keymap)]
    screens =
      [ (T.toTitle (T.replace "_" " " (screenName s)), screenKeymap s keymaps)
      | s <- screenNames
      ]

    keymapLines :: [KeySpec] -> Keymap -> [HelpLine]
    keymapLines prefix keymap =
      [ Entry (keysText (prefix <> [k])) (describeAction a)
      | (k, BindAction a) <- M.toList keymap.bindings
      ]
        <> concat
          [ Blank : Heading (keysText keys <> maybe "" (": " <>) group.name) : keymapLines keys group
          | (k, BindPrefix group) <- M.toList keymap.bindings
          , let keys = prefix <> [k]
          ]

    keysText :: [KeySpec] -> T.Text
    keysText = T.unwords . map renderKeySpec

-- | A line of the given width. The keys of all entries share a column as
-- wide as the longest key sequence.
renderHelpLine :: ColorMode -> StylesConfig -> Int -> Int -> HelpLine -> V.Image
renderHelpLine colorMode styles keyWidth width = \case
  Heading t -> cell [Span (Just (styles.label <> boldStyle)) t]
  Entry keys description ->
    cell
      [ Span (Just styles.value) (keys <> T.replicate (keyWidth + gap - textWidth keys) " ")
      , Span Nothing description
      ]
  Blank -> cell []
  where
    cell :: [Span Style] -> V.Image
    cell spans = padded (toAttr colorMode) (mempty, mempty) AlignLeft width (fitSpans width spans)

    gap :: Int
    gap = 2

    boldStyle :: Style
    boldStyle = mempty & #attributes .~ S.singleton Bold

-- | The width of the key column: the longest key sequence.
keyColumnWidth :: [HelpLine] -> Int
keyColumnWidth ls = maximum (0 : [textWidth keys | Entry keys _ <- ls])
