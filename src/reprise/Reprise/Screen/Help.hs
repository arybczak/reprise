-- | The help screen: the bindings of the global keymap and of each screen's
-- keymap, as text that scrolls.
module Reprise.Screen.Help
  ( helpView
  , helpVerb
  , keyColumnWidth
  ) where

import Data.Text qualified as T
import Effectful
import Graphics.Vty qualified as V

import Reprise.Action
import Reprise.Config
import Reprise.Format
import Reprise.Handler.Core
import Reprise.Keymap
import Reprise.State
import Reprise.Style
import Reprise.UI.SongList
import Reprise.Width

helpView :: AppEnv -> View -> V.Image
helpView env v =
  let ls = helpLines env.keymaps
      render = renderHelpLine env.colorMode env.config.styles (keyColumnWidth ls) v.width
  in V.vertCat . map render . take v.height $ drop v.offset ls

-- | How the help screen does a verb, if it does it.
helpVerb :: App es => Action -> Maybe (Eff es ())
helpVerb = \case
  Move t -> Just $ scrollLines t
  _ -> Nothing

-- | A line of the given width. The keys of all entries, after their
-- indentation, share a column as wide as the widest.
renderHelpLine :: ColorMode -> StylesConfig -> Int -> Int -> HelpLine -> V.Image
renderHelpLine colorMode styles keyWidth width = \case
  Heading depth t -> cell [Span Nothing (indent depth), Span (Just (styles.label <> boldStyle)) t]
  Entry depth keys description ->
    let keys' = indent depth <> keys
    in cell
         [ Span (Just styles.value) (keys' <> T.replicate (keyWidth + gap - textWidth keys') " ")
         , Span (Just styles.text) description
         ]
  Blank -> cell []
  where
    cell :: [Span Style] -> V.Image
    cell spans = padded (toAttr colorMode) (mempty, mempty) AlignLeft width (fitSpans width spans)

    gap :: Int
    gap = 2

-- | The width of the key column: the widest key sequence with its
-- indentation.
keyColumnWidth :: [HelpLine] -> Int
keyColumnWidth ls = maximum (0 : [textWidth (indent depth <> keys) | Entry depth keys _ <- ls])

indent :: Int -> T.Text
indent depth = T.replicate (2 * depth) " "
