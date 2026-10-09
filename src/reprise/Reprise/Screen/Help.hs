-- | The help screen: the bindings of the global keymap and of each screen's
-- keymap, as text that scrolls.
module Reprise.Screen.Help
  ( helpView
  , helpRows
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

helpView :: AppEnv -> AppState -> View -> V.Image
helpView env s v =
  V.vertCat
    . map (textRow env.colorMode v.width)
    . highlightFound env s HelpScreen
    . take v.height
    $ drop v.offset (helpRows env)

-- | The rows of the help.
helpRows :: AppEnv -> [[Span Style]]
helpRows env =
  let ls = helpLines env.keymaps
  in map (helpLineSpans env.config.styles (keyColumnWidth ls)) ls

-- | How the help screen does a verb, if it does it.
helpVerb :: App es => Action -> Maybe (Eff es ())
helpVerb = \case
  Move t -> Just $ scrollLines t
  _ -> Nothing

-- | A line. The keys of all entries, after their indentation, share a
-- column as wide as the widest.
helpLineSpans :: StylesConfig -> Int -> HelpLine -> [Span Style]
helpLineSpans styles keyWidth = \case
  Heading depth t -> [Span Nothing (indent depth), Span (Just (styles.label <> boldStyle)) t]
  Entry depth keys description ->
    let keys' = indent depth <> keys
    in [ Span (Just styles.value) (keys' <> T.replicate (keyWidth + gap - textWidth keys') " ")
       , Span (Just styles.text) description
       ]
  Blank -> []
  where
    gap :: Int
    gap = 2

-- | The width of the key column: the widest key sequence with its
-- indentation.
keyColumnWidth :: [HelpLine] -> Int
keyColumnWidth ls = maximum (0 : [textWidth (indent depth <> keys) | Entry depth keys _ <- ls])

indent :: Int -> T.Text
indent depth = T.replicate (2 * depth) " "
