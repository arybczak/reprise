-- | The help screen: the bindings of the global keymap and of each screen's
-- keymap, as text that scrolls.
module Reprise.Screen.Help
  ( helpView
  , scrollHelp
  , keyColumnWidth
  ) where

import Data.Set qualified as S
import Data.Text qualified as T
import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Format
import Reprise.Handler.Core
import Reprise.Keymap
import Reprise.State
import Reprise.Style
import Reprise.UI.SongList

helpView :: AppEnv -> View -> V.Image
helpView env v =
  let ls = helpLines env.keymaps
      render = renderHelpLine env.colorMode env.config.styles (keyColumnWidth ls) v.width
  in V.vertCat . map render . take v.height $ drop v.offset ls

-- | The help screen is text without items, so a move scrolls it.
scrollHelp :: App es => MoveTarget -> Eff es ()
scrollHelp t = do
  env <- getAppEnv
  s <- getS
  let h = max 1 (listHeight env s (focusedView s))
  case t of
    MoveUp -> scroll (-1)
    MoveDown -> scroll 1
    MovePageUp -> scroll (-h)
    MovePageDown -> scroll h
    MoveFirst -> modifyWithEnv . modifyView $ #offset .~ 0
    MoveLast -> modifyWithEnv . modifyView $ #offset .~ screenLength env s HelpScreen
    _ -> showMessage $ "The " <> screenText HelpScreen <> " has no " <> renderAction (Move t)
  where
    scroll :: App es => Int -> Eff es ()
    scroll delta = modifyWithEnv . modifyView $ #offset %~ (+ delta)

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
