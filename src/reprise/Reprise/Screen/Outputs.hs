-- | The outputs screen: MPD's audio outputs, which activate enables and
-- disables. An enabled output has the style of the song that plays, as
-- ncmpcpp shows it bold.
module Reprise.Screen.Outputs
  ( outputsView
  , outputsVerb
  , showOutputs
  , refreshOutputs
  , outputsFetched
  ) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Event
import Reprise.Format
import Reprise.Handler.Core
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.UI.SongList

outputsView :: AppEnv -> AppState -> View -> V.Image
outputsView env s v =
  let ctx = rowContext env Classic v.width
      row :: Int -> Output -> V.Image
      row i o =
        renderOtherRow
          ctx
          RowFlags
            { queued = False
            , playing = o.enabled
            , selected = False
            , found = False
            , cursor = i == v.cursor
            }
          [Span Nothing o.name]
  in V.vertCat . zipWith row [v.offset ..] . toList $
       visibleItems env s v (fromMaybe Seq.empty s.outputs)

-- | How the outputs screen does a verb, if it does it.
outputsVerb :: App es => Action -> Maybe (Eff es ())
outputsVerb = \case
  Move t -> Just $ modifyWithEnv (moveListCursor t)
  Activate -> Just toggleOutput
  _ -> Nothing

-- | Show the outputs, which are fetched the first time. They count as
-- shown before they come, so that a new connection fetches them again
-- after a failed fetch.
showOutputs :: App es => Eff es ()
showOutputs = do
  switchTo OutputsScreen
  loaded <- getsS (isJust . (.outputs))
  unless loaded $ do
    modifyS $ #outputs ?~ Seq.empty
    request outputs OutputsFetched

-- | Fetch the outputs again, once the screen showed them, e.g. after a
-- change of them or a new connection.
refreshOutputs :: App es => Eff es ()
refreshOutputs = do
  loaded <- getsS (isJust . (.outputs))
  when loaded $ request outputs OutputsFetched

outputsFetched :: App es => [Output] -> Eff es ()
outputsFetched fetched = do
  modifyS $ #outputs ?~ Seq.fromList fetched
  -- Fewer outputs can leave the cursor past the last.
  screen <- getsS ((.screen) . focusedView)
  when (screen == OutputsScreen) . modifyWithEnv $ modifyView id

-- | Enable the output under the cursor, or disable it. Its new state comes
-- back as a change of the outputs.
toggleOutput :: App es => Eff es ()
toggleOutput = do
  s <- getS
  forM_ (s.outputs >>= Seq.lookup (focusedView s).cursor) $ \o -> do
    mutate $ (if o.enabled then disableOutput else enableOutput) o.outputId
    showMessage $
      "Output \"" <> o.name <> "\" " <> if o.enabled then "disabled" else "enabled"
