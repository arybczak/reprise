-- | The visualizer, of the samples that MPD's fifo output writes:
--
-- * The spectrum, as bars of the levels of the frequencies, which the
--   worker computes. The left channel rises from the middle and the right
--   one hangs from it.
-- * The ellipse: the left channel across and the right one up, as in
--   ncmpcpp's stereo ellipse. Music in mono is a diagonal, and stereo widens
--   it. The samples are braille dots, eight in a cell, and those of the last
--   frames stay for a moment.
-- * The wave: the samples of each channel over time, as in ncmpcpp's sound
--   wave, of braille dots too, from where the worker found the bass rising.
--   The left channel is in the top half and the right one in the bottom
--   half.
--
-- The colors go from quiet to loud. "Reprise.Visualizer.Draw" draws them.
module Reprise.Screen.Visualizer
  ( visualizerView
  , visualizerSamples
  , visualizerSpectrum
  , visualizerWave
  , visualizerStats
  , nextVisualization
  , updateVisualizer
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Handler.Core
import Reprise.State
import Reprise.Style
import Reprise.Visualizer.Draw
import Reprise.Width

visualizerView :: AppEnv -> AppState -> View -> V.Image
visualizerView env s v = case env.config.visualizer.dataSource of
  Nothing ->
    V.text' (toAttr env.colorMode mempty) . truncateToWidth v.width $
      "Set visualizer.data_source in the config to the fifo of an MPD fifo output of format 44100:16:2"
  Just _
    | env.config.visualizer.debug ->
        V.cropBottom v.height $ debugLine V.<-> picture (max 0 (v.height - 1))
    | otherwise -> picture v.height
  where
    picture :: Int -> V.Image
    picture h = case s.toggles.visualization of
      Spectrum -> bars env.colorMode env.config.visualizer v.width h s.visualizer.spectrum
      Ellipse -> ellipse env.colorMode env.config.visualizer v.width h s.visualizer.frames
      Wave -> wave env.colorMode env.config.visualizer v.width h s.visualizer.wave

    debugLine :: V.Image
    debugLine =
      V.text' (toAttr env.colorMode env.config.styles.value) . truncateToWidth v.width $
        T.pack (show (Seq.length s.visualizer.drawn))
          <> " fps drawn"
          <> foldMap worker s.visualizer.stats

    worker :: FrameStats -> T.Text
    worker st =
      "; worker: "
        <> T.intercalate
          ", "
          [ number st.frames <> " frames"
          , number st.late <> " late"
          , number st.empty <> " empty"
          , number st.dropped <> " dropped"
          , number st.bytes <> " B read"
          ]

    number :: Int -> T.Text
    number = T.pack . show

-- | Read the samples while the visualizer shows, and only then. It runs
-- after every event.
updateVisualizer :: App es => Eff es ()
updateVisualizer = do
  env <- getAppEnv
  s <- getS
  let wanted = do
        guard $
          isJust env.config.visualizer.dataSource && (focusedView s).screen == VisualizerScreen
        pure s.toggles.visualization
  when (wanted /= s.visualizer.reading) $ do
    modifyS $
      #visualizer .~ VisualizerState wanted Seq.empty Nothing Nothing Seq.empty Nothing
    visualize wanted

nextVisualization :: App es => Eff es ()
nextVisualization = do
  modifyS $ #toggles % #visualization %~ cycleNext
  v <- getsS (.toggles.visualization)
  showMessage $ "Visualization: " <> visualizationName v

-- | Add the samples of a frame to those of the ellipse on the screen.
-- Samples that come after the ellipse stopped reading are dropped.
visualizerSamples :: App es => BS.ByteString -> Eff es ()
visualizerSamples samples = do
  env <- getAppEnv
  v <- getsS (.visualizer)
  if v.reading /= Just Ellipse || (BS.null samples && all BS.null v.frames)
    then keepScreen
    else do
      modifyS $
        #visualizer
          % #frames
          .~ Seq.take (trailFrames env.config.visualizer) (samples Seq.<| v.frames)
      countDrawn

-- | Show the spectrum of a frame. A spectrum that comes after the spectrum
-- stopped reading is dropped.
visualizerSpectrum :: App es => VS.Vector Double -> VS.Vector Double -> Eff es ()
visualizerSpectrum left right = do
  v <- getsS (.visualizer)
  if v.reading /= Just Spectrum
    then keepScreen
    else do
      modifyS $ #visualizer % #spectrum ?~ (left, right)
      countDrawn

-- | Show the wave of a frame. A wave that comes after the wave stopped
-- reading is dropped.
visualizerWave :: App es => BS.ByteString -> Eff es ()
visualizerWave samples = do
  v <- getsS (.visualizer)
  if v.reading /= Just Wave
    then keepScreen
    else do
      modifyS $ #visualizer % #wave ?~ samples
      countDrawn

-- | Show what happened to the worker's frames in the last second.
visualizerStats :: App es => FrameStats -> Eff es ()
visualizerStats stats = do
  v <- getsS (.visualizer)
  now <- getsS (.now)
  if isNothing v.reading
    then keepScreen
    else modifyS $ #visualizer %~ (#stats ?~ stats) . (#drawn %~ lastSecond now)

-- | Count a frame that the screen draws, with @visualizer.debug@.
countDrawn :: App es => Eff es ()
countDrawn = do
  env <- getAppEnv
  when env.config.visualizer.debug $ do
    now <- getsS (.now)
    modifyS $ #visualizer % #drawn %~ lastSecond now . (Seq.|> now)

-- | The times of the last second.
lastSecond :: Double -> Seq.Seq Double -> Seq.Seq Double
lastSecond now = Seq.dropWhileL (<= now - 1)

-- | The number of frames whose samples are on the screen.
trailFrames :: VisualizerConfig -> Int
trailFrames cfg =
  let FrameRate fps = cfg.fps
      Duration trail = cfg.trail
  in max 1 (round (realToFrac @_ @Double trail * fromIntegral fps))
