-- | The visualizer: the samples that MPD's fifo output writes, with the
-- left channel across and the right one up, as in ncmpcpp's stereo ellipse.
-- Mono is a diagonal, and stereo widens it. The samples are braille dots,
-- eight in a cell, colored by how loud they are, and those of the last
-- frames stay for a moment.
module Reprise.Screen.Visualizer
  ( visualizerView
  , visualizerSamples
  , updateVisualizer
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.ByteString qualified as BS
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Data.Word
import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Handler.Core
import Reprise.State
import Reprise.Style
import Reprise.Visualizer.Samples
import Reprise.Width

visualizerView :: AppEnv -> AppState -> View -> V.Image
visualizerView env s v = case env.config.visualizer.dataSource of
  Nothing ->
    V.text' (toAttr env.colorMode mempty) . truncateToWidth v.width $
      "Set visualizer.data_source in the config to the fifo of MPD's fifo output"
  Just _ -> scope env.colorMode env.config.visualizer v.width v.height s.visualizer.frames

-- | Read the samples while the visualizer shows, and only then. It runs
-- after every event.
updateVisualizer :: App es => Eff es ()
updateVisualizer = do
  env <- getAppEnv
  s <- getS
  let wanted =
        isJust env.config.visualizer.dataSource && (focusedView s).screen == VisualizerScreen
  when (wanted /= s.visualizer.reading) $ do
    modifyS $ #visualizer .~ VisualizerState wanted Seq.empty
    visualize wanted

-- | Add the samples of a frame to those on the screen. Samples that come
-- after the visualizer stopped reading are dropped.
visualizerSamples :: App es => BS.ByteString -> Eff es ()
visualizerSamples samples = do
  env <- getAppEnv
  v <- getsS (.visualizer)
  if not v.reading || (BS.null samples && all BS.null v.frames)
    then keepScreen
    else
      modifyS $
        #visualizer
          % #frames
          .~ Seq.take (trailFrames env.config.visualizer) (samples Seq.<| v.frames)

-- | The number of frames whose samples are on the screen.
trailFrames :: VisualizerConfig -> Int
trailFrames cfg =
  let FrameRate fps = cfg.fps
      Duration trail = cfg.trail
  in max 1 (round (realToFrac @_ @Double trail * fromIntegral fps))

-- | The samples of the frames as braille dots in a grid of the given size.
-- A cell has the color of its loudest sample.
scope
  :: ColorMode -> VisualizerConfig -> Int -> Int -> Seq.Seq BS.ByteString -> V.Image
scope colorMode cfg w h frames = V.vertCat (map row [0 .. h - 1])
  where
    -- The dots of each cell, and the color of its loudest sample.
    (dots, colors) = runST $ do
      dotsM <- MVU.replicate (w * h) (0 :: Word8)
      colorsM <- MVU.replicate (w * h) (0 :: Int)
      forM_ frames $ \pcm ->
        forM_ (points pcm) $ \(dx, dy, color) -> do
          let cell = (dy `div` dotRows) * w + dx `div` dotColumns
          MVU.modify dotsM (.|. brailleBit (dx `mod` dotColumns) (dy `mod` dotRows)) cell
          MVU.modify colorsM (max color) cell
      (,) <$> VU.unsafeFreeze dotsM <*> VU.unsafeFreeze colorsM

    row :: Int -> V.Image
    row y = V.horizCat . map run $ runsOf [cellAt (y * w + x) | x <- [0 .. w - 1]]

    cellAt :: Int -> (Maybe Int, Char)
    cellAt i = case dots VU.! i of
      0 -> (Nothing, ' ')
      d -> (Just (colors VU.! i), chr (brailleBlank + fromIntegral d))

    channels :: Int
    channels = if cfg.inStereo then 2 else 1

    -- The dots of a frame's samples, with their colors: the left channel
    -- across and the right one up, each at full scale at the edges of the
    -- grid.
    points :: BS.ByteString -> [(Int, Int, Int)]
    points pcm =
      [ (round (centerX + left * centerX), round (centerY - right * centerY), colorOf left right)
      | i <- [0 .. BS.length pcm `div` (bytesPerSample * channels) - 1]
      , let left = sampleAt pcm (i * channels)
            right = if cfg.inStereo then sampleAt pcm (i * channels + 1) else left
      ]

    -- By the distance from the center, as the root mean square of the
    -- channels, so that mono at full scale has the last color.
    colorOf :: Double -> Double -> Int
    colorOf left right =
      let loudness = sqrt ((left * left + right * right) / 2)
          n = NE.length cfg.colors
      in min (n - 1) (floor (loudness * fromIntegral n))

    dotsWide, dotsHigh :: Int
    dotsWide = w * dotColumns
    dotsHigh = h * dotRows

    centerX, centerY :: Double
    centerX = fromIntegral (dotsWide - 1) / 2
    centerY = fromIntegral (dotsHigh - 1) / 2

    run :: (Maybe Int, String) -> V.Image
    run (color, text) =
      V.string (toAttr colorMode (maybe mempty (cfg.colors NE.!!) color)) text

-- | Runs of cells of the same color, with their characters.
runsOf :: [(Maybe Int, Char)] -> [(Maybe Int, String)]
runsOf = \case
  [] -> []
  (color, c) : rest ->
    let (same, other) = span ((== color) . fst) rest
    in (color, c : map snd same) : runsOf other

-- | A braille character has two columns of four dots.
dotColumns, dotRows :: Int
dotColumns = 2
dotRows = 4

-- | The braille character without dots. Its dots are bits on top of it.
brailleBlank :: Int
brailleBlank = 0x2800

-- | The bit of the dot in a column and a row of a braille character. Dots 1
-- to 6 go down the columns, and dots 7 and 8 are the bottom row, which
-- braille added later.
brailleBit :: Int -> Int -> Word8
brailleBit column row
  | row == dotRows - 1 = bit ((dotRows - 1) * dotColumns + column)
  | otherwise = bit (row + (dotRows - 1) * column)
