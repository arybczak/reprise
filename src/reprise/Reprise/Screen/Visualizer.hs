-- | The visualizer: a goniometer of the samples that MPD's fifo output
-- writes. It turns the left and the right channel by 45°, so that mono is a
-- vertical line and stereo spreads to the sides. The samples are braille
-- dots, eight in a cell, and those of the last frames stay while they fade.
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
import Data.Foldable
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
  Just _ -> goniometer env.colorMode env.config.visualizer v.width v.height s.visualizer.frames

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
-- A cell has the color of the newest frame with a dot in it.
goniometer
  :: ColorMode -> VisualizerConfig -> Int -> Int -> Seq.Seq BS.ByteString -> V.Image
goniometer colorMode cfg w h frames = V.vertCat (map row [0 .. h - 1])
  where
    -- The dots of each cell, and the age of the newest frame with a dot in
    -- it.
    (dots, ages) = runST $ do
      dotsM <- MVU.replicate (w * h) (0 :: Word8)
      agesM <- MVU.replicate (w * h) (0 :: Int)
      -- The newer frames come later, so that their ages stay.
      forM_ (reverse (zip [0 ..] (toList frames))) $ \(age, pcm) ->
        forM_ (points pcm) $ \(dx, dy) -> do
          let cell = (dy `div` dotRows) * w + dx `div` dotColumns
          MVU.modify dotsM (.|. brailleBit (dx `mod` dotColumns) (dy `mod` dotRows)) cell
          MVU.write agesM cell age
      (,) <$> VU.unsafeFreeze dotsM <*> VU.unsafeFreeze agesM

    row :: Int -> V.Image
    row y = V.horizCat . map run $ runsOf [cellAt (y * w + x) | x <- [0 .. w - 1]]

    cellAt :: Int -> (Maybe Int, Char)
    cellAt i = case dots VU.! i of
      0 -> (Nothing, ' ')
      d -> (Just (colorIndex (ages VU.! i)), chr (brailleBlank + fromIntegral d))

    channels :: Int
    channels = if cfg.inStereo then 2 else 1

    -- The dots of a frame's samples. Full scale on both channels is the
    -- top or the bottom of the grid, and full scale on one with the other
    -- at zero is halfway to a side. The two axes scale apart, so that the
    -- picture fills a wide screen, as ncmpcpp's ellipse does.
    points :: BS.ByteString -> [(Int, Int)]
    points pcm =
      [ (round (centerX + x * centerX), round (centerY - y * centerY))
      | i <- [0 .. BS.length pcm `div` (bytesPerSample * channels) - 1]
      , let left = sampleAt pcm (i * channels)
            right = if cfg.inStereo then sampleAt pcm (i * channels + 1) else left
            x = (left - right) / 2
            y = (left + right) / 2
      ]

    dotsWide, dotsHigh :: Int
    dotsWide = w * dotColumns
    dotsHigh = h * dotRows

    centerX, centerY :: Double
    centerX = fromIntegral (dotsWide - 1) / 2
    centerY = fromIntegral (dotsHigh - 1) / 2

    colorIndex :: Int -> Int
    colorIndex age = min (NE.length cfg.colors - 1) (age * NE.length cfg.colors `div` trailFrames cfg)

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
