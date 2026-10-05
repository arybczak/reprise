-- | The visualizer, of the samples that MPD's fifo output writes:
--
-- * The spectrum, as bars of the levels of the frequencies, which the
--   worker computes. In stereo, the left channel rises from the middle and
--   the right one hangs from it.
-- * The ellipse: the left channel across and the right one up, as in
--   ncmpcpp's stereo ellipse. Mono is a diagonal, and stereo widens it. The
--   samples are braille dots, eight in a cell, and those of the last frames
--   stay for a moment.
--
-- The colors go from quiet to loud.
module Reprise.Screen.Visualizer
  ( visualizerView
  , visualizerSamples
  , visualizerSpectrum
  , nextVisualization
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
import Data.Vector.Storable qualified as VS
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
import Reprise.Visualizer.Spectrum
import Reprise.Width

visualizerView :: AppEnv -> AppState -> View -> V.Image
visualizerView env s v = case env.config.visualizer.dataSource of
  Nothing ->
    V.text' (toAttr env.colorMode mempty) . truncateToWidth v.width $
      "Set visualizer.data_source in the config to the fifo of MPD's fifo output"
  Just _ -> case s.toggles.visualization of
    Spectrum -> bars env.colorMode env.config.visualizer v.width v.height s.visualizer.spectrum
    Ellipse -> ellipse env.colorMode env.config.visualizer v.width v.height s.visualizer.frames

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
    modifyS $ #visualizer .~ VisualizerState wanted Seq.empty []
    visualize wanted

nextVisualization :: App es => Eff es ()
nextVisualization = do
  modifyS $ #toggles % #visualization %~ \v -> if v == maxBound then minBound else succ v
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
    else
      modifyS $
        #visualizer
          % #frames
          .~ Seq.take (trailFrames env.config.visualizer) (samples Seq.<| v.frames)

-- | Show the spectrum of a frame. A spectrum that comes after the spectrum
-- stopped reading is dropped.
visualizerSpectrum :: App es => [VS.Vector Double] -> Eff es ()
visualizerSpectrum spectra = do
  v <- getsS (.visualizer)
  if v.reading /= Just Spectrum
    then keepScreen
    else modifyS $ #visualizer % #spectrum .~ spectra

-- | The number of frames whose samples are on the screen.
trailFrames :: VisualizerConfig -> Int
trailFrames cfg =
  let FrameRate fps = cfg.fps
      Duration trail = cfg.trail
  in max 1 (round (realToFrac @_ @Double trail * fromIntegral fps))

----------------------------------------
-- The spectrum

-- | Which way the bars of a channel go.
data Growth = Rising | Hanging

-- | The bars of the spectra of the channels in a grid of the given size.
bars :: ColorMode -> VisualizerConfig -> Int -> Int -> [VS.Vector Double] -> V.Image
bars colorMode cfg w h = \case
  [] -> V.emptyImage
  [mono] -> channel Rising h mono
  left : right : _ ->
    let top = h `div` 2
    in channel Rising top left V.<-> channel Hanging (h - top) right
  where
    channel :: Growth -> Int -> VS.Vector Double -> V.Image
    channel growth rows magnitudes =
      let ls = levels w magnitudes
          row i =
            let fromFoot = case growth of
                  Rising -> rows - 1 - i
                  Hanging -> i
            in V.horizCat . map (run colorMode) $ runsOf [cell growth rows fromFoot l | l <- ls]
      in V.vertCat (map row [0 .. rows - 1])

    -- The part of a bar in a row, from the foot of the bar.
    cell :: Growth -> Int -> Int -> Double -> (Style, Char)
    cell growth rows fromFoot level =
      let filled = level * fromIntegral rows - fromIntegral fromFoot
          eighths = floor @Double @Int (filled * fromIntegral eighthsPerCell)
          n = NE.length cfg.colors
          color = cfg.colors NE.!! min (n - 1) (fromFoot * n `div` rows)
      in if
           | filled >= 1 -> (color, fullBlock)
           | eighths <= 0 -> (mempty, ' ')
           | otherwise -> case growth of
               Rising -> (color, lowerBlock eighths)
               Hanging -> (color, upperBlock eighths)

-- | The level of each of the columns, from 0 to 1. The columns go from the
-- lowest frequency that people hear to the highest, on a log scale, as in
-- ncmpcpp. A column has the mean of the magnitudes of its bins. The low
-- columns are narrower than a bin, so they interpolate between the bins
-- around their middles.
levels :: Int -> VS.Vector Double -> [Double]
levels w magnitudes = map level [0 .. w - 1]
  where
    level :: Int -> Double
    level x =
      let low = lowestFrequency * ratio ** fromIntegral x
          high = low * ratio
          first = ceiling (low / binWidth)
          final = min (VS.length magnitudes - 1) (ceiling (high / binWidth) - 1)
          magnitude
            | first <= final =
                VS.sum (VS.slice first (final - first + 1) magnitudes)
                  / fromIntegral (final - first + 1)
            | otherwise = interpolated (sqrt (low * high) / binWidth)
          decibels = 20 * logBase 10 magnitude
      in max 0 . min 1 $ (decibels - quietest) / (loudest - quietest)

    -- At a fractional bin.
    interpolated :: Double -> Double
    interpolated bin =
      let k = floor bin
          f = bin - fromIntegral k
      in (1 - f) * magnitudeOf k + f * magnitudeOf (k + 1)

    magnitudeOf :: Int -> Double
    magnitudeOf k = fromMaybe 0 (magnitudes VS.!? k)

    ratio :: Double
    ratio = (highestFrequency / lowestFrequency) ** (1 / fromIntegral w)

    binWidth :: Double
    binWidth = binFrequency 1

-- | The range of human hearing, in Hz.
lowestFrequency, highestFrequency :: Double
lowestFrequency = 20
highestFrequency = 20000

-- | The magnitudes, in dB, of an empty bar and of a full one, as in ncmpcpp
-- with the author's @visualizer_spectrum_gain@ of 10.
quietest, loudest :: Double
quietest = -100
loudest = -10

-- | The block characters split a cell in eighths.
eighthsPerCell :: Int
eighthsPerCell = 8

fullBlock :: Char
fullBlock = '█'

-- | The block of the lower eighths of a cell, of 1 to 7.
lowerBlock :: Int -> Char
lowerBlock eighths = chr (ord '▁' + eighths - 1)

-- | The block of the upper eighths of a cell, of 1 to 7. Most are in
-- Symbols for Legacy Computing, as in ncmpcpp. A lower block in reverse
-- video would be the same, but the terminal would draw the rest of the
-- cell in its default background, which isn't that of a transparent
-- terminal.
upperBlock :: Int -> Char
upperBlock eighths = "▔🮂🮃▀🮄🮅🮆" !! (eighths - 1)

----------------------------------------
-- The ellipse

-- | The samples of the frames as braille dots in a grid of the given size.
-- A cell has the color of its loudest sample.
ellipse
  :: ColorMode -> VisualizerConfig -> Int -> Int -> Seq.Seq BS.ByteString -> V.Image
ellipse colorMode cfg w h frames = V.vertCat (map row [0 .. h - 1])
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
    row y = V.horizCat . map (run colorMode) $ runsOf [cellAt (y * w + x) | x <- [0 .. w - 1]]

    cellAt :: Int -> (Style, Char)
    cellAt i = case dots VU.! i of
      0 -> (mempty, ' ')
      d -> (cfg.colors NE.!! (colors VU.! i), chr (brailleBlank + fromIntegral d))

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

----------------------------------------
-- Helpers

-- | Runs of cells of the same style, with their characters.
runsOf :: [(Style, Char)] -> [(Style, String)]
runsOf = \case
  [] -> []
  (style, c) : rest ->
    let (same, other) = span ((== style) . fst) rest
    in (style, c : map snd same) : runsOf other

run :: ColorMode -> (Style, String) -> V.Image
run colorMode (style, text) = V.string (toAttr colorMode style) text
