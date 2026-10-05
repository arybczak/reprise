-- | Benchmarks of what reprise does often with a long queue: handling keys
-- and changes of the queue, and drawing frames.
module Main (main) where

import Control.DeepSeq
import Control.Monad
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Int
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import Data.Word
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix.Output qualified as V
import Graphics.Vty.Platform.Unix.Settings qualified as V
import Optics.Core
import System.Posix.IO
import Test.Tasty.Bench

import Reprise.Collation
import Reprise.Config
import Reprise.Event
import Reprise.Handler
import Reprise.Keys
import Reprise.Mpd.Mirror hiding (queueLength)
import Reprise.Mpd.Protocol.Types
import Reprise.Selection
import Reprise.State
import Reprise.Style
import Reprise.UI.Layout
import Reprise.Visualizer.Spectrum

main :: IO ()
main =
  defaultMain
    [ bgroup
        "handler"
        [ afterKeys [] $ \s ->
            bench "down key" . whnfIO $ settled <$> handle (key "down") s
        , afterKeys [] $ \s ->
            bench "the change of the queue after a delete in the middle" . whnfIO $
              settled <$> handle deletedInTheMiddle s
        , -- The first key makes the text of the rows, which the next keys
          -- reuse. With no match, the find goes through the whole queue.
          afterKeys ["/"] $ \s ->
            bench "the first key of a find that matches nothing" . whnfIO $
              settled <$> handle (key "x") s
        , afterKeys ["/", "z", "q"] $ \s ->
            bench "a later key of a find that matches nothing" . whnfIO $
              settled <$> handle (key "x") s
        , -- Sorting by name computes a collation key for each entry.
          afterKeys ["2", "ctrl-t", "o"] $ \s ->
            bench "the listing of a directory as long as the queue, sorted by name" . whnfIO $
              settled <$> listed s
        ]
    , bgroup
        "screen"
        [ afterKeys scrolling $ \s ->
            bench "a frame of the queue" $ nf (renderScreen defaultEnv) s
        , env (xterm (renderScreen defaultEnv <$> (keys scrolling =<< loaded))) $ \t ->
            bench "the output of a whole frame for xterm-256color" . whnfIO $ output t
        , env (Settled <$> ellipseShown) $ \ ~(Settled s) ->
            bench "a frame of the ellipse" $ nf (renderScreen visualizerEnv) s
        , env (xterm (renderScreen visualizerEnv <$> ellipseShown)) $ \t ->
            bench "the output of a whole frame of the ellipse for xterm-256color" . whnfIO $
              output t
        , env (Planned <$> newTransform) $ \ ~(Planned transform) ->
            bench "the spectra of the channels for a frame" . nfIO $ spectraOf transform noiseWindow
        , env (Settled <$> spectrumShown) $ \ ~(Settled s) ->
            bench "a frame of the spectrum" $ nf (renderScreen visualizerEnv) s
        , env (xterm (renderScreen visualizerEnv <$> spectrumShown)) $ \t ->
            bench "the output of a whole frame of the spectrum for xterm-256color" . whnfIO $
              output t
        ]
    ]
  where
    -- Without the last frame, vty writes every row.
    output :: Terminal -> IO ()
    output ~(Terminal out dc frame) = do
      writeIORef (V.assumedStateRef out) V.initialAssumedState
      V.outputPicture dc (V.picForImage frame)

-- | The default config and keymaps, with colors.
defaultEnv :: AppEnv
defaultEnv =
  AppEnv
    { config = defaultConfig
    , keymaps = keymapsOf defaultConfig.keys
    , colorMode = WithColors
    , collator = userCollator
    }

-- | The default config with a data source of the visualizer.
visualizerEnv :: AppEnv
visualizerEnv = defaultEnv & #config % #visualizer % #dataSource ?~ "fifo"

-- | A benchmark of the state after the queue arrived from MPD and the keys
-- were pressed.
afterKeys :: [T.Text] -> (AppState -> Benchmark) -> Benchmark
afterKeys ks b = env (Settled <$> (keys ks =<< loaded)) $ \ ~(Settled s) -> b s

-- | The state after the queue arrived from MPD.
loaded :: IO AppState
loaded =
  foldM
    (flip handle)
    (initialState defaultConfig)
    [ uncurry Resized terminalSize
    , MpdConnected (Version 0 24 0)
    , QueueFetched (statusOf 1 queueLength, [song p p | p <- [0 .. queueLength - 1]])
    ]

-- | The state has no 'NFData' instance, and what 'settled' depends on is
-- what the benchmarks change.
newtype Settled = Settled AppState

instance NFData Settled where
  rnf (Settled s) = rnf (settled s)

-- | The plan of a transform has no 'NFData' instance, and it is ready once
-- it is made.
newtype Planned = Planned Transform

instance NFData Planned where
  rnf (Planned t) = t `seq` ()

-- | The keys that scroll the queue down a little, as while a key is held.
scrolling :: [T.Text]
scrolling = replicate (snd terminalSize) "down"

-- | The ellipse after a second of noise, which covers much of the screen as
-- loud music does.
ellipseShown :: IO AppState
ellipseShown = do
  s <- loaded
  foldM
    (flip (handleIn visualizerEnv))
    s
    (key "8" : key "space" : map VisualizerSamples noise)

-- | The spectrum of the noise, which has bars as high as loud music does.
spectrumShown :: IO AppState
spectrumShown = do
  s <- loaded
  spectra <- (`spectraOf` noiseWindow) =<< newTransform
  foldM (flip (handleIn visualizerEnv)) s [key "8", VisualizerSpectrum spectra]

-- | The spectra of the channels, as the worker computes them for a frame.
spectraOf :: Transform -> BS.ByteString -> IO [VS.Vector Double]
spectraOf transform window = forM [0, 1] $ \c -> spectrumOf transform 2 c window

-- | The noise of a spectrum's window.
noiseWindow :: BS.ByteString
noiseWindow = BS.takeEnd (windowSamples * 4) (BS.concat noise)

-- | A second of frames of noise, as MPD writes them.
noise :: [BS.ByteString]
noise = [frame (take frameLength (drop (i * frameLength) samples)) | i <- [0 .. fps - 1]]
  where
    frame :: [(Int16, Int16)] -> BS.ByteString
    frame = BL.toStrict . BB.toLazyByteString . foldMap (\(l, r) -> BB.int16LE l <> BB.int16LE r)

    -- The two channels have some of their noise in common.
    samples :: [(Int16, Int16)]
    samples = pairs (map sample (iterate next 1))

    pairs :: [Int] -> [(Int16, Int16)]
    pairs = \case
      a : b : rest -> (fromIntegral a, fromIntegral ((a + b) `div` 2)) : pairs rest
      _ -> []

    -- Numerical Recipes' linear congruential generator, at half of full
    -- scale.
    next :: Word32 -> Word32
    next x = 1664525 * x + 1013904223

    sample :: Word32 -> Int
    sample w = fromIntegral (fromIntegral @Word32 @Int16 (w `shiftR` 16)) `div` 2

    fps :: Int
    fps = 60

    frameLength :: Int
    frameLength = 44100 `div` fps

-- | The reply to @plchanges@ after the song in the middle of the queue was
-- deleted: every song after it moved up.
deletedInTheMiddle :: AppEvent
deletedInTheMiddle =
  QueueChangesFetched
    ( statusOf 2 (queueLength - 1)
    , [song (i - 1) i | i <- [queueLength `div` 2 + 1 .. queueLength - 1]]
    )

-- | The reply to the browser's listing on its way: a directory with a song
-- for each one of the queue.
listed :: AppState -> IO AppState
listed s = case s.browser.listing of
  Just l -> handle (BrowserListed l.token [SongEntry (song p p) | p <- [0 .. queueLength - 1]]) s
  Nothing -> error "no listing on its way"

-- | vty's output to a terminal, which writes to @/dev/null@, and a frame
-- to write.
data Terminal = Terminal V.Output V.DisplayContext V.Image

-- | vty's output has no 'NFData' instance, and it is ready once it is made.
instance NFData Terminal where
  rnf (Terminal _ _ frame) = rnf frame

xterm :: IO V.Image -> IO Terminal
xterm frame = do
  devNull <- openFd "/dev/null" WriteOnly defaultFileFlags
  out <-
    V.buildOutput
      V.defaultConfig
      V.UnixSettings
        { V.settingVmin = 1
        , V.settingVtime = 0
        , V.settingInputFd = stdInput
        , V.settingOutputFd = devNull
        , V.settingTermName = "xterm-256color"
        }
  dc <- V.displayContext out terminalSize
  Terminal out dc <$> frame

----------------------------------------
-- Helpers

handle :: AppEvent -> AppState -> IO AppState
handle = handleIn defaultEnv

handleIn :: AppEnv -> AppEvent -> AppState -> IO AppState
handleIn e event s = do
  (s', _, _) <- runEvent e 1 event s
  pure s'

key :: T.Text -> AppEvent
key = KeyPressed . either (error . T.unpack) id . parseKeySpec

keys :: [T.Text] -> AppState -> IO AppState
keys ks s = foldM (flip handle) s (map key ks)

-- | A number that depends on what the events change: the queue, the
-- selection, the browser's items, the visualizer's frames and spectra, the
-- view and the prompt.
settled :: AppState -> Int
settled s =
  Seq.length s.mirror.queue
    + S.size s.queueState.selection.keys
    + Seq.length s.browser.items
    + Seq.length s.visualizer.frames
    + sum (map VS.length s.visualizer.spectrum)
    + (focusedView s).cursor
    + (focusedView s).offset
    + maybe 0 (T.length . (.question)) s.prompt

-- | A song at a position, with the tags that the default columns show.
song :: Int -> Int -> Song
song pos i =
  Song
    { file = "dir/" <> T.pack (show i) <> ".flac"
    , tags =
        M.fromList
          [ (Artist, ["Artist number " <> T.pack (show (i `div` 20))])
          , (Album, ["An album called " <> T.pack (show (i `div` 10))])
          , (Title, ["The title of song " <> T.pack (show i) <> " in the queue"])
          , (Track, [T.pack (show (i `mod` 10 + 1))])
          ]
    , duration = Just 200
    , range = Nothing
    , lastModified = Nothing
    , format = Nothing
    , position = Just (SongPos pos)
    , songId = Just (SongId (i + 1))
    , priority = 0
    }

statusOf :: Int -> Int -> Status
statusOf version len =
  Status
    { volume = Just 50
    , repeat = False
    , random = False
    , single = SingleOff
    , consume = ConsumeOff
    , playlistVersion = PlaylistVersion (fromIntegral version)
    , playlistLength = len
    , state = Stopped
    , currentPosition = Nothing
    , currentId = Nothing
    , nextPosition = Nothing
    , nextId = Nothing
    , elapsed = Nothing
    , duration = Nothing
    , bitrate = Nothing
    , crossfade = 0
    , audio = Nothing
    , updatingDb = Nothing
    , error = Nothing
    }

-- | The length of the queue that the work on performance measured.
queueLength :: Int
queueLength = 4254

-- | The terminal that the work on performance measured.
terminalSize :: (Int, Int)
terminalSize = (160, 45)
