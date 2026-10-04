-- | Helpers shared by the tests.
module Utils
  ( -- * Songs
    song
  , statusOf

    -- * State
  , testState
  , runEvents
  , Result (..)

    -- * Images
  , imageLines
  , imageSpans
  ) where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Graphics.Text.Width qualified as W
import Graphics.Vty qualified as V
import Graphics.Vty.Image.Internal qualified as VI
import MPD.Protocol.Request
import MPD.Types

import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Handler
import Reprise.State
import Reprise.Style

----------------------------------------
-- Songs

-- | A song in the queue at a position, with its id one more than the
-- position.
song :: Int -> [(Tag, [T.Text])] -> Seconds -> Song
song pos tags len =
  Song
    { file = "dir/" <> T.pack (show pos) <> ".flac"
    , tags = M.fromList tags
    , duration = Just len
    , lastModified = Nothing
    , format = Nothing
    , position = Just (SongPos pos)
    , songId = Just (SongId (pos + 1))
    , priority = 0
    }

-- | A status with the given state and current position.
statusOf :: PlayerState -> Maybe Int -> Int -> Status
statusOf st current queueLen =
  Status
    { volume = Just 50
    , repeat = False
    , random = False
    , single = SingleOff
    , consume = ConsumeOff
    , playlistVersion = PlaylistVersion 1
    , playlistLength = queueLen
    , state = st
    , currentPosition = SongPos <$> current
    , currentId = SongId . (+ 1) <$> current
    , nextPosition = Nothing
    , nextId = Nothing
    , elapsed = 10 <$ current
    , duration = 60 <$ current
    , bitrate = Nothing
    , crossfade = 0
    , audio = Nothing
    , updatingDb = Nothing
    , error = Nothing
    }

----------------------------------------
-- State

-- | The state with the default config and keymaps, a terminal of the given
-- size, and a queue that MPD sent.
testState :: (Int, Int) -> Status -> [Song] -> AppState
testState size st songs =
  let defaults = either (error . unlines) id defaultKeymapOverrides
      s0 = initialState defaultConfig (keymapsOf defaults defaultConfig.keys) WithColors
      events =
        [ Resized (fst size) (snd size)
        , MpdConnected (Version 0 24 0)
        , QueueFetched (st, songs)
        ]
  in (runEvents 0 events s0).state

data Result = Result
  { state :: AppState
  , requests :: [[Request]]
  , pending :: [PendingRequest]
  , commands :: [UiCommand]
  }

-- | Handle events at a monotonic time, and collect what they requested.
runEvents :: Double -> [AppEvent] -> AppState -> Result
runEvents now events s0 = L.foldl' step (Result s0 [] [] []) events
  where
    step :: Result -> AppEvent -> Result
    step r e =
      let (s, ps, cs) = runEvent now e r.state
      in Result s (r.requests <> map pendingRequestLines ps) (r.pending <> ps) (r.commands <> cs)

----------------------------------------
-- Images

-- | The text of an image, one line for each row, without styles.
-- | The pieces of text of an image with their attributes, in the order of
-- the image's structure. Cropping is ignored.
imageSpans :: V.Image -> [(V.Attr, T.Text)]
imageSpans = \case
  VI.HorizText {VI.attr = a, VI.displayText = t} -> [(a, TL.toStrict t)]
  VI.HorizJoin {VI.partLeft = l, VI.partRight = r} -> imageSpans l <> imageSpans r
  VI.VertJoin {VI.partTop = t, VI.partBottom = b} -> imageSpans t <> imageSpans b
  VI.Crop i _ _ _ _ -> imageSpans i
  _ -> []

imageLines :: V.Image -> [T.Text]
imageLines = map T.stripEnd . render
  where
    render :: V.Image -> [T.Text]
    render = \case
      VI.HorizText {VI.displayText = t} -> [TL.toStrict t]
      VI.HorizJoin {VI.partLeft = l, VI.partRight = r} -> zipWith (<>) (render l) (render r)
      VI.VertJoin {VI.partTop = t, VI.partBottom = b} -> render t <> render b
      VI.BGFill {VI.outputWidth = w, VI.outputHeight = h} -> replicate h (T.replicate w " ")
      VI.Crop i l t w h -> map (padTo w . takeColumns w . dropColumns l) . take h . drop t $ render i
      VI.EmptyImage -> []

    padTo :: Int -> T.Text -> T.Text
    padTo w t = t <> T.replicate (w - width t) " "

    width :: T.Text -> Int
    width = T.foldl' (\n c -> n + W.safeWcwidth c) 0

    takeColumns :: Int -> T.Text -> T.Text
    takeColumns n t = T.pack . go n $ T.unpack t
      where
        go :: Int -> String -> String
        go _ [] = []
        go k (c : cs)
          | W.safeWcwidth c <= k = c : go (k - W.safeWcwidth c) cs
          | otherwise = []

    dropColumns :: Int -> T.Text -> T.Text
    dropColumns n t = T.pack . go n $ T.unpack t
      where
        go :: Int -> String -> String
        go k cs | k <= 0 = cs
        go _ [] = []
        go k (c : cs) = go (k - W.safeWcwidth c) cs
