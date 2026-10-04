-- | Benchmarks of what reprise does often with a long queue: handling keys
-- and changes of the queue, and drawing frames.
module Main (main) where

import Control.DeepSeq
import Data.IORef
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix.Output qualified as V
import Graphics.Vty.Platform.Unix.Settings qualified as V
import System.Posix.IO
import Test.Tasty.Bench

import Reprise.Config
import Reprise.Event
import Reprise.Handler
import Reprise.Keys
import Reprise.Mpd.Mirror hiding (queueLength)
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Style
import Reprise.UI.Layout

main :: IO ()
main =
  defaultMain
    [ bgroup
        "handler"
        [ bench "down key" $ whnf (settled . handle (key "down")) loaded
        , bench "the change of the queue after a delete in the middle" $
            whnf (settled . handle deletedInTheMiddle) loaded
        , -- The first key makes the text of the rows, which the next keys
          -- reuse. With no match, the find goes through the whole queue.
          bench "the first key of a find that matches nothing" $
            whnf (settled . handle (key "x")) (keys ["/"] loaded)
        , bench "a later key of a find that matches nothing" $
            whnf (settled . handle (key "x")) (keys ["/", "z", "q"] loaded)
        ]
    , bgroup
        "screen"
        [ bench "a frame of the queue" $ nf renderScreen scrolled
        , env xterm $ \ ~(Terminal out dc frame) ->
            bench "the output of a whole frame for xterm-256color" . whnfIO $ do
              -- Without the last frame, vty writes every row.
              writeIORef (V.assumedStateRef out) V.initialAssumedState
              V.outputPicture dc (V.picForImage frame)
        ]
    ]

-- | The state after the queue arrived from MPD.
loaded :: AppState
loaded =
  L.foldl'
    (flip handle)
    (initialState defaultConfig (keymapsOf defaultConfig.keys) WithColors)
    [ uncurry Resized terminalSize
    , MpdConnected (Version 0 24 0)
    , QueueFetched (statusOf 1 queueLength, [song p p | p <- [0 .. queueLength - 1]])
    ]

-- | A queue scrolled down a little, as while a key is held.
scrolled :: AppState
scrolled = keys (replicate (snd terminalSize) "down") loaded

-- | The reply to @plchanges@ after the song in the middle of the queue was
-- deleted: every song after it moved up.
deletedInTheMiddle :: AppEvent
deletedInTheMiddle =
  QueueChangesFetched
    ( statusOf 2 (queueLength - 1)
    , [song (i - 1) i | i <- [queueLength `div` 2 + 1 .. queueLength - 1]]
    )

-- | vty's output to a terminal, which writes to @/dev/null@, and a frame
-- to write.
data Terminal = Terminal V.Output V.DisplayContext V.Image

-- | vty's output has no 'NFData' instance, and it is ready once it is made.
instance NFData Terminal where
  rnf (Terminal _ _ frame) = rnf frame

xterm :: IO Terminal
xterm = do
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
  pure (Terminal out dc (renderScreen scrolled))

----------------------------------------
-- Helpers

handle :: AppEvent -> AppState -> AppState
handle e s = let (s', _, _) = runEvent 1 e s in s'

key :: T.Text -> AppEvent
key = KeyPressed . either (error . T.unpack) id . parseKeySpec

keys :: [T.Text] -> AppState -> AppState
keys ks s = L.foldl' (flip handle) s (map key ks)

-- | A number that depends on what the events change: the queue, the
-- selection, the view and the prompt.
settled :: AppState -> Int
settled s =
  Seq.length s.mirror.queue
    + S.size s.queueState.selection
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
