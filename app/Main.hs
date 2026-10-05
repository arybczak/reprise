-- | The entry point of reprise.
module Main
  ( main
  ) where

import Brick qualified as B
import Brick.BChan qualified as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Version
import Effectful
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix qualified as V
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Paths_reprise qualified as Paths
import Reprise.App
import Reprise.Collation
import Reprise.Config
import Reprise.Effect.Mpd
import Reprise.Mpd.Address
import Reprise.Mpd.Worker
import Reprise.State
import Reprise.Style
import Reprise.Visualizer.Worker
import Reprise.Width

data Options = Options
  { host :: Maybe T.Text
  , port :: Maybe Int
  , config :: Maybe FilePath
  }

options :: Parser Options
options =
  Options
    <$> optional
      ( strOption
          ( long "host"
              <> metavar "HOST"
              <> help "The host or the socket of MPD"
          )
      )
    <*> optional
      ( option
          auto
          ( long "port"
              <> metavar "PORT"
              <> help "The port of MPD"
          )
      )
    <*> optional
      ( strOption
          ( long "config"
              <> metavar "FILE"
              <> help "The configuration file"
          )
      )

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (options <**> helper <**> simpleVersioner (showVersion Paths.version))
        (fullDesc <> progDesc "A terminal client for the Music Player Daemon")
  configFile <-
    maybe (getXdgDirectory XdgConfig ("reprise" </> "config.yaml")) pure opts.config
  config <-
    loadConfig configFile >>= \case
      Right c -> pure c
      Left errs -> do
        mapM_ (hPutStrLn stderr) errs
        exitFailure
  colorMode <-
    lookupEnv "NO_COLOR" <&> \case
      Just v | not (null v) -> NoColors
      _ -> WithColors
  settings <- resolveSettings config.mpd <$> sources opts
  logLine <- openLog
  requests <- newTQueueIO
  events <- B.newBChan eventChannelSize
  visualizing <- newTVarIO Nothing
  let workers = Workers {emit = B.writeBChan events, logLine = logLine, requests = requests}
      restarted worker = forkIO . forever $ do
        r <- try @SomeException worker
        either (logLine . ("A worker failed: " <>) . T.pack . displayException) pure r
        threadDelay retryInterval
  forM_ [idleWorker, commandWorker] $ \worker ->
    restarted . runEff . runMpd settings $ worker workers
  forM_ config.visualizer.dataSource $ \source -> do
    path <- expandHome source
    let FrameRate fps = config.visualizer.fps
    restarted . visualizerWorker $
      VisualizerSource
        { path = path
        , channels = if config.visualizer.inStereo then 2 else 1
        , fps = fps
        , reading = visualizing
        , emit = void . B.writeBChanNonBlocking events
        }
  installWidthTable
  let buildVty = V.mkVty V.defaultConfig
  vty <- buildVty
  void $
    B.customMain
      vty
      buildVty
      (Just events)
      ( app
          AppEnv
            { config = config
            , keymaps = keymapsOf config.keys
            , colorMode = colorMode
            , collator = userCollator
            }
          Channels {requests = requests, events = events, visualizing = visualizing}
      )
      (initialState config)

sources :: Options -> IO Sources
sources opts = do
  envHost <- fmap T.pack <$> lookupEnv "MPD_HOST"
  envPort <- fmap T.pack <$> lookupEnv "MPD_PORT"
  runtimeDir <- lookupEnv "XDG_RUNTIME_DIR"
  let candidates = maybe [] (\d -> [d </> "mpd" </> "socket"]) runtimeDir <> ["/run/mpd/socket"]
  existing <- filterM doesPathExist candidates
  pure
    Sources
      { cliHost = opts.host
      , cliPort = opts.port
      , envHost = envHost
      , envPort = envPort
      , existingSockets = existing
      }

-- | A leading @~/@ is the home directory.
expandHome :: FilePath -> IO FilePath
expandHome path = case L.stripPrefix "~/" path of
  Just rest -> (</> rest) <$> getHomeDirectory
  Nothing -> pure path

-- | Open the log in @$XDG_STATE_HOME/reprise/reprise.log@. Each run appends
-- to it. The UI owns the terminal, so nothing else may print there.
openLog :: IO (T.Text -> IO ())
openLog = do
  dir <- getXdgDirectory XdgState "reprise"
  createDirectoryIfMissing True dir
  h <- openFile (dir </> "reprise.log") AppendMode
  hSetBuffering h LineBuffering
  lock <- newMVar ()
  pure $ \msg -> withMVar lock $ \() -> do
    time <- getZonedTime
    T.hPutStrLn h $ T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z " time) <> msg
