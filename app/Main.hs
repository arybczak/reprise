-- | The entry point of reprise.
module Main
  ( main
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict qualified as S
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Version
import Effectful
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix qualified as V
import Network.HTTP.Client.TLS
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
import Reprise.Exception
import Reprise.History
import Reprise.Lyrics.Http
import Reprise.Lyrics.Lrclib
import Reprise.Lyrics.Tekstowo
import Reprise.Lyrics.Worker
import Reprise.Mpd.Address
import Reprise.Mpd.Protocol.Connection
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
  historyFile <- getXdgDirectory XdgState ("reprise" </> "history")
  savedHistory <-
    readHistoryFile historyFile `catchSync` \e -> do
      logLine $ "The history of the prompts can't be read: " <> T.pack (displayException e)
      pure []
  requests <- newTQueueIO
  password <- newTVarIO settings.password
  events <- newTBQueueIO eventQueueSize
  visualizing <- newTVarIO Nothing
  let emit = atomically . writeTBQueue events
      workers =
        Workers
          { emit = emit
          , logLine = logLine
          , requests = requests
          , password = password
          }
      mpdWorkers =
        [ runEff . runMpd settings (readTVarIO password) $ worker workers
        | worker <- [idleWorker, commandWorker]
        ]
  visualizer <- forM config.visualizer.dataSource $ \source -> do
    path <- expandHome source
    let FrameRate fps = config.visualizer.fps
    pure . visualizerWorker $
      VisualizerSource
        { path = path
        , fps = fps
        , reading = visualizing
        , emit = \e -> atomically $ do
            full <- isFullTBQueue events
            unless full $ writeTBQueue events e
            pure (not full)
        , debug = config.visualizer.debug
        }
  lyrics <- newTVarIO Nothing
  lyricsInBackground <- newTVarIO Nothing
  editor <- case config.editor.command of
    Just configured -> pure (Just configured)
    Nothing -> do
      let set name = mfilter (not . null) <$> lookupEnv name
      visual <- set "VISUAL"
      fmap T.pack . (visual <|>) <$> set "EDITOR"
  lyricsDirectory <-
    maybe
      (getXdgDirectory XdgData ("reprise" </> "lyrics"))
      expandHome
      config.lyrics.directory
  manager <- newTlsManager
  let userAgent = "reprise/" <> T.pack (showVersion Paths.version) <> " (" <> repository <> ")"
      fetcher = \case
        Lrclib -> lrclib (httpsGet manager userAgent "LRCLIB" "lrclib.net")
        Tekstowo -> tekstowo (httpsGet manager userAgent "tekstowo.pl" "www.tekstowo.pl")
      lyricsFetcher =
        lyricsWorker
          LyricsSource
            { directory = lyricsDirectory
            , fetchers = map fetcher config.lyrics.fetchers
            , requested = lyrics
            , background = lyricsInBackground
            , emit = emit
            , logLine = logLine
            }
      restarted :: IO () -> IO ()
      restarted worker = forever $ do
        worker `catchSync` \e -> logLine $ "A worker failed: " <> T.pack (displayException e)
        threadDelay retryInterval
  installWidthTable
  withAsync
    (mapConcurrently_ restarted (mpdWorkers <> toList visualizer <> [lyricsFetcher]))
    $ \_ ->
      runApp
        AppEnv
          { config = config
          , keymaps = keymapsOf config.keys
          , colorMode = colorMode
          , collator = userCollator
          , lyricsDirectory = lyricsDirectory
          , editor = editor
          }
        Channels
          { requests = requests
          , events = events
          , visualizing = visualizing
          , lyrics = lyrics
          , lyricsInBackground = lyricsInBackground
          , saveToHistoryFile = \line ->
              saveToHistoryFile historyFile line `catchSync` \e ->
                logLine $ "The history of the prompts can't be saved: " <> T.pack (displayException e)
          }
        (V.mkVty V.defaultConfig)
        (initialState config) {history = savedHistory}

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
  lock <- S.newMVar ()
  pure $ \msg -> S.withMVar lock $ \() -> do
    time <- getZonedTime
    T.hPutStrLn h $ T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z " time) <> msg

-- | Where reprise lives, for the user agent of its requests.
repository :: T.Text
repository = "https://github.com/arybczak/reprise"
