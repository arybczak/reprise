-- | Where reprise finds MPD.
module Reprise.Mpd.Address
  ( Sources (..)
  , resolveSettings
  ) where

import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import GHC.Generics
import System.FilePath

import Reprise.Config
import Reprise.Mpd.Protocol.Connection

-- | The places that can name the server, in their order of priority after
-- the command line and the config.
data Sources = Sources
  { cliHost :: Maybe T.Text
  , cliPort :: Maybe Port
  , envHost :: Maybe T.Text
  -- ^ @MPD_HOST@, which may start with @password\@@.
  , envPort :: Maybe T.Text
  -- ^ @MPD_PORT@.
  , existingSockets :: [FilePath]
  -- ^ The usual socket locations that exist.
  , home :: FilePath
  -- ^ The home directory, which @~/@ in a socket path stands for, as in
  -- MPD's own config.
  }
  deriving stock (Generic)

-- | MPD's default port.
defaultPort :: Port
defaultPort = Port 6600

-- | The connection settings: the host from the command line, the config or
-- @MPD_HOST@, else the first usual socket that exists, else @localhost@. A
-- TCP host takes the port from the same sources, whichever gave the host,
-- else 6600. Fails if the port from @MPD_PORT@ isn't one.
--
-- A password in the host of the command line comes before the one of the
-- config, which comes before one in the host of the config or @MPD_HOST@.
resolveSettings :: MpdConfig -> Sources -> Either T.Text Settings
resolveSettings config sources = do
  a <- address
  pure
    Settings
      { address = a
      , password =
          listToMaybe . catMaybes $
            if isJust sources.cliHost
              then [hostPassword, config.password]
              else [config.password, hostPassword]
      , timeout = let Timeout (Duration t) = config.timeout in Just t
      }
  where
    (hostPassword, host) = case listToMaybe (catMaybes [sources.cliHost, config.host, envHost]) of
      Just h -> case T.breakOn "@" h of
        -- An abstract socket starts with @, so a password needs text before
        -- the first @.
        (before, rest)
          | not (T.null before) && not (T.null rest) -> (Just before, Just (T.drop 1 rest))
        _ -> (Nothing, Just h)
      Nothing -> (Nothing, Nothing)

    port :: Either T.Text Port
    port = case listToMaybe (catMaybes [sources.cliPort, config.port]) of
      Just p -> Right p
      Nothing -> case envPort of
        Just t -> either (Left . ("MPD_PORT: " <>)) Right (parsePort t)
        Nothing -> Right defaultPort

    -- An empty variable is unset, as an empty @NO_COLOR@ or @EDITOR@ is, so
    -- that @MPD_HOST= reprise@ leaves it out.
    envHost :: Maybe T.Text
    envHost = mfilter (not . T.null) sources.envHost

    envPort :: Maybe T.Text
    envPort = mfilter (not . T.null) sources.envPort

    address :: Either T.Text Address
    address = case host of
      Just h
        | "/" `T.isPrefixOf` h -> Right $ UnixAddress (T.unpack h)
        | Just path <- T.stripPrefix "~/" h ->
            Right $ UnixAddress (sources.home </> T.unpack path)
        | Just name <- T.stripPrefix "@" h -> Right $ UnixAddress ('\0' : T.unpack name)
        | otherwise -> tcp (T.unpack h)
      Nothing -> case sources.existingSockets of
        socket : _ -> Right $ UnixAddress socket
        [] -> tcp "localhost"

    tcp :: String -> Either T.Text Address
    tcp h = (\(Port p) -> TcpAddress h p) <$> port
