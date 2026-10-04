-- | Where reprise finds MPD.
module Reprise.Mpd.Address
  ( Sources (..)
  , resolveSettings
  , defaultPort
  ) where

import Data.Maybe
import Data.Text qualified as T
import GHC.Generics
import Network.Socket qualified as N

import Reprise.Config
import Reprise.Mpd.Protocol.Connection

-- | The places that can name the server, in their order of priority after
-- the command line and the config.
data Sources = Sources
  { cliHost :: Maybe T.Text
  , cliPort :: Maybe Int
  , envHost :: Maybe T.Text
  -- ^ @MPD_HOST@, which may start with @password\@@.
  , envPort :: Maybe T.Text
  -- ^ @MPD_PORT@.
  , existingSockets :: [FilePath]
  -- ^ The usual socket locations that exist.
  }
  deriving stock (Generic)

-- | MPD's default port.
defaultPort :: Int
defaultPort = 6600

-- | The connection settings: the host from the command line, the config or
-- @MPD_HOST@, else the first usual socket that exists, else
-- @localhost:6600@.
resolveSettings :: MpdConfig -> Sources -> Settings
resolveSettings config sources =
  Settings
    { address = address
    , password = listToMaybe (catMaybes [config.password, hostPassword])
    , timeout = let Duration t = config.timeout in Just t
    }
  where
    (hostPassword, host) = case listToMaybe (catMaybes [sources.cliHost, config.host, sources.envHost]) of
      Just h -> case T.breakOn "@" h of
        -- An abstract socket starts with @, so a password needs text before
        -- the first @.
        (before, rest)
          | not (T.null before) && not (T.null rest) -> (Just before, Just (T.drop 1 rest))
        _ -> (Nothing, Just h)
      Nothing -> (Nothing, Nothing)

    port :: Int
    port =
      fromMaybe defaultPort . listToMaybe $
        catMaybes [sources.cliPort, config.port, sources.envPort >>= readPort]

    address :: Address
    address = case host of
      Just h
        | "/" `T.isPrefixOf` h -> UnixAddress (T.unpack h)
        | Just name <- T.stripPrefix "@" h -> UnixAddress ('\0' : T.unpack name)
        | otherwise -> TcpAddress (T.unpack h) (fromIntegral port)
      Nothing -> case sources.existingSockets of
        socket : _ -> UnixAddress socket
        [] -> TcpAddress "localhost" (fromIntegral port)

    readPort :: T.Text -> Maybe Int
    readPort t = case reads (T.unpack t) of
      [(p, "")] | p > 0 && p <= toInteger (maxBound @N.PortNumber) -> Just (fromInteger p)
      _ -> Nothing
