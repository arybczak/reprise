module AddressTests (addressTests) where

import Data.Text qualified as T
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Config
import Reprise.Mpd.Address
import Reprise.Mpd.Protocol.Connection

addressTests :: TestTree
addressTests =
  testGroup
    "Address"
    [ testCase "the command line first" test_commandLine
    , testCase "MPD_HOST with a password" test_envPassword
    , testCase "a socket path" test_socketPath
    , testCase "an abstract socket" test_abstractSocket
    , testCase "the usual socket" test_usualSocket
    , testCase "localhost" test_localhost
    , testCase "a port is from 1 to 65535" test_ports
    , testCase "MPD_PORT that isn't a port" test_badEnvPort
    ]

test_commandLine :: Assertion
test_commandLine = do
  s <-
    settingsOf
      defaultConfig.mpd
      (noSources & #cliHost ?~ "music" & #cliPort ?~ Port 7000 & #envHost ?~ "other")
  assertEqual "address" (TcpAddress "music" 7000) s.address

test_envPassword :: Assertion
test_envPassword = do
  s <-
    settingsOf
      defaultConfig.mpd
      (noSources & #envHost ?~ "secret@music" & #envPort ?~ "6601")
  assertEqual "address" (TcpAddress "music" 6601) s.address
  assertEqual "password" (Just "secret") s.password

test_socketPath :: Assertion
test_socketPath = do
  s <- settingsOf defaultConfig.mpd (noSources & #envHost ?~ "/run/user/1000/mpd/socket")
  assertEqual "address" (UnixAddress "/run/user/1000/mpd/socket") s.address

test_abstractSocket :: Assertion
test_abstractSocket = do
  s <- settingsOf defaultConfig.mpd (noSources & #envHost ?~ "@mpd")
  assertEqual "address" (UnixAddress "\0mpd") s.address
  assertEqual "no password" Nothing s.password

test_usualSocket :: Assertion
test_usualSocket = do
  s <- settingsOf defaultConfig.mpd (noSources & #existingSockets .~ ["/run/mpd/socket"])
  assertEqual "address" (UnixAddress "/run/mpd/socket") s.address

test_localhost :: Assertion
test_localhost = do
  s <- settingsOf defaultConfig.mpd noSources
  assertEqual "address" (TcpAddress "localhost" 6600) s.address
  assertEqual "timeout" (Just 5) s.timeout

test_ports :: Assertion
test_ports = do
  assertEqual "the first" (Right (Port 1)) (parsePort "1")
  assertEqual "the last" (Right (Port 65535)) (parsePort "65535")
  assertEqual
    "past the last"
    (Left "a port is from 1 to 65535, not 65536")
    (parsePort "65536")
  assertEqual "0" (Left "a port is from 1 to 65535, not 0") (parsePort "0")
  assertEqual "a sign" (Left "a port is from 1 to 65535, not +1") (parsePort "+1")
  assertEqual "a space" (Left "a port is from 1 to 65535, not  1") (parsePort " 1")

-- | It is an error only where it would be the port.
test_badEnvPort :: Assertion
test_badEnvPort = do
  assertEqual
    "of a TCP address"
    (Left "MPD_PORT: a port is from 1 to 65535, not 70000")
    (address (noSources & #envPort ?~ "70000"))
  assertEqual
    "of a socket"
    (Right (UnixAddress "/run/mpd/socket"))
    (address (noSources & #envHost ?~ "/run/mpd/socket" & #envPort ?~ "70000"))
  where
    address :: Sources -> Either T.Text Address
    address = fmap (.address) . resolveSettings defaultConfig.mpd

----------------------------------------
-- Helpers

noSources :: Sources
noSources = Sources Nothing Nothing Nothing Nothing []

settingsOf :: MpdConfig -> Sources -> IO Settings
settingsOf config = either (assertFailure . T.unpack) pure . resolveSettings config
