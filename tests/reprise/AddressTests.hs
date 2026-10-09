module AddressTests (addressTests) where

import Control.Exception
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Network.Socket qualified as N
import Optics.Core
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Config
import Reprise.Effect.Mpd
import Reprise.Mpd.Address
import Reprise.Mpd.Protocol.Connection
import Reprise.Mpd.TestServer

addressTests :: TestTree
addressTests =
  testGroup
    "Address"
    [ testCase "the command line first" test_commandLine
    , testCase "MPD_HOST with a password" test_envPassword
    , testCase "the order of the passwords" test_passwordOrder
    , testCase "an empty variable is unset" test_emptyVariables
    , testCase "the port of another source than the host" test_portOfAnotherSource
    , testCase "a socket path" test_socketPath
    , testCase "a socket in the home directory" test_homeSocket
    , testCase "an abstract socket" test_abstractSocket
    , testCase "the usual socket" test_usualSocket
    , testCase "localhost" test_localhost
    , testCase "a port is from 1 to 65535" test_ports
    , testCase "MPD_PORT that isn't a port" test_badEnvPort
    , testCase "the addresses to try" test_candidates
    , withResource (startTestServer []) stopTestServer $ \getServer ->
        testCase "a socket that nothing listens on" (test_deadSocket getServer)
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

test_passwordOrder :: Assertion
test_passwordOrder = do
  let config = defaultConfig.mpd & #password ?~ "config"
  fromCli <- settingsOf config (noSources & #cliHost ?~ "cli@music")
  assertEqual "the command line's" (Just "cli") fromCli.password
  fromEnv <- settingsOf config (noSources & #envHost ?~ "env@music")
  assertEqual "the config's" (Just "config") fromEnv.password
  configHost <- settingsOf (config & #host ?~ "host@music") noSources
  assertEqual "the config's own" (Just "config") configHost.password

-- | The port comes from the first source with one, whichever gave the host.
test_portOfAnotherSource :: Assertion
test_portOfAnotherSource = do
  s <-
    settingsOf
      (defaultConfig.mpd & #host ?~ "music")
      (noSources & #envHost ?~ "other" & #envPort ?~ "6601")
  assertEqual "address" (TcpAddress "music" 6601) s.address

test_emptyVariables :: Assertion
test_emptyVariables = do
  s <-
    settingsOf
      defaultConfig.mpd
      (noSources & #envHost ?~ "" & #envPort ?~ "" & #existingSockets .~ ["/run/mpd/socket"])
  assertEqual "the usual socket" (UnixAddress "/run/mpd/socket") s.address
  tcp <- settingsOf defaultConfig.mpd (noSources & #envHost ?~ "music" & #envPort ?~ "")
  assertEqual "the default port" (TcpAddress "music" 6600) tcp.address

test_socketPath :: Assertion
test_socketPath = do
  s <- settingsOf defaultConfig.mpd (noSources & #envHost ?~ "/run/user/1000/mpd/socket")
  assertEqual "address" (UnixAddress "/run/user/1000/mpd/socket") s.address

-- | @~/@ is the home directory, from any source of the host, also after a
-- password.
test_homeSocket :: Assertion
test_homeSocket = do
  let config = defaultConfig.mpd & #host ?~ "~/.config/mpd/socket"
  s <- settingsOf config noSources
  assertEqual "address" (UnixAddress "/home/user/.config/mpd/socket") s.address
  fromEnv <- settingsOf defaultConfig.mpd (noSources & #envHost ?~ "secret@~/mpd/socket")
  assertEqual "after a password" (UnixAddress "/home/user/mpd/socket") fromEnv.address
  assertEqual "the password" (Just "secret") fromEnv.password
  tcp <- settingsOf defaultConfig.mpd (noSources & #envHost ?~ "~music")
  assertEqual "a host name" (TcpAddress "~music" 6600) tcp.address

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
    address = fmap ((.address) . NE.head) . resolveSettings defaultConfig.mpd

-- | Without a host, the usual sockets that exist come first, then
-- localhost, as a socket can stay after MPD stopped listening on it. A host
-- that the user names is the only one.
test_candidates :: Assertion
test_candidates = do
  let addresses sources =
        either (assertFailure . T.unpack) (pure . map (.address) . NE.toList) $
          resolveSettings defaultConfig.mpd sources
  assertEqual
    "the usual ones"
    [ UnixAddress "/run/user/1000/mpd/socket"
    , UnixAddress "/run/mpd/socket"
    , TcpAddress "localhost" 6600
    ]
    =<< addresses
      (noSources & #existingSockets .~ ["/run/user/1000/mpd/socket", "/run/mpd/socket"])
  assertEqual "a host" [TcpAddress "music" 6600]
    =<< addresses (noSources & #envHost ?~ "music" & #existingSockets .~ ["/run/mpd/socket"])

-- | A socket that nothing listens on gives way to the next address.
test_deadSocket :: IO TestServer -> Assertion
test_deadSocket getServer = withSystemTempDirectory "mpd" $ \dir -> do
  server <- getServer
  let dead = dir </> "socket"
      settings a = Settings {address = a, password = Nothing, timeout = Just 10}
  -- A socket that was bound and closed, as MPD leaves it when it stops.
  bracket (N.socket N.AF_UNIX N.Stream N.defaultProtocol) N.close $ \sock ->
    N.bind sock (N.SockAddrUnix dead)
  version <-
    runEff
      . runMpd (settings (UnixAddress dead) NE.:| [settings (UnixAddress server.socketPath)])
      $ connectMpd Nothing
  assertBool "connected" (version >= minimumVersion)

----------------------------------------
-- Helpers

noSources :: Sources
noSources = Sources Nothing Nothing Nothing Nothing [] "/home/user"

-- | The settings that are tried first.
settingsOf :: MpdConfig -> Sources -> IO Settings
settingsOf config = either (assertFailure . T.unpack) (pure . NE.head) . resolveSettings config
