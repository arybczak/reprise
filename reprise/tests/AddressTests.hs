module AddressTests (addressTests) where

import MPD.Connection
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Config
import Reprise.Mpd.Address

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
    ]

test_commandLine :: Assertion
test_commandLine = do
  let s =
        resolveSettings
          defaultConfig.mpd
          (noSources & #cliHost ?~ "music" & #cliPort ?~ 7000 & #envHost ?~ "other")
  assertEqual "address" (TcpAddress "music" 7000) s.address

test_envPassword :: Assertion
test_envPassword = do
  let s =
        resolveSettings
          defaultConfig.mpd
          (noSources & #envHost ?~ "secret@music" & #envPort ?~ "6601")
  assertEqual "address" (TcpAddress "music" 6601) s.address
  assertEqual "password" (Just "secret") s.password

test_socketPath :: Assertion
test_socketPath = do
  let s = resolveSettings defaultConfig.mpd (noSources & #envHost ?~ "/run/user/1000/mpd/socket")
  assertEqual "address" (UnixAddress "/run/user/1000/mpd/socket") s.address

test_abstractSocket :: Assertion
test_abstractSocket = do
  let s = resolveSettings defaultConfig.mpd (noSources & #envHost ?~ "@mpd")
  assertEqual "address" (UnixAddress "\0mpd") s.address
  assertEqual "no password" Nothing s.password

test_usualSocket :: Assertion
test_usualSocket = do
  let s = resolveSettings defaultConfig.mpd (noSources & #existingSockets .~ ["/run/mpd/socket"])
  assertEqual "address" (UnixAddress "/run/mpd/socket") s.address

test_localhost :: Assertion
test_localhost = do
  let s = resolveSettings defaultConfig.mpd noSources
  assertEqual "address" (TcpAddress "localhost" 6600) s.address
  assertEqual "timeout" (Just 5) s.timeout

----------------------------------------
-- Helpers

noSources :: Sources
noSources = Sources Nothing Nothing Nothing Nothing []
