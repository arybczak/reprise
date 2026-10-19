module Reprise.Test.Lyrics.Http (httpTests) where

import Control.Concurrent
import Control.Exception
import Data.Text qualified as T
import Network.HTTP.Client
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Reprise.Lyrics.Http

httpTests :: TestTree
httpTests =
  testGroup
    "HTTP"
    [ testCase "a body that stalls runs out of time" test_stalledBody
    , testCase "a body cut off by a reset" test_resetBody
    ]

-- | A server that sends the headers and part of the body, then nothing.
test_stalledBody :: Assertion
test_stalledBody =
  bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \listener -> do
    N.bind listener (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
    N.listen listener 1
    p <- N.socketPort listener
    let serve = do
          (sock, _) <- N.accept listener
          flip finally (N.close sock) $ do
            _ <- N.recv sock 4096
            N.sendAll sock "HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\npart of it"
            threadDelay maxBound
    bracket (forkIO serve) killThread $ \_ -> do
      manager <- newManager defaultManagerSettings
      let base = defaultRequest {host = "127.0.0.1", port = fromIntegral p}
      r <- timeout (5 * 1000000) $ getWithin 200000 manager "reprise" "The site" base "/" []
      assertEqual "the reply" (Just (Left "The site didn't answer in time")) r

-- | A server that sends the headers and part of the body, then resets the
-- connection. The failure is one that a fetcher reports, so that the next
-- fetcher is asked.
test_resetBody :: Assertion
test_resetBody =
  bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \listener -> do
    N.bind listener (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
    N.listen listener 1
    p <- N.socketPort listener
    let serve = do
          (sock, _) <- N.accept listener
          flip finally (N.close sock) $ do
            _ <- N.recv sock 4096
            N.sendAll sock "HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\npart of it"
            N.setSockOpt sock N.Linger (N.StructLinger 1 0)
    bracket (forkIO serve) killThread $ \_ -> do
      manager <- newManager defaultManagerSettings
      let base = defaultRequest {host = "127.0.0.1", port = fromIntegral p}
      r <-
        timeout (5 * 1000000) . try @SomeException $
          getWithin 5000000 manager "reprise" "The site" base "/" []
      case r of
        Just (Right (Left why)) ->
          assertBool ("why: " <> T.unpack why) ("The site can't be reached" `T.isPrefixOf` why)
        other -> assertFailure $ "not a failure of the site: " <> show other
