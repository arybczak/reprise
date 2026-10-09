module HttpTests (httpTests) where

import Control.Concurrent
import Control.Exception
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
