-- | The HTTP requests of the fetchers of lyrics.
module Reprise.Lyrics.Http
  ( Get
  , httpsGet
  , answeredWith
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Client
import Network.HTTP.Types

-- | A GET of a path of a site with the parameters of its query: the status
-- and the body of the reply, or why there is none. The tests replace it.
type Get = BS.ByteString -> [(T.Text, T.Text)] -> IO (Either T.Text (Int, BS.ByteString))

-- | A GET of a site over HTTPS, with a manager that speaks TLS, by the
-- site's name for the messages, its host and the user agent, by which the
-- sites ask clients to name themselves. A redirect isn't followed but
-- answered, as a fetcher knows what it means, e.g. tekstowo.pl redirects a
-- search that finds nothing to a page that bots must not read.
httpsGet :: Manager -> T.Text -> T.Text -> BS.ByteString -> Get
httpsGet manager userAgent site siteHost sitePath params = do
  let request =
        setQueryString [(T.encodeUtf8 k, Just (T.encodeUtf8 v)) | (k, v) <- params] $
          defaultRequest
            { host = siteHost
            , port = 443
            , secure = True
            , path = sitePath
            , requestHeaders = [(hUserAgent, T.encodeUtf8 userAgent)]
            , responseTimeout = responseTimeoutMicro timeout
            , redirectCount = 0
            }
  try (httpLbs request manager) >>= \case
    Right response ->
      pure $ Right (statusCode (responseStatus response), BL.toStrict (responseBody response))
    Left err -> pure . Left $ case err of
      HttpExceptionRequest _ ResponseTimeout -> site <> " didn't answer in time"
      HttpExceptionRequest _ ConnectionTimeout -> site <> " didn't answer in time"
      HttpExceptionRequest _ content -> site <> " can't be reached: " <> T.pack (show content)
      InvalidUrlException url why -> site <> "'s URL " <> T.pack url <> " is invalid: " <> T.pack why

-- | Why a site's reply has no lyrics, by its status, which the fetcher
-- didn't expect.
answeredWith :: T.Text -> Int -> T.Text
answeredWith site status = site <> " answered with the status " <> T.pack (show status)

-- | How long a site has to answer, in microseconds. LRCLIB answered 30
-- requests in 0.86 s at most on 2026-10-05. The worker serves one request
-- at a time, so a request that hangs holds up the next ones, and it gives
-- up after about ten times that.
timeout :: Int
timeout = 10 * 1000000
