-- | The HTTP requests of the fetchers of lyrics.
module Reprise.Lyrics.Http
  ( Get
  , httpsGet
  , getWithin
  , answeredWith
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Client
import Network.HTTP.Types
import System.Timeout

-- | A GET of a path of a site with the parameters of its query: the status
-- and the body of the reply, or why there is none. The tests replace it.
type Get = BS.ByteString -> [(T.Text, T.Text)] -> IO (Either T.Text (Int, BS.ByteString))

-- | A GET of a site over HTTPS, with a manager that speaks TLS, by the
-- site's name for the messages, its host and the user agent, by which the
-- sites ask clients to name themselves.
httpsGet :: Manager -> T.Text -> T.Text -> BS.ByteString -> Get
httpsGet manager userAgent site siteHost =
  getWithin
    requestTimeout
    manager
    userAgent
    site
    defaultRequest {host = siteHost, port = 443, secure = True}

-- | A GET of the site of a request, which gives up after a number of
-- microseconds, with the body. http-client's own timeout ends with the
-- headers, and a body that stalls would hold up the fetcher for good. A
-- redirect isn't followed but answered, as a fetcher knows what it means,
-- e.g. tekstowo.pl redirects a search that finds nothing to a page that
-- bots must not read.
getWithin :: Int -> Manager -> T.Text -> T.Text -> Request -> Get
getWithin limit manager userAgent site base sitePath params = do
  let request =
        setQueryString [(T.encodeUtf8 k, Just (T.encodeUtf8 v)) | (k, v) <- params] $
          base
            { path = sitePath
            , requestHeaders = [(hUserAgent, T.encodeUtf8 userAgent)]
            , responseTimeout = responseTimeoutNone
            , redirectCount = 0
            }
  timeout limit (try (httpLbs request manager)) >>= \case
    Nothing -> pure . Left $ site <> " didn't answer in time"
    Just (Right response) ->
      pure $ Right (statusCode (responseStatus response), BL.toStrict (responseBody response))
    Just (Left err) -> pure . Left $ case err of
      HttpExceptionRequest _ ConnectionTimeout -> site <> " didn't answer in time"
      HttpExceptionRequest _ content -> site <> " can't be reached: " <> T.pack (show content)
      InvalidUrlException url why -> site <> "'s URL " <> T.pack url <> " is invalid: " <> T.pack why

-- | Why a site's reply has no lyrics, by its status, which the fetcher
-- didn't expect.
answeredWith :: T.Text -> Int -> T.Text
answeredWith site status = site <> " answered with the status " <> T.pack (show status)

-- | How long a site has to answer, in microseconds. LRCLIB answered 30
-- requests in 0.86 s at most on 2026-10-05. A request that hangs holds up
-- the fetches in the background after it, so it gives up after about ten
-- times that.
requestTimeout :: Int
requestTimeout = 10 * 1000000
