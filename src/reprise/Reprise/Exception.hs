-- | Handling of the exceptions that an action throws itself, and their text.
module Reprise.Exception
  ( catchSync
  , exceptionText
  ) where

import Control.Exception
import Data.Text qualified as T

-- | Catch an exception that the action throws, but not an asynchronous one,
-- e.g. the cancellation of the thread, which ends it as it should.
catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync action handler =
  action `catch` \e -> case fromException @SomeAsyncException e of
    Just _ -> throwIO e
    Nothing -> handler e

-- | An exception as people read it.
exceptionText :: Exception e => e -> T.Text
exceptionText = T.pack . displayException
