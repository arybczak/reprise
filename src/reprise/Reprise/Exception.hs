-- | Handling of the exceptions that an action throws itself.
module Reprise.Exception
  ( catchSync
  ) where

import Control.Exception

-- | Catch an exception that the action throws, but not an asynchronous one,
-- e.g. the cancellation of the thread, which ends it as it should.
catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync action handler =
  action `catch` \e -> case fromException @SomeAsyncException e of
    Just _ -> throwIO e
    Nothing -> handler e
