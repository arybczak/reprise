-- | Handling of the exceptions that an action throws itself, and their text.
module Reprise.Exception
  ( catchSync
  , exceptionText
  , Signalled (..)
  ) where

import Control.Exception
import Data.Text qualified as T
import Foreign.C.Types

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

-- | A signal that ends reprise, by its number. It is asynchronous, so that
-- 'catchSync' doesn't take it for a failure, e.g. of the editor that it
-- interrupts.
newtype Signalled = Signalled CInt
  deriving stock (Eq, Show)

instance Exception Signalled where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException
