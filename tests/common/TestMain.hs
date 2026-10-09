-- | The main of the test suites.
module TestMain (testMain) where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Environment
import Test.Tasty

-- | Run the tests as 'defaultMain' does, on a thread for each capability of
-- the RTS, unless @-j@ or @TASTY_NUM_THREADS@ sets their number. tasty
-- would take a thread for each core, beyond the capabilities.
testMain :: TestTree -> IO ()
testMain tree = do
  set <- lookupEnv "TASTY_NUM_THREADS"
  when (isNothing set) $ setEnv "TASTY_NUM_THREADS" . show =<< getNumCapabilities
  defaultMain tree
