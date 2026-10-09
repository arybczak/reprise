-- | The fifo of MPD's fifo output, which the visualizer's worker reads. The
-- tests replace it with samples that MPD writes at given times.
module Reprise.Effect.Fifo
  ( -- * Effect
    Fifo (..)

    -- ** Handlers
  , runFifo

    -- ** Operations
  , openFifo
  , readFifo
  , closeFifo
  ) where

import Data.ByteString qualified as BS
import Data.IORef.Strict qualified as S
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import System.IO

data Fifo :: Effect where
  OpenFifo :: Fifo m ()
  ReadFifo :: Fifo m BS.ByteString
  CloseFifo :: Fifo m ()

type instance DispatchOf Fifo = Dynamic

-- | Run the effect with the fifo at a path, open once at a time.
runFifo :: IOE :> es => FilePath -> Eff (Fifo : es) a -> Eff es a
runFifo path action = do
  ref <- liftIO $ S.newIORef Nothing
  let close = S.readIORef ref >>= mapM_ hClose >> S.writeIORef ref Nothing
      handled = interpretWith_ action $ \case
        -- A fifo opens at once without a writer, as GHC opens it
        -- non-blocking.
        OpenFifo -> liftIO $ do
          close
          h <- openBinaryFile path ReadMode
          S.writeIORef ref (Just h)
        ReadFifo -> liftIO $ S.readIORef ref >>= maybe (pure BS.empty) readAvailable
        CloseFifo -> liftIO close
  handled `finally` liftIO close

-- | Open the fifo, closing the one open before. Throws an 'IOException'
-- if it can't.
openFifo :: Fifo :> es => Eff es ()
openFifo = send OpenFifo

-- | What the fifo holds, without waiting for more. Nothing without an open
-- fifo.
readFifo :: Fifo :> es => Eff es BS.ByteString
readFifo = send ReadFifo

closeFifo :: Fifo :> es => Eff es ()
closeFifo = send CloseFifo

-- | Read what the fifo holds, without waiting for more.
readAvailable :: Handle -> IO BS.ByteString
readAvailable h = BS.concat <$> go
  where
    go :: IO [BS.ByteString]
    go = do
      chunk <- BS.hGetNonBlocking h pipeCapacity
      if BS.length chunk < pipeCapacity then pure [chunk] else (chunk :) <$> go

-- | The capacity of a pipe on Linux, so that a read usually takes all that
-- the fifo holds.
pipeCapacity :: Int
pipeCapacity = 65536
