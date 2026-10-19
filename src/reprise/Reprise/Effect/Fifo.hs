-- | The fifo of MPD's fifo output, which the visualizer's worker reads. The
-- tests replace it with samples that MPD writes at given times.
module Reprise.Effect.Fifo
  ( -- * Effect
    Fifo (..)
  , Waited (..)

    -- ** Handlers
  , runFifo

    -- ** Operations
  , openFifo
  , readFifo
  , waitFifo
  , closeFifo
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString qualified as BS
import Data.IORef.Strict qualified as S
import Data.Maybe
import Data.Word
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Foreign.C.Error
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.IO.Exception qualified as GHC
import System.IO.Error
import System.Posix.Files
import System.Posix.IO
import System.Posix.IO.ByteString qualified as B
import System.Posix.Types

data Fifo :: Effect where
  OpenFifo :: Fifo m ()
  ReadFifo :: Fifo m BS.ByteString
  WaitFifo :: STM () -> Fifo m Waited
  CloseFifo :: Fifo m ()

type instance DispatchOf Fifo = Dynamic

-- | How a wait for the samples ended.
data Waited
  = -- | The fifo holds samples.
    SamplesCame
  | -- | What the wait waited for besides the samples happened.
    Cancelled
  | -- | The writer closed the fifo, e.g. MPD quit.
    WriterGone
  deriving stock (Eq, Show)

-- | The fifo that is open, and the samples that a wait read from it.
data Open = Open Fd BS.ByteString

-- | Run the effect with the fifo at a path, open once at a time.
runFifo :: IOE :> es => FilePath -> Eff (Fifo : es) a -> Eff es a
runFifo path action = do
  ref <- liftIO $ S.newIORef Nothing
  buffer <- liftIO $ mallocForeignPtrBytes pipeCapacity
  let close = S.readIORef ref >>= mapM_ (\(Open fd _) -> closeFd fd) >> S.writeIORef ref Nothing
      handled = interpretWith_ action $ \case
        -- A fifo opens at once without a writer, as it opens non-blocking.
        -- Another file, e.g. an audio file named by mistake, would be read
        -- whole on every frame.
        OpenFifo -> liftIO $ do
          close
          fifo <- isNamedPipe <$> getFileStatus path
          unless fifo . ioError $
            mkIOError GHC.InappropriateType "" Nothing (Just path) `ioeSetErrorString` "not a fifo"
          fd <- openFd path ReadOnly defaultFileFlags {nonBlock = True}
          S.writeIORef ref (Just (Open fd BS.empty))
        ReadFifo ->
          liftIO $
            S.readIORef ref >>= \case
              Nothing -> pure BS.empty
              Just (Open fd came) -> do
                S.writeIORef ref (Just (Open fd BS.empty))
                (came <>) . fromMaybe BS.empty <$> readAvailable buffer fd
        WaitFifo cancel ->
          liftIO (S.readIORef ref) >>= \case
            Nothing -> liftIO $ Cancelled <$ atomically cancel
            Just (Open fd came)
              | BS.null came -> waitReadable buffer ref cancel fd
              | otherwise -> pure SamplesCame
        CloseFifo -> liftIO close
  handled `finally` liftIO close
  where
    -- The samples that the wait reads stay for the next read.
    waitReadable
      :: IOE :> es
      => ForeignPtr Word8
      -> S.IORef (Maybe Open)
      -> STM ()
      -> Fd
      -> Eff es Waited
    waitReadable buffer ref cancel fd = do
      (ready, unregister) <- liftIO $ threadWaitReadSTM fd
      readable <-
        liftIO (atomically ((True <$ ready) `orElse` (False <$ cancel)))
          `finally` liftIO unregister
      if not readable
        then pure Cancelled
        else
          liftIO (readAvailable buffer fd) >>= \case
            Nothing -> pure WriterGone
            Just new
              | BS.null new -> waitReadable buffer ref cancel fd
              | otherwise -> SamplesCame <$ liftIO (S.writeIORef ref (Just (Open fd new)))

-- | Open the fifo, closing the one open before. Throws an 'IOException'
-- if it can't.
openFifo :: Fifo :> es => Eff es ()
openFifo = send OpenFifo

-- | What the fifo holds, without waiting for more. Nothing without an open
-- fifo, or once its writer closed it.
readFifo :: Fifo :> es => Eff es BS.ByteString
readFifo = send ReadFifo

-- | Wait until the fifo holds samples, or until an 'STM' action doesn't
-- retry. The samples stay for 'readFifo'. Without an open fifo, it waits
-- for the action. It needs the threaded runtime.
--
-- On Linux, a fifo that is opened anew doesn't count as readable until a
-- writer comes and goes, so once the writer went, a wait on the fifo
-- opened again waits for the next writer.
waitFifo :: Fifo :> es => STM () -> Eff es Waited
waitFifo = send . WaitFifo

closeFifo :: Fifo :> es => Eff es ()
closeFifo = send CloseFifo

-- | Read what the fifo holds, without waiting for more. 'Nothing' if the
-- writer closed it and it holds nothing.
--
-- The reads go to one buffer of 'pipeCapacity', and only what they read is
-- copied out. A new buffer for each read was 64 KiB on every frame, also
-- for a read of nothing: 5% of what the spectrum allocated at 90 frames a
-- second, and 1% of what the ellipse did (measured with +RTS -s).
readAvailable :: ForeignPtr Word8 -> Fd -> IO (Maybe BS.ByteString)
readAvailable buffer fd = go []
  where
    go :: [BS.ByteString] -> IO (Maybe BS.ByteString)
    go chunks =
      tryIOError (withForeignPtr buffer readChunk) >>= \case
        Right chunk
          -- A read of nothing is the end, as the writer closed the fifo.
          | BS.null chunk -> pure $ if null chunks then Nothing else Just (joined chunks)
          | BS.length chunk < pipeCapacity -> pure . Just $ joined (chunk : chunks)
          | otherwise -> go (chunk : chunks)
        Left err
          | wouldBlock err -> pure . Just $ joined chunks
          | otherwise -> ioError err

    readChunk :: Ptr Word8 -> IO BS.ByteString
    readChunk p = do
      n <- B.fdReadBuf fd p (fromIntegral pipeCapacity)
      BS.packCStringLen (castPtr p, fromIntegral n)

    joined :: [BS.ByteString] -> BS.ByteString
    joined = BS.concat . reverse

    wouldBlock :: IOException -> Bool
    wouldBlock err = (Errno <$> GHC.ioe_errno err) `elem` [Just eAGAIN, Just eWOULDBLOCK]

-- | The capacity of a pipe on Linux, so that a read usually takes all that
-- the fifo holds.
pipeCapacity :: Int
pipeCapacity = 65536
