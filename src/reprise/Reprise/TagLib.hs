{-# LANGUAGE CApiFFI #-}

-- | Reading the tags of a local file with TagLib's C API.
module Reprise.TagLib
  ( FileTags (..)
  , readFileTags
  ) where

import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign
import Foreign.C
import Foreign.C.ConstPtr
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding

-- | What TagLib reads from a file.
data FileTags = FileTags
  { properties :: [(T.Text, [T.Text])]
  -- ^ Each property of TagLib's property map, e.g. @ARTIST@, with its
  -- values.
  , seconds :: Maybe Int
  -- ^ The length, which the C API has in whole seconds only.
  }
  deriving stock (Eq, Show)

-- | The tags of a file, or Nothing if TagLib doesn't know its type or
-- finds neither tags nor audio in it.
readFileTags :: FilePath -> IO (Maybe FileTags)
readFileTags path = do
  encoding <- getFileSystemEncoding
  GHC.withCString encoding path $ \cpath ->
    bracket (c_fileNew cpath) (\file -> unless (file == nullPtr) (c_fileFree file)) $ \file ->
      if file == nullPtr
        then pure Nothing
        else do
          valid <- c_fileIsValid file
          if valid == 0
            then pure Nothing
            else Just <$> fileTags file
  where
    fileTags :: Ptr TagLibFile -> IO FileTags
    fileTags file = do
      properties <- withStrings (c_propertyKeys file) $ \keys ->
        forM keys $ \key -> do
          values <- withStrings (c_propertyGet file key) (traverse peekText)
          (,values) <$> peekText key
      audio <- c_fileAudioProperties file
      seconds <-
        if unConstPtr audio == nullPtr
          then pure Nothing
          else Just . fromIntegral <$> c_audioPropertiesLength audio
      pure FileTags {properties = properties, seconds = seconds}

    -- A NULL-terminated array of strings that TagLib returns, which is
    -- freed after the action.
    withStrings :: IO (Ptr CString) -> ([CString] -> IO a) -> IO a
    withStrings get action =
      bracket get (\ptr -> unless (ptr == nullPtr) (c_propertyFree ptr)) $ \ptr ->
        action =<< if ptr == nullPtr then pure [] else peekArray0 nullPtr ptr

    -- The C API's strings are UTF-8.
    peekText :: CString -> IO T.Text
    peekText = fmap T.decodeUtf8Lenient . BS.packCString

data TagLibFile

data AudioProperties

-- Opening a file reads it, so it is a safe call.
foreign import capi safe "tag_c.h taglib_file_new"
  c_fileNew :: CString -> IO (Ptr TagLibFile)

foreign import capi unsafe "tag_c.h taglib_file_free"
  c_fileFree :: Ptr TagLibFile -> IO ()

foreign import capi unsafe "tag_c.h taglib_file_is_valid"
  c_fileIsValid :: Ptr TagLibFile -> IO CInt

foreign import capi unsafe "tag_c.h taglib_file_audioproperties"
  c_fileAudioProperties :: Ptr TagLibFile -> IO (ConstPtr AudioProperties)

foreign import capi unsafe "tag_c.h taglib_audioproperties_length"
  c_audioPropertiesLength :: ConstPtr AudioProperties -> IO CInt

foreign import capi unsafe "tag_c.h taglib_property_keys"
  c_propertyKeys :: Ptr TagLibFile -> IO (Ptr CString)

foreign import capi unsafe "tag_c.h taglib_property_get"
  c_propertyGet :: Ptr TagLibFile -> CString -> IO (Ptr CString)

foreign import capi unsafe "tag_c.h taglib_property_free"
  c_propertyFree :: Ptr CString -> IO ()
