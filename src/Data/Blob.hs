{-|
  Module      : Data.Blob
  Description : Library for reading and writing large binary values to disk
  Stability   : Experimental
-}

module Data.Blob where

import           Data.Blob.FileOperations
import           Data.Blob.Types

-- | Write a blob to a unique file
write :: Blob -> IO BlobId
write (Blob b) = do
  (filename, handle) <- createUniqueFile "blob"
  writeToHandle handle b
  return filename

-- | Read a blob, given a unique blob id
read :: BlobId -> IO Blob
read blobid = fmap Blob $ readFromFile blobid

-- | Delete the file corresponding to the blob id
delete :: BlobId -> IO ()
delete = deleteFile
