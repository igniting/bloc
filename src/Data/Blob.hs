{-|
  Module      : Data.Blob
  Description : Library for reading and writing large binary values to disk
  Stability   : Experimental
-}

module Data.Blob where

import qualified Data.Blob.FileOperations as F
import           Data.Blob.Types

-- | Create file for storing the blob
createBlob :: FilePath -> IO BlobId
createBlob = F.createUniqueFile

-- | Write part of blob to a given blob id
writePartial :: BlobId -> Blob -> IO ()
writePartial blobid (Blob b) = F.appendFile blobid b

-- | Read a blob, given a unique blob id
readBlob :: BlobId -> IO Blob
readBlob blobid = fmap Blob $ F.readFromFile blobid

-- | Delete the file corresponding to the blob id
deleteBlob :: BlobId -> IO ()
deleteBlob = F.deleteFile
