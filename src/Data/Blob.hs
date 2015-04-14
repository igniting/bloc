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

-- | Open file for reading
initRead :: BlobId -> IO BlobHandle
initRead = F.openFile

-- | Read given number of bytes from the blob handle
readBlobPartial :: BlobHandle -> Int -> IO Blob
readBlobPartial h sz = fmap Blob $ F.readFromHandle h sz

-- | Complete reading from a file
finalizeRead :: BlobHandle -> IO ()
finalizeRead = F.closeHandle

-- | Delete the file corresponding to the blob id
deleteBlob :: BlobId -> IO ()
deleteBlob = F.deleteFile
