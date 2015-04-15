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

-- | Open file for writing
initWrite :: BlobId -> IO BlobHandle
initWrite = F.openFileForWrite

-- | Write part of blob to a given blob id
writePartial :: BlobHandle -> Blob -> IO ()
writePartial h (Blob b) = F.writeToHandle h b

-- | Finalize write
finalizeWrite :: BlobHandle -> IO ()
finalizeWrite = F.closeHandle

-- | Open file for reading
initRead :: BlobId -> IO BlobHandle
initRead = F.openFileForRead

-- | Read given number of bytes from the blob handle
readPartial :: BlobHandle -> Int -> IO Blob
readPartial h sz = fmap Blob $ F.readFromHandle h sz

-- | Complete reading from a file
finalizeRead :: BlobHandle -> IO ()
finalizeRead = F.closeHandle

-- | Delete the file corresponding to the blob id
deleteBlob :: BlobId -> IO ()
deleteBlob = F.deleteFile
