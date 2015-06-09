{-|
  Module      : Data.Blob
  Stability   : Experimental
  Portability : non-portable (requires POSIX)

  This module provides interface for handling large objects -
  also known as blobs.

  One of the use cases for bloc is storing large objects in databases.
  Instead of storing the entire blob in the database, you can just store
  the 'BlobId' of the blob.

  Multiple values in the database can share a 'BlobId' and we provide an
  interface for garbage collection for such cases.
-}

module Data.Blob ( Blob (..)
                 , BlobId
                 , BlobStore
                 , WriteContext
                 , ReadContext
                 , openBlobStore
                 , newBlob
                 , writePartial
                 , endWrite
                 , createBlob
                 , startRead
                 , readPartial
                 , skipBytes
                 , endRead
                 , readBlob
                 , deleteBlob
                 ) where

import           Control.Monad            ((<=<))
import qualified Crypto.Hash.SHA512       as SHA512
import           Data.Blob.Directories
import qualified Data.Blob.FileOperations as F
import           Data.Blob.Types
import           System.Directory         (renameFile)

-- | All the blobs of an application are stored in the
-- same directory. 'openBlobStore' returns the 'BlobStore'
-- corresponding to a given directory.
openBlobStore :: FilePath -> IO BlobStore
openBlobStore dir = do
  mapM_ F.createDir [dir, tempDir dir, currDir dir]
  F.createFileInDir (currDir dir) metadataFile
  F.recoverFromGC dir
  return $ BlobStore dir

-- | Creates an empty blob in the given BlobStore.
--
-- Use 'writePartial' to write contents to the newly
-- created blob.
newBlob :: BlobStore -> IO WriteContext
newBlob (BlobStore dir) = do
  filename <- F.createUniqueFile dir
  let temploc = TempLocation dir filename
  h <- F.openFileForWrite $ getFullPath tempDirName temploc
  return $ WriteContext temploc h SHA512.init

-- | 'writePartial' appends the given blob to the
-- blob referenced by the 'WriteContext'.
writePartial :: WriteContext -> Blob -> IO WriteContext
writePartial (WriteContext l h ctx) (Blob b) = do
  F.writeToHandle h b
  let newctx = SHA512.update ctx b
  return $ WriteContext l h newctx

-- | Finalize the write to the given blob.
--
-- After calling 'endWrite' no more updates are possible
-- on the blob.
endWrite :: WriteContext -> IO BlobId
endWrite (WriteContext l h ctx) = do
  F.syncAndClose h
  renameFile (getFullPath tempDirName l) (getFullPath currDirName blobId)
  F.syncDir (currDir (baseDir l))
  return blobId
  where
    newfilename = "sha512-" ++ F.toFileName (SHA512.finalize ctx)
    blobId = BlobId (baseDir l) newfilename

-- | Create a blob from the given contents.
--
-- Use 'createBlob' only for small contents.
-- For large contents, use the partial write interface
-- ('newBlob' followed by calls to 'writePartial').
createBlob :: BlobStore -> Blob -> IO BlobId
createBlob blobstore blob = newBlob blobstore
  >>= \wc -> writePartial wc blob
  >>= endWrite

-- | Open blob for reading.
startRead :: BlobId -> IO ReadContext
startRead = fmap ReadContext . F.getHandle

-- | Read given number of bytes from the blob.
readPartial :: ReadContext -> Int -> IO Blob
readPartial (ReadContext h) sz = fmap Blob $ F.readFromHandle h sz

-- | Skip given number of bytes ahead in the blob.
skipBytes :: ReadContext -> Integer -> IO ()
skipBytes (ReadContext h) = F.seekHandle h

-- | Complete reading from a blob.
endRead :: ReadContext -> IO ()
endRead (ReadContext h) = F.closeHandle h

-- | 'readBlob' reads an entire blob.
--
-- Use 'readBlob' only for small blobs.
-- For large blobs, use 'readPartial' instead.
readBlob :: BlobId -> IO Blob
readBlob = fmap Blob . F.readAllFromHandle <=< F.getHandle

-- | Deletes the given blob.
--
-- Use 'deleteBlob' only when you are sure that the given blob
-- is not accessible by anyone. If the blob is shared, you should
-- use the GC interface instead.
deleteBlob :: BlobId -> IO ()
deleteBlob = F.deleteFile . getFullPath currDirName
