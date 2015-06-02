{-|
  Module      : Data.Blob
  Description : Library for reading and writing large binary values to disk
  Stability   : Experimental
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

import qualified Crypto.Hash.SHA512       as SHA512
import qualified Data.Blob.FileOperations as F
import           Data.Blob.GC             (markAsAccessible)
import           Data.Blob.Types

-- | Returns a BlobStore.
-- This method ensures that the base directory exists
-- and contains tmp and curr subdirectories.
openBlobStore :: FilePath -> IO BlobStore
openBlobStore dir = do
  F.createTempIfMissing dir
  F.createCurrIfMissing dir
  return $ BlobStore dir

-- | Creates an empty blob in the given BlobStore.
-- Use 'writePartial' to write contents to the newly
-- created blob.
newBlob :: BlobStore -> IO WriteContext
newBlob (BlobStore dir) = do
  filename <- F.createUniqueFile dir
  let temploc = TempLocation dir filename
  h <- F.openFileForWrite $ F.getTempPath temploc
  return $ WriteContext temploc h SHA512.init

-- | 'writePartial' appends the given blob to the
-- blob referenced by the 'WriteContext'.
writePartial :: WriteContext -> Blob -> IO WriteContext
writePartial (WriteContext l h ctx) (Blob b) = do
  F.writeToHandle h b
  let newctx = SHA512.update ctx b
  return $ WriteContext l h newctx

-- | Finalize the write to the given blob.
-- After calling 'endWrite' no more updates are possible
-- on the blob.
endWrite :: WriteContext -> IO BlobId
endWrite (WriteContext l h ctx) = do
  F.syncAndClose h
  markAsAccessible blobId
  F.moveFile (F.getTempPath l) (baseDir l) newfilename
  return blobId
  where
    newfilename = "sha512-" ++ F.toFileName (SHA512.finalize ctx)
    blobId = BlobId (baseDir l) newfilename

-- | Create a blob from the given contents.
-- Use 'createBlob' only for small contents.
-- For large contents, use the partial write interface
-- ('newBlob' followed by calls to 'writePartial').
createBlob :: BlobStore -> Blob -> IO BlobId
createBlob blobstore blob = newBlob blobstore
  >>= \wc -> writePartial wc blob
  >>= endWrite

-- | Open blob for reading
startRead :: BlobId -> IO ReadContext
startRead loc = fmap ReadContext $ F.openFileForRead (F.getCurrPath loc)

-- | Read given number of bytes from the blob handle
readPartial :: ReadContext -> Int -> IO Blob
readPartial (ReadContext h) sz = fmap Blob $ F.readFromHandle h sz

-- | Skip given number of bytes ahead in the blob
skipBytes :: ReadContext -> Integer -> IO ()
skipBytes (ReadContext h) = F.seekHandle h

-- | Complete reading from a file
endRead :: ReadContext -> IO ()
endRead (ReadContext h) = F.closeHandle h

-- | Read an entire blob
-- Use 'readBlob' only for small blobs.
-- For large blobs, use 'readPartial' instead.
readBlob :: BlobId -> IO Blob
readBlob blobid = fmap Blob $ F.readFile (F.getCurrPath blobid)

-- | Delete the file corresponding to the blob id
deleteBlob :: BlobId -> IO ()
deleteBlob = F.deleteFile . F.getCurrPath
