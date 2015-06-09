{-|
  Module      : Data.Blob.GC
  Stability   : Experimental
  Portability : non-portable (requires POSIX)

  This module contains methods related to the garbage collection
  of the deleted blobs.
-}

module Data.Blob.GC where

import           Control.Concurrent       (threadDelay)
import           Control.Monad            (when)
import           Data.Blob.Directories
import           Data.Blob.FileOperations
import           Data.Blob.Types
import           System.Directory

-- | Initialize garbage collection for blobs in a given BlobStore.
--
-- startGC throws an error if another GC is already running on
-- the same BlobStore.
startGC :: BlobStore -> IO ()
startGC (BlobStore dir) = do
  renameDirectory (currDir dir) (gcDir dir)
  createDir (currDir dir)
  createFileInDir (currDir dir) metadataFile

-- | Mark a blob as accessible during a GC.
--
-- You have to ensure that all the blobs which are accessible
-- are marked by 'markAsAccessible' before calling 'endGC'.
--
-- Blobs which are created after 'startGC' had finished need not
-- be marked.
markAsAccessible :: BlobId -> IO ()
markAsAccessible loc = moveFile (getFullPath gcDirName loc) (getFullPath currDirName loc)

-- | Stops the garbage collection.
--
-- This will __delete all the blobs which have not been marked__
-- by 'markAsAccessible' and created before 'startGC' had finished.
endGC :: BlobStore -> Int -> IO ()
endGC (BlobStore dir) delay = do
  checkgcDir <- doesDirectoryExist (gcDir dir)
  when checkgcDir $ do
    syncDir (currDir dir)
    threadDelay delay
    removeDirectoryRecursive (gcDir dir)

-- | 'markAccessibleBlobs' takes a list of 'BlobId' which are still
-- accessible. 'markAccessibleBlobs' deletes the remaining blobs.
--
-- It is safer to use this method instead of using 'startGC' and 'endGC',
-- since if you forget to mark all the accessible blobs using
-- 'markAsAccessible', 'endGC' might end up deleting accessible blobs.
markAccessibleBlobs :: BlobStore -> Int -> [BlobId] -> IO ()
markAccessibleBlobs blobstore delay blobids = do
  startGC blobstore
  mapM_ markAsAccessible blobids
  endGC blobstore delay
