{-|
  Module      : Data.Blob.GC
  Stability   : Experimental
  Portability : non-portable (requires POSIX)

  This module contains methods related to the garbage collection
  of the deleted blobs.
-}

module Data.Blob.GC where

import           Control.Monad            (void, when)
import           Data.Blob.FileOperations
import           Data.Blob.Types
import           System.Directory
import           System.FilePath.Posix    ((</>))
import           System.IO.Error          (tryIOError)

-- | Initialize garbage collection for blobs in a given BlobStore.
--
-- startGC throws an error if another GC is already running on
-- the same BlobStore.
startGC :: BlobStore -> IO ()
startGC (BlobStore dir) = do
  createDirectory gcDir
  forAllInDirectory currDir (createFileInDir gcDir)
  where
    gcDir   = dir </> gcDirName
    currDir = dir </> currDirName

-- | Mark a blob as accessible during a GC.
--
-- You have to ensure that all the blobs which are accessible
-- are marked by 'markAsAccessible' before calling 'endGC'.
--
-- Blobs which are created after 'startGC' had finished need not
-- be marked.
markAsAccessible :: BlobId -> IO ()
markAsAccessible = void . tryIOError . deleteFile . getGCPath

-- | Stops the garbage collection.
--
-- This will __delete all the blobs which have not been marked__
-- by 'markAsAccessible' and created before 'startGC' had finished.
endGC :: BlobStore -> IO ()
endGC (BlobStore dir) = do
  checkgcDir <- doesDirectoryExist gcDir
  when checkgcDir $ do
    forAllInDirectory gcDir (deleteFileInDir currDir)
    removeDirectoryRecursive gcDir
  where
    gcDir   = dir </> gcDirName
    currDir = dir </> currDirName

-- | 'markAccessibleBlobs' takes a list of 'BlobId' which are still
-- accessible. 'markAccessibleBlobs' deletes the remaining blobs.
--
-- It is safer to use this method instead of using 'startGC' and 'endGC',
-- since if you forget to mark all the accessible blobs using
-- 'markAsAccessible', 'endGC' might end up deleting accessible blobs.
markAccessibleBlobs :: BlobStore -> [BlobId] -> IO ()
markAccessibleBlobs blobstore blobids = do
  startGC blobstore
  mapM_ markAsAccessible blobids
  endGC blobstore
