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
-- We create an empty file in the gc directory corresponding to
-- each blob in the curr directory.
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
-- This deletes the corresponding file of blob from gc directory.
markAsAccessible :: BlobId -> IO ()
markAsAccessible = void . tryIOError . deleteFile . getGCPath

-- | Stops the garbage collection.
--
-- This deletes the blobs from curr directory which
-- still have a corresponding file in the gc directory.
endGC :: BlobStore -> IO ()
endGC (BlobStore dir) = do
  checkgcDir <- doesDirectoryExist gcDir
  when checkgcDir $ do
    forAllInDirectory gcDir (deleteFileInDir currDir)
    removeDirectoryRecursive gcDir
  where
    gcDir   = dir </> gcDirName
    currDir = dir </> currDirName
