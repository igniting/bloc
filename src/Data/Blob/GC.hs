{-|
  Module      : Data.Blob.GC
  Description : Methods related to garbage collection of blobs
  Stability   : Experimental
-}

module Data.Blob.GC where

import           Control.Monad            (void, when)
import           Data.Blob.FileOperations
import           Data.Blob.Types
import           System.Directory
import           System.FilePath.Posix    ((</>))
import           System.IO.Error          (tryIOError)

-- | Initialize garbage collection for blobs in a given BlobStore
-- We create an empty file in the GC directory corresponding to
-- each blob in the active directory.
-- startGC throws an error if another GC is already running on
-- the same BlobStore.
startGC :: BlobStore -> IO ()
startGC (BlobStore dir) = do
  createDirectory gcDir
  forAllInDirectory currDir (createFileInDir gcDir)
  where
    gcDir   = dir </> oldDir
    currDir = dir </> activeDir

-- | Mark a blob as accessible during a GC.
-- This deletes the corresponding file of blob from GC directory
markAsAccessible :: BlobId -> IO ()
markAsAccessible = void . tryIOError . deleteFile . getOldPath

-- | Stops the garbage collection.
-- This deletes the blobs from active directory which
-- still have a corresponding file in the GC directory
endGC :: BlobStore -> IO ()
endGC (BlobStore dir) = do
  checkgcDir <- doesDirectoryExist gcDir
  when checkgcDir $ do
    forAllInDirectory gcDir (deleteFileInDir currDir)
    removeDirectoryRecursive gcDir
  where
    gcDir   = dir </> oldDir
    currDir = dir </> activeDir
