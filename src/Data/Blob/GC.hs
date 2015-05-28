{-|
  Module      : Data.Blob.GC
  Description : Methods related to garbage collection of blobs
  Stability   : Experimental
-}

module Data.Blob.GC where

import           Control.Monad            (when)
import           Data.Blob.FileOperations
import           Data.Blob.Types
import           System.Directory
import           System.FilePath.Posix    ((</>))

-- | Initialize garbage collection for blobs in a given BlobStore
-- We create an empty file in the GC directory corresponding to
-- each blob in the active directory
startGC :: BlobStore -> IO ()
startGC (BlobStore dir) = do
  checkgcDir <- doesDirectoryExist gcDir
  if checkgcDir
     then error "Another GC already in progress."
     else do
       createDirectory gcDir
       forAllInDirectory currDir (createFileInDir gcDir)
  where
    gcDir   = dir </> oldDir
    currDir = dir </> activeDir

-- | Mark a blob as accessible during a GC.
-- This deletes the corresponding file of blob from GC directory
markBlobAsAccessible :: BlobId -> IO ()
markBlobAsAccessible loc = deleteFile (getOldPath loc)

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
