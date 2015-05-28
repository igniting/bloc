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

-- | Initialize garbage collection for blobs in a given folder
-- The active directory is renamed as "old directory".
-- Any blobs created at the time of GC will still go to the
-- active directory.
startGC :: BlobStore -> IO ()
startGC (BlobStore dir) = do
  let gcDir = dir </> oldDir
  checkgcDir <- doesDirectoryExist gcDir
  if checkgcDir
     then error "Another GC already in progress."
     else do
       let currDir = dir </> activeDir
       renameDirectory currDir gcDir
       createDirectory currDir
       return ()

-- | Mark a blob as accessible during a GC.
-- This moves the blob from "old directory" to active directory.
markBlobAsAccessible :: BlobId -> IO ()
markBlobAsAccessible loc = renameFile (getOldPath loc) (getActivePath loc)

-- | Stops the garbage collection.
-- This deletes the "old directory" which now contains only
-- the unreferenced blobs.
endGC :: BlobStore -> IO ()
endGC (BlobStore dir) = do
  let gcDir = dir </> oldDir
  checkgcDir <- doesDirectoryExist gcDir
  when checkgcDir $ removeDirectoryRecursive (dir </> oldDir)
