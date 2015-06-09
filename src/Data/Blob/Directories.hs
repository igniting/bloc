{-|
  Module      : Data.Blob.Directories
  Stability   : Experimental
  Portability : non-portable (requires POSIX)
-}

module Data.Blob.Directories where

import           Data.Blob.Types
import           System.FilePath.Posix ((</>))

-- | Directory for storing partial blobs
tempDirName :: FilePath
tempDirName = "tmp"

-- | Given the root directory, 'tempDir' returns the absolute
-- path of temp directory
tempDir :: FilePath -> FilePath
tempDir dir = dir </> tempDirName

-- | Directory for storing active blobs
currDirName :: FilePath
currDirName = "curr"

-- | Given the root directory, 'currDir' returns the absolute
-- path of curr directory
currDir :: FilePath -> FilePath
currDir dir = dir </> currDirName

-- | Directory for storing blobs during GC
gcDirName :: FilePath
gcDirName = "gc"

-- | Name of the file containing metadata
metadataFile :: FilePath
metadataFile = "metadata"

-- | Given the root directory, 'gcDir' returns the absolute
-- path of gc directory
gcDir :: FilePath -> FilePath
gcDir dir = dir </> gcDirName

-- | Given any location and the sub-directory,
-- 'getFullPath' returns the absolute path for that location
getFullPath :: Location a => FilePath -> a -> FilePath
getFullPath dir loc = baseDir loc </> dir </> blobName loc
