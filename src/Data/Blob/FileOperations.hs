{-|
  Module      : Data.Blob.FileOperations
  Description : Contains wrapper for all file operations
  Stability   : Experimental
-}

module Data.Blob.FileOperations where

import qualified Data.ByteString       as B
import           Data.UUID             (toString)
import           Data.UUID.V4          (nextRandom)
import           System.Directory
import           System.FilePath.Posix ((</>))

-- | Creates a unique file in a given directory
createUniqueFile :: FilePath -> IO FilePath
createUniqueFile baseDir = do
  filename <- fmap toString nextRandom
  -- Create the base directory if missing
  createDirectoryIfMissing True baseDir
  let absoluteFilePath = baseDir </> filename
  createFile absoluteFilePath
  return absoluteFilePath

-- | Create an empty file.
-- | If the file exists, replace it with an empty file
createFile :: FilePath -> IO ()
createFile path = B.writeFile path B.empty

-- | Append a ByteString to a file
appendFile :: FilePath -> B.ByteString -> IO ()
appendFile = B.appendFile

-- | Read contents from a given file name
readFromFile :: FilePath -> IO B.ByteString
readFromFile = B.readFile

-- | Delete the given file
deleteFile :: FilePath -> IO ()
deleteFile = removeFile
