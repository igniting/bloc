{-|
  Module      : Data.Blob.FileOperations
  Description : Contains wrapper for all file operations
  Stability   : Experimental
-}

module Data.Blob.FileOperations where

import qualified Data.ByteString  as B
import           Data.UUID        (toString)
import           Data.UUID.V4     (nextRandom)
import           System.Directory
import           System.IO

-- | Creates a unique filename and open it for reading/writing
createUniqueFile :: IO (FilePath, Handle)
createUniqueFile = do
  filename <- fmap toString nextRandom
  handle <- openBinaryFile filename WriteMode
  return (filename, handle)

-- | Write contents to given file handle and close it on completion
writeToHandle :: Handle -> B.ByteString -> IO ()
writeToHandle handle contents = do
  hSetBuffering handle NoBuffering
  B.hPut handle contents
  hClose handle

-- | Read contents from a given file name
readFromFile :: FilePath -> IO B.ByteString
readFromFile = B.readFile

-- | Delete the given file
deleteFile :: FilePath -> IO ()
deleteFile = removeFile
