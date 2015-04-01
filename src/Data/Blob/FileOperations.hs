{-|
  Module      : Data.Blob.FileOperations
  Description : Contains wrapper for all file operations
  Stability   : Experimental
-}

module Data.Blob.FileOperations where

import qualified Data.ByteString   as B
import           System.Directory
import           System.IO
import           System.Posix.Temp (mkstemp)

-- | Creates a unique filename and open it for reading/writing
createUniqueFile :: String -> IO (FilePath, Handle)
createUniqueFile = mkstemp

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
