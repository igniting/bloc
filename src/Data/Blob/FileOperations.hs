{-|
  Module      : Data.Blob.FileOperations
  Description : Contains wrapper for all file operations
  Stability   : Experimental
-}

module Data.Blob.FileOperations where

import           Data.Blob.Types
import qualified Data.ByteString        as B
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (unpack)
import           Data.UUID              (toString)
import           Data.UUID.V4           (nextRandom)
import           Foreign.C.Error        (throwErrnoIfMinus1_)
import           Foreign.C.Types        (CInt (..))
import           System.Directory
import           System.FilePath.Posix  ((</>))
import qualified System.IO              as S
import           System.IO.Error        (tryIOError)
import           System.Posix.IO        (handleToFd)
import           System.Posix.Types     (Fd (..))

-- | Directory for storing partial blobs
tempDir :: FilePath
tempDir = "tmp"

-- | Directory for storing active blobs
activeDir :: FilePath
activeDir = "curr"

-- | Directory for storing blobs while GC
oldDir :: FilePath
oldDir = "old"

-- | Return full path for blob stored in temp directory
getTempPath :: TempLocation -> FilePath
getTempPath loc = baseDir loc </> tempDir </> blobName loc

-- | Return full path for blob stored in old directory
getOldPath :: BlobId -> FilePath
getOldPath loc = baseDir loc </> oldDir </> blobName loc

-- | Return full path for blob stored in active directory
getActivePath :: BlobId -> FilePath
getActivePath loc = baseDir loc </> activeDir </> blobName loc

-- | Create temp directory if missing
createTempIfMissing :: FilePath -> IO ()
createTempIfMissing dir = createDirectoryIfMissing True (dir </> tempDir)

-- | Create active directory if missing
createActiveIfMissing :: FilePath -> IO ()
createActiveIfMissing dir = createDirectoryIfMissing True (dir </> activeDir)

-- | Creates a unique file in the temp directory
createUniqueFile :: FilePath -> IO FilePath
createUniqueFile dir = do
  filename <- fmap toString nextRandom
  -- Create parent directory if missing
  let parentDir = dir </> tempDir
  createDirectoryIfMissing True parentDir
  let absoluteFilePath = parentDir </> filename
  createFile absoluteFilePath
  return filename

-- | Move file to active directory
-- Handle the case when the active directory might not be present
-- This can happen in startGC where activeDir is renamed to oldDir and
-- activeDir has not be created yet
moveFile :: FilePath -> FilePath -> FilePath -> IO ()
moveFile path dir filename = tryMoveFile 0 path newPath where
  newPath = dir </> activeDir </> filename
  maxTries = 5
  tryMoveFile :: Int -> FilePath -> FilePath -> IO ()
  tryMoveFile count path' newPath'
    | count > maxTries = error $ "Maximum tries reached while moving " ++ path'
    | otherwise        = do
      r <- tryIOError $ renameFile path' newPath'
      case r of
           Left _ -> tryMoveFile (count + 1) path' newPath'
           _ -> return ()

-- | Create an empty file.
-- | If the file exists, replace it with an empty file
createFile :: FilePath -> IO ()
createFile path = B.writeFile path B.empty

-- | Open file for writing
openFileForWrite :: FilePath -> IO S.Handle
openFileForWrite path = do
  h <- S.openFile path S.AppendMode
  S.hSetBuffering h S.NoBuffering
  return h

-- | Open file for reading
openFileForRead :: FilePath -> IO S.Handle
openFileForRead path = do
  h <- S.openFile path S.ReadMode
  S.hSetBuffering h S.NoBuffering
  return h

-- | Write contents to a given handle
writeToHandle :: S.Handle -> B.ByteString -> IO ()
writeToHandle = B.hPut

-- | Read contents from a given handle
readFromHandle :: S.Handle -> Int -> IO B.ByteString
readFromHandle = B.hGet

-- | Skip given number of bytes forward
seekHandle :: S.Handle -> Integer -> IO ()
seekHandle handle = S.hSeek handle S.RelativeSeek

-- | Close the given handle
closeHandle :: S.Handle -> IO ()
closeHandle = S.hClose

-- | Sync the data to disk
syncAndClose :: S.Handle -> IO ()
syncAndClose handle = handleToFd handle >>= fsync

-- | Binding to the C fsync function
fsync :: Fd -> IO ()
fsync (Fd fd) = throwErrnoIfMinus1_ "fsync" $ c_fsync fd

foreign import ccall "fsync" c_fsync :: CInt -> IO CInt

-- | Delete the given file
deleteFile :: FilePath -> IO ()
deleteFile = removeFile

-- | Generate a printable file name
toFileName :: B.ByteString -> FilePath
toFileName = unpack . encode
