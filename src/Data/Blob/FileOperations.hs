{-|
  Module      : Data.Blob.FileOperations
  Description : Contains wrapper for all file operations
  Stability   : Experimental
-}

module Data.Blob.FileOperations where

import           Control.Exception      (bracket)
import           Control.Monad          (unless, when)
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
import           System.Posix.Directory (DirStream, closeDirStream,
                                         openDirStream, readDirStream)
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
moveFile :: FilePath -> FilePath -> FilePath -> IO ()
moveFile path dir filename = renameFile path newPath where
  newPath = dir </> activeDir </> filename

-- | Create an empty file.
-- | If the file exists, replace it with an empty file
createFile :: FilePath -> IO ()
createFile path = B.writeFile path B.empty

-- | Create an empty file in a given directory
createFileInDir :: FilePath  -- ^ Parent directory
                -> FilePath  -- ^ File name
                -> IO ()
createFileInDir dir name = createFile (dir </> name)

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

-- | Delete the file by name and base directory
deleteFileInDir :: FilePath -- ^ Base directory
                -> FilePath -- ^ File name
                -> IO ()
deleteFileInDir dir name = deleteFile (dir </> name)

-- | Generate a printable file name
toFileName :: B.ByteString -> FilePath
toFileName = unpack . encode

-- | Perform an action on all files of a given directory
forAllInDirectory :: FilePath             -- ^ Path of base directory
                  -> (FilePath -> IO ())  -- ^ Action to be performed on each file
                  -> IO ()
forAllInDirectory dir action = bracket (openDirStream dir) closeDirStream loop where
  loop :: DirStream -> IO ()
  loop dirstream = do
    e <- readDirStream dirstream
    unless (null e) $ do
      when (isProperFile e) $ action e
      loop dirstream
  isProperFile :: FilePath -> Bool
  isProperFile filename = filename /= "." && filename /= ".."
