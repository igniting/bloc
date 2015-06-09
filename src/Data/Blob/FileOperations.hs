{-|
  Module      : Data.Blob.FileOperations
  Stability   : Experimental
  Portability : non-portable (requires POSIX)
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
import qualified System.Posix.IO        as P
import           System.Posix.Types     (Fd (..))

-- | Directory for storing partial blobs
tempDirName :: FilePath
tempDirName = "tmp"

-- | Directory for storing active blobs
currDirName :: FilePath
currDirName = "curr"

-- | Directory for storing file names of blobs during GC
gcDirName :: FilePath
gcDirName = "gc"

-- | Return full path for blob stored in temp directory
getTempPath :: TempLocation -> FilePath
getTempPath loc = baseDir loc </> tempDirName </> blobName loc

-- | Return full path for blob stored in GC directory
getGCPath :: BlobId -> FilePath
getGCPath loc = baseDir loc </> gcDirName </> blobName loc

-- | Return full path for blob stored in active directory
getCurrPath :: BlobId -> FilePath
getCurrPath loc = baseDir loc </> currDirName </> blobName loc

-- | Create temp directory if missing
createTempIfMissing :: FilePath -> IO ()
createTempIfMissing dir = createDirectoryIfMissing True (dir </> tempDirName)

-- | Create active directory if missing
createCurrIfMissing :: FilePath -> IO ()
createCurrIfMissing dir = createDirectoryIfMissing True (dir </> currDirName)

-- | Creates a unique file in the temp directory
createUniqueFile :: FilePath -> IO FilePath
createUniqueFile dir = do
  filename <- fmap toString nextRandom
  createFile (dir </> tempDirName </> filename)
  return filename

-- | Move file to active directory
moveFile :: FilePath -> FilePath -> FilePath -> IO ()
moveFile path dir filename = renameFile path newPath where
  newPath = dir </> currDirName </> filename

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

-- | Read an entire file
readFile :: FilePath -> IO B.ByteString
readFile = B.readFile

-- | Skip given number of bytes forward
seekHandle :: S.Handle -> Integer -> IO ()
seekHandle handle = S.hSeek handle S.RelativeSeek

-- | Close the given handle
closeHandle :: S.Handle -> IO ()
closeHandle = S.hClose

-- | Sync the data to disk
syncAndClose :: S.Handle -> IO ()
syncAndClose handle = P.handleToFd handle >>= fdatasync

-- | Binding to the C @fdatasync@ function
fdatasync :: Fd -> IO ()
fdatasync (Fd fd) = throwErrnoIfMinus1_ "fdatasync" $ c_fdatasync fd

-- | Foreign interface for @fdatasync@ function
foreign import ccall "fdatasync" c_fdatasync :: CInt -> IO CInt

-- | Call @fdatasync@ on a directory
syncDir :: FilePath -> IO ()
syncDir dir = bracket
  (P.openFd dir P.ReadOnly Nothing P.defaultFileFlags)
  P.closeFd
  fdatasync

-- | Sync the curr dir of a given base directory
syncCurrDir :: FilePath -> IO ()
syncCurrDir basedir = syncDir (basedir </> currDirName)

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
