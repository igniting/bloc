{-|
  Module      : Data.Blob.FileOperations
  Stability   : Experimental
  Portability : non-portable (requires POSIX)
-}

module Data.Blob.FileOperations where

import           Control.Exception      (bracket)
import           Control.Monad          (unless, void, when)
import           Data.Blob.Directories
import           Data.Blob.Types        (BlobId)
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
import           System.Posix.Directory (DirStream, closeDirStream,
                                         openDirStream, readDirStream)
import qualified System.Posix.IO        as P
import           System.Posix.Types     (Fd (..))

-- | Create an empty directory
createDir :: FilePath -> IO ()
createDir = void . tryIOError . createDirectory

-- | Creates a unique file in the temp directory
createUniqueFile :: FilePath -> IO FilePath
createUniqueFile dir = do
  filename <- fmap toString nextRandom
  createFile (tempDir dir </> filename)
  return filename

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

-- | Read all the contents from a handle
readAllFromHandle :: S.Handle -> IO B.ByteString
readAllFromHandle = B.hGetContents

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

-- | Delete the given file
deleteFile :: FilePath -> IO ()
deleteFile = removeFile

-- | Delete the file by name and base directory
deleteFileInDir :: FilePath -- ^ Base directory
                -> FilePath -- ^ File name
                -> IO ()
deleteFileInDir dir name = deleteFile (dir </> name)

-- | Try to move a file, ignore any IO errors raised
moveFile :: FilePath -> FilePath -> IO ()
moveFile oldpath newpath = void . tryIOError $ renameFile oldpath newpath

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

-- | 'recoverFromGC' recovers from a state when the process crashed
-- after calling 'startGC'.
recoverFromGC :: FilePath -> IO ()
recoverFromGC dir = do
  checkgcDir <- doesDirectoryExist (gcDir dir)
  when checkgcDir $ do
    forAllInDirectory (gcDir dir)
      (\file -> renameFile (gcDir dir </> file) (currDir dir </> file))
    removeDirectory (gcDir dir)

-- | Return a readonly handle for blobid
getHandle :: BlobId -> IO S.Handle
getHandle blobId = do
  -- TODO: Use EitherT
  r1 <- tryIOError . openFileForRead $ getFullPath currDirName blobId
  case r1 of
       Right handle -> return handle
       Left _ -> do
         r2 <- tryIOError . openFileForRead $ getFullPath gcDirName blobId
         case r2 of
              Right handle -> return handle
              Left _ -> openFileForRead $ getFullPath currDirName blobId
