{-|
  Module      : Data.Blob.Types
  Stability   : Experimental
  Portability : non-portable (requires POSIX)
-}

module Data.Blob.Types where

import           Crypto.Hash.SHA512
import           Data.ByteString    (ByteString)
import           System.IO

-- | Wrapper around strict 'ByteString'.
newtype Blob = Blob ByteString deriving (Eq)

-- | This is used to store the base directory
newtype BlobStore = BlobStore FilePath

class Location a where
  baseDir   :: a -> FilePath
  blobName  :: a -> FilePath

data TempLocation = TempLocation FilePath FilePath

instance Location TempLocation where
  baseDir  (TempLocation dir _)  = dir
  blobName (TempLocation _ name) = name

data BlobId = BlobId FilePath FilePath

instance Location BlobId where
  baseDir  (BlobId dir _)  = dir
  blobName (BlobId _ name) = name

data WriteContext = WriteContext { writeLoc    :: TempLocation
                                 , writeHandle :: Handle
                                 , hashCtx     :: Ctx
                                 }

-- | ReadContext stores the file handle which has been opened
-- for reading.
data ReadContext = ReadContext Handle
