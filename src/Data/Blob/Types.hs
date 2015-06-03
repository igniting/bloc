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

-- | This is used to store the base directory.
--
-- Each application should maintain there own BlobStores.
newtype BlobStore = BlobStore FilePath

class Location a where
  baseDir   :: a -> FilePath
  blobName  :: a -> FilePath

-- | 'TempLocation' is used to identify partial blobs.
data TempLocation = TempLocation FilePath FilePath

instance Location TempLocation where
  baseDir  (TempLocation dir _)  = dir
  blobName (TempLocation _ name) = name

-- | BlobId is used to uniquely identify any blob.
data BlobId = BlobId FilePath FilePath

instance Location BlobId where
  baseDir  (BlobId dir _)  = dir
  blobName (BlobId _ name) = name

-- | WriteContext maintains the blob's state during writing.
data WriteContext = WriteContext { writeLoc    :: TempLocation
                                 , writeHandle :: Handle
                                 , hashCtx     :: Ctx
                                 }

-- | ReadContext maintains the reading state of a blob.
data ReadContext = ReadContext Handle
