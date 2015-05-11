{-|
  Module      : Data.Blob.Types
  Description : Type definitions
  Stability   : Experimental
-}

module Data.Blob.Types where

import           Crypto.Hash.SHA512
import           Data.ByteString    (ByteString)
import           System.IO

newtype Blob = Blob ByteString deriving (Eq)

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

data ReadContext = ReadContext Handle
