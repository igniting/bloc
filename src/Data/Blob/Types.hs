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

data Location = Location { baseDir  :: FilePath
                         , blobName :: FilePath
                         }

data WriteContext = WriteContext { writeLoc    :: Location
                                 , writeHandle :: Handle
                                 , hashCtx     :: Ctx
                                 }

data ReadContext = ReadContext { readLoc    :: Location
                               , readHandle :: Handle
                               }
