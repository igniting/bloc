{-|
  Module      : Data.Blob.Types
  Description : Type definitions
  Stability   : Experimental
-}

module Data.Blob.Types where

import           Data.ByteString (ByteString)
import           System.IO

newtype Blob       = Blob ByteString
type    BlobId     = FilePath
type    BlobHandle = Handle
