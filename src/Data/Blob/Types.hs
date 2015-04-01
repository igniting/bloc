{-|
  Module      : Data.Blob.Types
  Description : Type definitions
  Stability   : Experimental
-}

module Data.Blob.Types where

import Data.ByteString (ByteString)

newtype Blob    = Blob ByteString
type    BlobId  = FilePath
