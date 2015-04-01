{-|
  Module      : Data.Blob
  Description : Library for reading and writing large binary values to disk
  Stability   : Experimental
-}

module Data.Blob where

import Data.Blob.Types

create :: Blob -> IO BlobId
create = undefined

read :: BlobId -> IO Blob
read = undefined

delete :: Blob -> IO ()
delete = undefined
