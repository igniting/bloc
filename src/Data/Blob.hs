{-|
  Module      : Data.Blob
  Description : Library for reading and writing large binary values to disk
  Stability   : Experimental
-}

module Data.Blob ( Blob (..)
                 , Location (..)
                 , WriteContext
                 , ReadContext
                 , createBlob
                 , initWrite
                 , writePartial
                 , finalizeWrite
                 , initRead
                 , readPartial
                 , finalizeRead
                 , deleteBlob
                 ) where

import qualified Crypto.Hash.SHA512       as SHA512
import qualified Data.Blob.FileOperations as F
import           Data.Blob.Types

-- | Create file for storing the blob
createBlob :: FilePath -> IO Location
createBlob dir = do
  newpath <- F.createUniqueFile dir
  return $ Location dir newpath

-- | Open file for writing
initWrite :: Location -> IO WriteContext
initWrite loc = do
  h <- F.openFileForWrite $ fullPath loc
  return $ WriteContext loc h SHA512.init

-- | Write part of blob to a given blob id
writePartial :: WriteContext -> Blob -> IO WriteContext
writePartial (WriteContext l h ctx) (Blob b) = do
  F.writeToHandle h b
  let newctx = SHA512.update ctx b
  return $ WriteContext l h newctx

-- | Finalize write
finalizeWrite :: WriteContext -> IO Location
finalizeWrite (WriteContext l h ctx) = do
  F.closeHandle h
  let newfilename = "sha512-" ++ F.toFileName (SHA512.finalize ctx)
  newpath <- F.moveFile (fullPath l) (baseDir l) newfilename
  return $ Location (baseDir l) newpath

-- | Open file for reading
initRead :: Location -> IO ReadContext
initRead loc = do
  h <- F.openFileForRead $ fullPath loc
  return $ ReadContext loc h

-- | Read given number of bytes from the blob handle
readPartial :: ReadContext -> Int -> IO Blob
readPartial (ReadContext _ h) sz = fmap Blob $ F.readFromHandle h sz

-- | Complete reading from a file
finalizeRead :: ReadContext -> IO ()
finalizeRead (ReadContext _ h) = F.closeHandle h

-- | Delete the file corresponding to the blob id
deleteBlob :: Location -> IO ()
deleteBlob (Location _ path) = F.deleteFile path
