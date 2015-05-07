{-|
  Module      : Data.Blob
  Description : Library for reading and writing large binary values to disk
  Stability   : Experimental
-}

module Data.Blob ( Blob (..)
                 , BlobId
                 , TempLocation
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
import           System.Directory         (doesFileExist)

-- | Create file for storing the blob
createBlob :: FilePath -> IO TempLocation
createBlob dir = do
  filename <- F.createUniqueFile dir
  return $ TempLocation dir filename

-- | Open file for writing
initWrite :: TempLocation -> IO WriteContext
initWrite loc = do
  h <- F.openFileForWrite $ F.getTempPath loc
  return $ WriteContext loc h SHA512.init

-- | Write part of blob to a given blob id
writePartial :: WriteContext -> Blob -> IO WriteContext
writePartial (WriteContext l h ctx) (Blob b) = do
  F.writeToHandle h b
  let newctx = SHA512.update ctx b
  return $ WriteContext l h newctx

-- | Finalize write
finalizeWrite :: WriteContext -> IO BlobId
finalizeWrite (WriteContext l h ctx) = do
  F.closeHandle h
  let newfilename = "sha512-" ++ F.toFileName (SHA512.finalize ctx)
  F.moveFile (F.getTempPath l) (baseDir l) newfilename
  return $ BlobId (baseDir l) newfilename

-- | Open blob for reading
-- The blob can be at two locations: curr/old
-- We first check in curr and if not found, read from old
initRead :: BlobId -> IO ReadContext
initRead loc = do
  let activePath = F.getActivePath loc
  chkInActive <- doesFileExist activePath
  if chkInActive
     then do
       h <- F.openFileForRead activePath
       return $ ReadContext loc h
     else do
       h <- F.openFileForRead $ F.getOldPath loc
       return $ ReadContext loc h

-- | Read given number of bytes from the blob handle
readPartial :: ReadContext -> Int -> IO Blob
readPartial (ReadContext _ h) sz = fmap Blob $ F.readFromHandle h sz

-- | Complete reading from a file
finalizeRead :: ReadContext -> IO ()
finalizeRead (ReadContext _ h) = F.closeHandle h

-- | Delete the file corresponding to the blob id
deleteBlob :: BlobId -> IO ()
deleteBlob loc = do
  let activePath = F.getActivePath loc
  chkInActive <- doesFileExist activePath
  F.deleteFile $ if chkInActive then activePath else F.getOldPath loc
