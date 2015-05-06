import           Data.Blob
import           Data.Blob.GC
import           Data.ByteString.Char8   (pack)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

testDir :: FilePath
testDir = "/tmp/blob-test-dir"

-- | Write the given string as contents of a blob and return it's location
writeStringToBlob :: String -> IO Location
writeStringToBlob s = createBlob testDir >>=
  initWrite >>=
  (\wc -> writePartial wc (Blob (pack s))) >>=
  finalizeWrite

-- | Read given length from a location
readFromBlob :: Int -> Location -> IO Blob
readFromBlob size loc = do
  rc <- initRead loc
  bs <- readPartial rc size
  finalizeRead rc
  return bs

-- | Should be able to read from blob just written to
propWriteAndRead :: String -> Property
propWriteAndRead s = monadicIO $ do
  loc <- run $ writeStringToBlob s
  bs <- run $ readFromBlob (length s) loc
  assert (Blob (pack s) == bs)

-- | Should be able to read blobs during GC
propReadDuringGC :: String -> Property
propReadDuringGC s = monadicIO $ do
  loc <- run $ writeStringToBlob s
  rc <- run $ initRead loc
  run $ startGC testDir
  run $ endGC testDir
  bs <- run $ readPartial rc (length s)
  run $ finalizeRead rc
  assert (Blob (pack s) == bs)

-- | Should be able to create blobs during GC
propCreateDuringGC :: String -> Property
propCreateDuringGC s = monadicIO $ do
  run $ startGC testDir
  loc <- run $ writeStringToBlob s
  run $ endGC testDir
  bs <- run $ readFromBlob (length s) loc
  assert (Blob (pack s) == bs)

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 50 } propWriteAndRead
  quickCheckWith stdArgs { maxSuccess = 50 } propReadDuringGC
  quickCheckWith stdArgs { maxSuccess = 50 } propCreateDuringGC
