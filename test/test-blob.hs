import           Data.Blob
import           Data.Blob.GC
import           Data.ByteString.Char8   (pack)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

testDir :: FilePath
testDir = "/tmp/blob-test-dir"

-- | Write the given string as contents of a blob and return it's location
writeStringToBlob :: String -> IO BlobId
writeStringToBlob s = createBlob testDir >>=
  (\wc -> writePartial wc (Blob (pack s))) >>=
  finalizeWrite

-- | Read given length from a location
readFromBlob :: Int -> BlobId -> IO Blob
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

-- | Should be able to read "safe" blobs during GC
propReadMarkedDuringGC :: String -> Property
propReadMarkedDuringGC s = monadicIO $ do
  loc <- run $ writeStringToBlob s
  run $ startGC testDir
  run $ markBlobAsAccessible loc
  bs <- run $ readFromBlob (length s) loc
  run $ endGC testDir
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
main = hspec $ do
  describe "basic" $
    it "should be able to read from blob just written" $ property propWriteAndRead
  describe "gc" $ do
    it "should be able to read blobs during GC" $ property propReadDuringGC
    it "should be able to create blobs during GC" $ property propCreateDuringGC
    it "should be able to read moved blobs during GC" $ property propReadMarkedDuringGC
