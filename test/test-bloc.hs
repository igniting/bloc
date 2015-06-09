import           Data.Blob
import           Data.Blob.GC
import           Data.ByteString.Char8   (pack)
import           System.Directory        (removeDirectoryRecursive)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

testDir :: FilePath
testDir = "/tmp/blob-test-dir"

gcDelay :: Int
gcDelay = 0

-- | Write the given string as contents of a blob and return it's location
writeStringToBlob :: String -> BlobStore -> IO BlobId
writeStringToBlob s blobStore = createBlob blobStore (Blob (pack s))

-- | Should be able to read from blob just written to
propWriteAndRead :: String -> Property
propWriteAndRead s = monadicIO $ do
  blobStore <- run $ openBlobStore testDir
  loc <- run $ writeStringToBlob s blobStore
  bs <- run $ readBlob loc
  assert (Blob (pack s) == bs)

-- | Should be able to read blobs during GC
propReadDuringGC :: String -> Property
propReadDuringGC s = monadicIO $ do
  blobStore <- run $ openBlobStore testDir
  loc <- run $ writeStringToBlob s blobStore
  rc <- run $ startRead loc
  run $ startGC blobStore
  run $ endGC blobStore gcDelay
  bs <- run $ readPartial rc (length s)
  run $ endRead rc
  assert (Blob (pack s) == bs)

-- | Should be able to read "safe" blobs during GC
propReadMarkedDuringGC :: String -> Property
propReadMarkedDuringGC s = monadicIO $ do
  blobStore <- run $ openBlobStore testDir
  loc <- run $ writeStringToBlob s blobStore
  run $ startGC blobStore
  run $ markAsAccessible loc
  bs <- run $ readBlob loc
  run $ endGC blobStore gcDelay
  assert (Blob (pack s) == bs)

-- | Should be able to create blobs during GC
propCreateDuringGC :: String -> Property
propCreateDuringGC s = monadicIO $ do
  blobStore <- run $ openBlobStore testDir
  run $ startGC blobStore
  loc <- run $ writeStringToBlob s blobStore
  run $ endGC blobStore gcDelay
  bs <- run $ readBlob loc
  assert (Blob (pack s) == bs)

-- | Should be able to skip bytes while reading
propSkipBytes :: String -> String -> Property
propSkipBytes s1 s2 = monadicIO $ do
  blobStore <- run $ openBlobStore testDir
  loc <- run $ writeStringToBlob (s1 ++ s2) blobStore
  rc <- run $ startRead loc
  run $ skipBytes rc (fromIntegral $ length s1)
  bs <- run $ readPartial rc (length s2)
  run $ endRead rc
  assert (Blob (pack s2) == bs)

startGCTwice :: IO ()
startGCTwice = do
  blobStore <- openBlobStore testDir
  startGC blobStore
  startGC blobStore

cleanUp :: IO ()
cleanUp = removeDirectoryRecursive testDir

main :: IO ()
main = hspec $ do
  describe "basic" $
    it "should be able to read from blob just written" $ property propWriteAndRead
  describe "read" $
    it "should be able to skip bytes while reading" $ property propSkipBytes
  describe "gc" $ do
    it "should be able to read blobs during GC" $ property propReadDuringGC
    it "should be able to create blobs during GC" $ property propCreateDuringGC
    it "should be able to read moved blobs during GC" $ property propReadMarkedDuringGC
    it "should fail when startGC is called twice" $ do
      startGCTwice `shouldThrow` anyIOException
      cleanUp
