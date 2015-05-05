import           Data.Blob
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

-- | Should be able to read from blob just written to
propWriteAndRead :: String -> Property
propWriteAndRead s = monadicIO $ do
  loc <- run $ writeStringToBlob s
  rc <- run $ initRead loc
  bs <- run $ readPartial rc (length s)
  run $ finalizeRead rc
  assert (Blob (pack s) == bs)

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 50 } propWriteAndRead
