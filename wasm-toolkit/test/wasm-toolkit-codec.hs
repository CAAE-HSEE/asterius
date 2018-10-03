import Data.Binary.Get
import Data.Binary.Put
import Language.WebAssembly.WireFormat
import Language.WebAssembly.WireFormat.Orphans
import Test.QuickCheck

testCodec :: (Eq a, Show a) => Gen a -> Get a -> (a -> Put) -> Property
testCodec gen g p =
  forAll gen $ \x ->
    case runGetOrFail g $ runPut $ p x of
      Right (_, _, r) -> x == r
      _ -> False

main :: IO ()
main =
  quickCheck $
  withMaxSuccess 576460752303423488 $
  testCodec (resize 64 genSection) getSection putSection
