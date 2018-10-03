{-# OPTIONS_GHC -Wno-overflowed-literals #-}

import Data.Binary.Get
import Data.Binary.Put
import Language.WebAssembly.WireFormat
import Language.WebAssembly.WireFormat.Orphans
import Test.QuickCheck
import Test.QuickCheck.Gen

testCodec :: (Eq a, Show a) => Gen a -> Get a -> (a -> Put) -> Property
testCodec gen g p =
  forAll gen $ \x ->
    case runGetOrFail g $ runPut $ p x of
      Right (_, _, r) -> x == r
      _ -> False

testLEB128 :: Property
testLEB128 =
  conjoin
    [ testCodec (pure 0) getVU32 putVU32
    , testCodec (pure maxBound) getVU32 putVU32
    , testCodec (pure minBound) getVS32 putVS32
    , testCodec (pure (-1)) getVS32 putVS32
    , testCodec (pure 0) getVS32 putVS32
    , testCodec (pure maxBound) getVS32 putVS32
    , testCodec (pure minBound) getVS64 putVS64
    , testCodec (pure (-1)) getVS64 putVS64
    , testCodec (pure 0) getVS64 putVS64
    , testCodec (pure maxBound) getVS64 putVS64
    , testCodec (pure 0xFFFFFFFFFFFFFFF8) getVS64 putVS64
    , testCodec chooseAny getVU32 putVU32
    , testCodec chooseAny getVS32 putVS32
    , testCodec chooseAny getVS64 putVS64
    , testCodec chooseAny getF32 putF32
    , testCodec chooseAny getF64 putF64
    ]

main :: IO ()
main =
  quickCheck $
  withMaxSuccess 576460752303423488 $
  conjoin [testLEB128, testCodec (resize 64 genModule) getModule putModule]
