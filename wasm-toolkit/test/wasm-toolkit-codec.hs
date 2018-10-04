{-# OPTIONS_GHC -Wno-overflowed-literals #-}

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Language.WebAssembly.WireFormat
import Language.WebAssembly.WireFormat.Orphans
import Test.QuickCheck
import Test.QuickCheck.Gen

testCodecGen ::
     (Eq a, Show a) => Gen a -> (a -> [a]) -> Get a -> (a -> Put) -> Property
testCodecGen gen s g p =
  forAllShrink gen s $ \x ->
    case runGetOrFail g $ runPut $ p x of
      Right (_, _, r) -> x == r
      _ -> False

testCodecFile :: LBS.ByteString -> Get a -> (a -> Put) -> Property
testCodecFile buf g p =
  property $
  case runGetOrFail g buf of
    Right (_, _, x) -> runPut (p x) == buf
    _ -> False

testCodecModule :: FilePath -> IO Property
testCodecModule p = do
  buf <- LBS.readFile p
  pure $ testCodecFile buf getModule putModule

testLEB128Static :: Property
testLEB128Static =
  conjoin
    [ testCodecGen (pure 0) (const []) getVU32 putVU32
    , testCodecGen (pure maxBound) (const []) getVU32 putVU32
    , testCodecGen (pure minBound) (const []) getVS32 putVS32
    , testCodecGen (pure (-1)) (const []) getVS32 putVS32
    , testCodecGen (pure 0) (const []) getVS32 putVS32
    , testCodecGen (pure maxBound) (const []) getVS32 putVS32
    , testCodecGen (pure minBound) (const []) getVS64 putVS64
    , testCodecGen (pure (-1)) (const []) getVS64 putVS64
    , testCodecGen (pure 0) (const []) getVS64 putVS64
    , testCodecGen (pure maxBound) (const []) getVS64 putVS64
    , testCodecGen (pure 0xFFFFFFFFFFFFFFF8) (const []) getVS64 putVS64
    ]

testLEB128Dynamic :: Property
testLEB128Dynamic =
  conjoin
    [ testCodecGen chooseAny (const []) getVU32 putVU32
    , testCodecGen chooseAny (const []) getVS32 putVS32
    , testCodecGen chooseAny (const []) getVS64 putVS64
    , testCodecGen chooseAny (const []) getF32 putF32
    , testCodecGen chooseAny (const []) getF64 putF64
    ]

main :: IO ()
main = do
  quickCheck testLEB128Static
  for_
    [ "test/array.wasm"
    , "test/fib.wasm"
    , "test/jsffi.wasm"
    , "test/rtsapi.wasm"
    , "test/stableptr.wasm"
    ] $
    testCodecModule >=> quickCheck
  r <-
    quickCheckResult $
    withMaxSuccess 576460752303423488 $
    conjoin
      [ testLEB128Dynamic
      , testCodecGen (resize 64 genModule) genericShrink getModule putModule
      ]
  writeFile "test/wasm-toolkit-codec.txt" $ show r