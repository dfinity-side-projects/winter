module Property (tests) where

import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Wasm.Binary.LEB128

tests :: TestTree
tests = testGroup "property"
   [ testProperty "compressible unsigned 32-bit integer" propWord32LEB128
   , testProperty "compressible signed 32-bit integer" propInt32LEB128
   , testProperty "compressible unsigned 64-bit integer" propWord64LEB128
   , testProperty "compressible signed 64-bit integer" propInt64LEB128
   ]

propWord32LEB128 :: Word32 -> Bool
propWord32LEB128 x = x == runGet (getULEB128 32) (runPut (putULEB128 x))

propInt32LEB128 :: Int32 -> Bool
propInt32LEB128 x = x == runGet (getSLEB128 32) (runPut (putSLEB128 x))

propWord64LEB128 :: Word64 -> Bool
propWord64LEB128 x = x == runGet (getULEB128 64) (runPut (putULEB128 x))

propInt64LEB128 :: Int64 -> Bool
propInt64LEB128 x = x == runGet (getSLEB128 64) (runPut (putSLEB128 x))
