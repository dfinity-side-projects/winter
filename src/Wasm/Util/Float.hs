{-# LANGUAGE FlexibleContexts #-}

module Wasm.Util.Float
  ( floatToBits
  , floatFromBits
  , doubleToBits
  , doubleFromBits
  , f32CanonicalNaN
  , f64CanonicalNaN
  ) where

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Word
import GHC.ST (runST, ST)

cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s))
     => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

floatToBits :: Float -> Word32
floatToBits x = runST (cast x)

floatFromBits :: Word32 -> Float
floatFromBits x = runST (cast x)

doubleToBits :: Double -> Word64
doubleToBits x = runST (cast x)

doubleFromBits :: Word64 -> Double
doubleFromBits x = runST (cast x)

-- | In GHC 0/0 generates NaN with sign bit set (i.e. negative NaN) which is not
-- the canonical NaN specified in the Wasm spec. This is the canonical NaN.
f32CanonicalNaN :: Float
f32CanonicalNaN =
    -- 0b0_11111111_10000000000000000000000
    floatFromBits 2143289344

-- | Like `f32CanonicalNaN` but for F64.
f64CanonicalNaN :: Double
f64CanonicalNaN =
    -- 0b0_11111111111_1000000000000000000000000000000000000000000000000000
    doubleFromBits 9221120237041090560
