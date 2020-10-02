{-# LANGUAGE FlexibleContexts #-}

module Wasm.Util.Float
  ( floatToBits
  , floatFromBits
  , doubleToBits
  , doubleFromBits
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
