{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Wasm.Syntax.Values where

import           Control.DeepSeq
import           Data.Bits (shiftR, (.&.))
import           Data.Bool
import           Data.Int
import           Data.Word
import           GHC.Generics

import           Wasm.Syntax.Types
import           Wasm.Util.Float (floatToBits, doubleToBits)

data Value
  = I32 {-# UNPACK #-} !Int32
  | I64 {-# UNPACK #-} !Int64
  | F32 {-# UNPACK #-} !Float
  | F64 {-# UNPACK #-} !Double
  deriving (Generic, NFData)

typeOf :: Value -> ValueType
typeOf = \case
  I32 _ -> I32Type
  I64 _ -> I64Type
  F32 _ -> F32Type
  F64 _ -> F64Type

defaultValue :: ValueType -> Value
defaultValue = \case
  I32Type -> I32 0
  I64Type -> I64 0
  F32Type -> F32 0
  F64Type -> F64 0

valueOfBool :: Bool -> Value
valueOfBool = I32 . bool 0 1

instance Show Value where
  show v = case v of
    I32 i -> show i ++ "i32"
    I64 i -> show i ++ "i64"
    F32 f -> show f ++ "f32 (" ++ showFloatBits f ++ ")"
    F64 d -> show d ++ "f64 (" ++ showDoubleBits d ++ ")"

showFloatBits :: Float -> String
showFloatBits = showFBits 8 23 . fromIntegral . floatToBits

showDoubleBits :: Double -> String
showDoubleBits = showFBits 11 52 . doubleToBits

showFBits :: Word8 -> Word8 -> Word64 -> String
showFBits exp_digits signi_digits float_bits =
    show_bits (float_bits `shiftR` fromIntegral (exp_digits + signi_digits)) 1 ++
    "_" ++
    reverse (show_bits (float_bits `shiftR` fromIntegral signi_digits) exp_digits) ++
    "_" ++
    reverse (show_bits float_bits signi_digits)
  where
    show_bits :: Word64 -> Word8 -> String
    show_bits _ 0 = ""
    show_bits x n =
      let bit = if x .&. 1 == 1 then '1' else '0'
       in bit : show_bits (x `shiftR` 1) (n - 1)
