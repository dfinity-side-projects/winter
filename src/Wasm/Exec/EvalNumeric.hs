{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Wasm.Exec.EvalNumeric where

import Data.Bits
import Data.Int
import Data.Word
import Prelude hiding (lookup, elem)

import Wasm.Runtime.Memory
import Wasm.Syntax.Ops.Float as F
import Wasm.Syntax.Ops.Int as I
import Wasm.Syntax.Ops.Kind
import Wasm.Syntax.Types
import Wasm.Syntax.Values

{- Runtime type errors -}

data NumericError
  = NumericTypeError Int Value ValueType
  | NumericIntegerDivideByZero
  | NumericIntegerOverflow
  deriving (Show, Eq)

class Numeric t where
  type OpType t :: * -> *

  valueType :: ValueType

  toValue   :: t -> Value
  fromValue :: Int -> Value -> Either NumericError t

instance Numeric Word32 where
  type OpType Word32 = I32Op
  valueType = I32Type
  toValue = I32 . fromIntegral
  fromValue _ (I32 x) = Right (fromIntegral x)
  fromValue i x = Left (NumericTypeError i x I32Type)

instance Numeric Int32 where
  type OpType Int32 = I32Op
  valueType = I32Type
  toValue = I32
  fromValue _ (I32 x) = Right x
  fromValue i x = Left (NumericTypeError i x I32Type)

instance Numeric Word64 where
  type OpType Word64 = I64Op
  valueType = I64Type
  toValue = I64 . fromIntegral
  fromValue _ (I64 x) = Right (fromIntegral x)
  fromValue i x = Left (NumericTypeError i x I64Type)

instance Numeric Int64 where
  type OpType Int64 = I64Op
  valueType = I64Type
  toValue = I64
  fromValue _ (I64 x) = Right x
  fromValue i x = Left (NumericTypeError i x I64Type)

instance Numeric Float where
  type OpType Float = F32Op
  valueType = F32Type
  toValue = F32
  fromValue _ (F32 x) = Right x
  fromValue i x = Left (NumericTypeError i x F32Type)

instance Numeric Double where
  type OpType Double = F64Op
  valueType = F64Type
  toValue = F64
  fromValue _ (F64 x) = Right x
  fromValue i x = Left (NumericTypeError i x F64Type)

unaryOp :: Numeric t
        => (OpType t Unary -> t -> t) -> OpType t Unary -> Value
        -> Either NumericError Value
unaryOp f op x = toValue . f op <$> fromValue 1 x

binaryOp :: Numeric t
         => (OpType t Binary -> t -> t -> Either NumericError t)
         -> OpType t Binary
         -> Value
         -> Value
         -> Either NumericError Value
binaryOp f op x y = do
  x' <- fromValue 1 x
  y' <- fromValue 2 y
  r <- f op x' y'
  return $ toValue r

testOp :: Numeric t
       => (OpType t Test -> t -> Bool) -> OpType t Test -> Value
       -> Either NumericError Value
testOp f op x = valueOfBool . f op <$> fromValue 1 x

compareOp :: Numeric t
          => (OpType t Compare -> t -> t -> Bool)
          -> OpType t Compare
          -> Value
          -> Value
          -> Either NumericError Value
compareOp f op x y =
  (valueOfBool .) . f op <$> fromValue 1 x <*> fromValue 2 y

convertOp :: (OpType t Convert -> Value -> Either NumericError Value)
          -> OpType t Convert
          -> Value
          -> Either NumericError Value
convertOp = id

checkDiv0 :: Integral t
          => (t -> t -> t)
          -> (t -> t -> Either NumericError t)
checkDiv0 _ _ 0 = Left NumericIntegerDivideByZero
checkDiv0 op x y = Right $ op x y

checkDiv0Minus1 :: (Integral t, Bounded t)
          => (t -> t -> t)
          -> (t -> t -> Either NumericError t)
checkDiv0Minus1 _ _ 0 = Left NumericIntegerDivideByZero
checkDiv0Minus1 _ x y | x == minBound && y == -1 = Left NumericIntegerOverflow
checkDiv0Minus1 op x y = Right $ op x y


class Numeric t => IntType t where
  clz :: t -> t
  default clz :: (FiniteBits t, Num t) => t -> t
  clz = fromIntegral . countLeadingZeros

  ctz :: t -> t
  default ctz :: (FiniteBits t, Num t) => t -> t
  ctz = fromIntegral . countTrailingZeros

  popcnt :: t -> t
  default popcnt :: (Bits t, Num t) => t -> t
  popcnt = fromIntegral . popCount

  intUnOp :: IntOp n Unary -> t -> t
  intUnOp op x = case op of
    Clz -> clz x
    Ctz -> ctz x
    Popcnt -> popcnt x

  iadd :: t -> t -> t
  default iadd :: Num t => t -> t -> t
  iadd = (+)

  isub :: t -> t -> t
  default isub :: Num t => t -> t -> t
  isub = (-)

  imul :: t -> t -> t
  default imul :: Num t => t -> t -> t
  imul = (*)

  idiv_s :: t -> t -> Either NumericError t
  default idiv_s :: (Integral t, Bounded t) => t -> t -> Either NumericError t
  idiv_s = checkDiv0Minus1 quot

  idiv_u :: t -> t -> Either NumericError t
  default idiv_u :: Integral t => t -> t -> Either NumericError t
  idiv_u = checkDiv0 quot

  irem_s :: t -> t -> Either NumericError t
  default irem_s :: Integral t => t -> t -> Either NumericError t
  irem_s = checkDiv0 rem

  irem_u :: t -> t -> Either NumericError t
  default irem_u :: Integral t => t -> t -> Either NumericError t
  irem_u = checkDiv0 rem

  iand_ :: t -> t -> t
  default iand_ :: Bits t => t -> t -> t
  iand_ = (.&.)

  ior_ :: t -> t -> t
  default ior_ :: Bits t => t -> t -> t
  ior_ = (.|.)

  ixor :: t -> t -> t
  default ixor :: Bits t => t -> t -> t
  ixor = xor

  ishl :: t -> t -> t
  default ishl :: (Integral t, FiniteBits t) => t -> t -> t
  ishl x y = shiftL x (fromIntegral y `mod` finiteBitSize x)

  ishr_u :: t -> t -> t
  default ishr_u :: (Integral t, FiniteBits t) => t -> t -> t
  ishr_u x y = shiftR x (fromIntegral y `mod` finiteBitSize x)

  ishr_s :: t -> t -> t
  default ishr_s :: (Integral t, FiniteBits t) => t -> t -> t
  ishr_s x y = shiftR x (fromIntegral y `mod` finiteBitSize x)

  irotl  :: t -> t -> t
  default irotl  :: (Integral t, FiniteBits t) => t -> t -> t
  irotl x y = rotateL x (fromIntegral y `mod` finiteBitSize x)

  irotr  :: t -> t -> t
  default irotr  :: (Integral t, FiniteBits t) => t -> t -> t
  irotr x y = rotateR x (fromIntegral y `mod` finiteBitSize x)

  intBinOp :: IntOp n Binary -> t -> t -> Either NumericError t
  intBinOp op x y = case op of
    I.Add -> Right $ iadd x y
    I.Sub -> Right $ isub x y
    I.Mul -> Right $ imul x y
    DivS  -> idiv_s x y
    DivU  -> idiv_u x y
    RemS  -> irem_s x y
    RemU  -> irem_u x y
    And   -> Right $ iand_ x y
    Or    -> Right $ ior_ x y
    Xor   -> Right $ ixor x y
    Shl   -> Right $ ishl x y
    ShrS  -> Right $ ishr_s x y
    ShrU  -> Right $ ishr_u x y
    Rotl  -> Right $ irotl x y
    Rotr  -> Right $ irotr x y

  eqz :: t -> Bool
  default eqz :: (Eq t, Num t) => t -> Bool
  eqz = (== 0)

  intTestOp :: IntOp n Test -> t -> Bool
  intTestOp op x = case op of
    Eqz -> eqz x

  ieq :: t -> t -> Bool
  default ieq :: Eq t => t -> t -> Bool
  ieq = (==)

  ine :: t -> t -> Bool
  default ine :: Eq t => t -> t -> Bool
  ine = (/=)

  ilt_s :: t -> t -> Bool
  default ilt_s :: Ord t => t -> t -> Bool
  ilt_s = (<)

  ilt_u :: t -> t -> Bool
  default ilt_u :: Ord t => t -> t -> Bool
  ilt_u = (<)

  ile_s :: t -> t -> Bool
  default ile_s :: Ord t => t -> t -> Bool
  ile_s = (<=)

  ile_u :: t -> t -> Bool
  default ile_u :: Ord t => t -> t -> Bool
  ile_u = (<=)

  igt_s :: t -> t -> Bool
  default igt_s :: Ord t => t -> t -> Bool
  igt_s = (>)

  igt_u :: t -> t -> Bool
  default igt_u :: Ord t => t -> t -> Bool
  igt_u = (>)

  ige_s :: t -> t -> Bool
  default ige_s :: Ord t => t -> t -> Bool
  ige_s = (>=)

  ige_u :: t -> t -> Bool
  default ige_u :: Ord t => t -> t -> Bool
  ige_u = (>=)

  intRelOp :: IntOp n Compare -> t -> t -> Bool
  intRelOp op x y = case op of
    I.Eq -> ieq x y
    I.Ne -> ine x y
    LtS  -> ilt_s x y
    LtU  -> ilt_u x y
    GtS  -> igt_s x y
    GtU  -> igt_u x y
    LeS  -> ile_s x y
    LeU  -> ile_u x y
    GeS  -> ige_s x y
    GeU  -> ige_u x y

  intCvtOp :: IntOp n Convert -> Value -> Either NumericError Value

class Numeric t => FloatType t where
  fneg :: t -> t
  default fneg :: Num t => t -> t
  fneg = negate

  fabs :: t -> t
  default fabs :: Num t => t -> t
  fabs = abs

  fsqrt :: t -> t
  default fsqrt :: Floating t => t -> t
  fsqrt = sqrt

  fceil :: t -> t
  default fceil :: RealFrac t => t -> t
  fceil = (fromIntegral :: Integer -> t) . ceiling

  ffloor :: t -> t
  default ffloor :: RealFrac t => t -> t
  ffloor = (fromIntegral :: Integer -> t) . floor

  ftrunc :: t -> t
  default ftrunc :: RealFrac t => t -> t
  ftrunc = (fromIntegral :: Integer -> t) . truncate

  fnearest :: t -> t
  default fnearest :: RealFrac t => t -> t
  fnearest = (fromIntegral :: Integer -> t) . round

  floatUnOp :: FloatOp n Unary -> t -> t
  floatUnOp op x = case op of
    Neg     -> fneg x
    Abs     -> fabs x
    Sqrt    -> fsqrt x
    Ceil    -> fceil x
    Floor   -> ffloor x
    Trunc   -> ftrunc x
    Nearest -> fnearest x

  fadd :: t -> t -> t
  default fadd :: Num t => t -> t -> t
  fadd = (+)

  fsub :: t -> t -> t
  default fsub :: Num t => t -> t -> t
  fsub = (-)

  fmul :: t -> t -> t
  default fmul :: Num t => t -> t -> t
  fmul = (*)

  fdiv :: t -> t -> t
  default fdiv :: Fractional t => t -> t -> t
  fdiv = (/)

  fmin :: t -> t -> t
  default fmin :: Ord t => t -> t -> t
  fmin = min

  fmax :: t -> t -> t
  default fmax :: Ord t => t -> t -> t
  fmax = max

  fcopysign :: t -> t -> t
  default fcopysign :: (Ord t, Num t) => t -> t -> t
  fcopysign = \x y -> if x < 0 then - (abs y) else abs y

  floatBinOp :: FloatOp n Binary -> t -> t -> Either NumericError t
  floatBinOp op x y = case op of
    F.Add    -> Right $ fadd x y
    F.Sub    -> Right $ fsub x y
    F.Mul    -> Right $ fmul x y
    Div      -> Right $ fdiv x y
    Min      -> Right $ fmin x y
    Max      -> Right $ fmax x y
    CopySign -> Right $ fcopysign x y

  feq :: t -> t -> Bool
  default feq :: Eq t => t -> t -> Bool
  feq = (==)

  fne :: t -> t -> Bool
  default fne :: Eq t => t -> t -> Bool
  fne = (/=)

  flt :: t -> t -> Bool
  default flt :: Ord t => t -> t -> Bool
  flt = (<)

  fle :: t -> t -> Bool
  default fle :: Ord t => t -> t -> Bool
  fle = (<=)

  fgt :: t -> t -> Bool
  default fgt :: Ord t => t -> t -> Bool
  fgt = (>)

  fge :: t -> t -> Bool
  default fge :: Ord t => t -> t -> Bool
  fge = (>=)

  floatRelOp :: FloatOp n Compare -> t -> t -> Bool
  floatRelOp op x y = case op of
    F.Eq -> feq x y
    F.Ne -> fne x y
    Lt   -> flt x y
    Gt   -> fgt x y
    Le   -> fle x y
    Ge   -> fge x y

  floatCvtOp :: FloatOp n Convert -> Value -> Either NumericError Value

i32_wrap_i64 :: Int64 -> Int32
i32_wrap_i64 = fromIntegral

i32_trunc_s_f32 :: Float -> Int32
i32_trunc_s_f32 = truncate

i32_trunc_u_f32 :: Float -> Word32
i32_trunc_u_f32 = truncate

i32_trunc_s_f64 :: Double -> Int32
i32_trunc_s_f64 = truncate

i32_trunc_u_f64 :: Double -> Word32
i32_trunc_u_f64 = truncate

i32_reinterpret_f32 :: Float -> Int32
i32_reinterpret_f32 = floatToBits

i64_extend_s_i32 :: Int32 -> Int64
i64_extend_s_i32 = fromIntegral

i64_extend_u_i32 :: Word32 -> Word64
i64_extend_u_i32 = fromIntegral

i64_trunc_s_f32 :: Float -> Int64
i64_trunc_s_f32 = truncate

i64_trunc_u_f32 :: Float -> Word64
i64_trunc_u_f32 = truncate

i64_trunc_s_f64 :: Double -> Int64
i64_trunc_s_f64 = truncate

i64_trunc_u_f64 :: Double -> Word64
i64_trunc_u_f64 = truncate

i64_reinterpret_f64 :: Double -> Int64
i64_reinterpret_f64 = doubleToBits

f32_demote_f64 :: Double -> Float
f32_demote_f64 = fromRational . toRational

f32_convert_s_i32 :: Int32 -> Float
f32_convert_s_i32 = fromIntegral

f32_convert_u_i32 :: Word32 -> Float
f32_convert_u_i32 = fromIntegral

f32_convert_s_i64 :: Int64 -> Float
f32_convert_s_i64 = fromIntegral

f32_convert_u_i64 :: Word64 -> Float
f32_convert_u_i64 = fromIntegral

f32_reinterpret_i32 :: Int32 -> Float
f32_reinterpret_i32 = floatFromBits

f64_promote_f64 :: Float -> Double
f64_promote_f64 = fromRational . toRational

f64_convert_s_i32 :: Int32 -> Double
f64_convert_s_i32 = fromIntegral

f64_convert_u_i32 :: Word32 -> Double
f64_convert_u_i32 = fromIntegral

f64_convert_s_i64 :: Int64 -> Double
f64_convert_s_i64 = fromIntegral

f64_convert_u_i64 :: Word64 -> Double
f64_convert_u_i64 = fromIntegral

f64_reinterpret_i64 :: Int64 -> Double
f64_reinterpret_i64 = doubleFromBits

instance IntType Int32 where
  intCvtOp op = case op of
    WrapI64          -> fmap (toValue . i32_wrap_i64) . fromValue 1
    ExtendSI32       -> error "ExtendSI32 on Int32 has no meaning"
    ExtendUI32       -> error "ExtendUI32 on Int32 has no meaning"
    TruncSF32        -> fmap (toValue . i32_trunc_s_f32) . fromValue 1
    TruncUF32        -> fmap (toValue . i32_trunc_u_f32) . fromValue 1
    TruncSF64        -> fmap (toValue . i32_trunc_s_f64) . fromValue 1
    TruncUF64        -> fmap (toValue . i32_trunc_u_f64) . fromValue 1
    ReinterpretFloat -> fmap (toValue . i32_reinterpret_f32) . fromValue 1

  idiv_u = checkDiv0 $ \x -> fromIntegral . quot   (fromIntegral x :: Word32) . fromIntegral
  irem_u = checkDiv0 $ \x -> fromIntegral . rem    (fromIntegral x :: Word32) . fromIntegral
  ishr_u x y = fromIntegral $
    shiftR (fromIntegral x :: Word32) (fromIntegral y `mod` finiteBitSize x)

  ilt_u x y = (fromIntegral x :: Word32) < (fromIntegral y :: Word32)
  igt_u x y = (fromIntegral x :: Word32) > (fromIntegral y :: Word32)
  ile_u x y = (fromIntegral x :: Word32) <= (fromIntegral y :: Word32)
  ige_u x y = (fromIntegral x :: Word32) >= (fromIntegral y :: Word32)

instance IntType Word32 where
  intCvtOp op = case op of
    WrapI64          -> fmap (toValue . i32_wrap_i64) . fromValue 1
    ExtendSI32       -> error "ExtendSI32 on Word32 has no meaning"
    ExtendUI32       -> error "ExtendSI32 on Word32 has no meaning"
    TruncSF32        -> fmap (toValue . i32_trunc_s_f32) . fromValue 1
    TruncUF32        -> fmap (toValue . i32_trunc_u_f32) . fromValue 1
    TruncSF64        -> fmap (toValue . i32_trunc_s_f64) . fromValue 1
    TruncUF64        -> fmap (toValue . i32_trunc_u_f64) . fromValue 1
    ReinterpretFloat -> fmap (toValue . i32_reinterpret_f32) . fromValue 1

  idiv_s = checkDiv0Minus1 $ \x -> fromIntegral . quot   (fromIntegral x :: Int32) . fromIntegral
  irem_s = checkDiv0 $ \x -> fromIntegral . rem    (fromIntegral x :: Int32) . fromIntegral
  ishr_s x y = fromIntegral $
    shiftR (fromIntegral x :: Word32) (fromIntegral y `mod` finiteBitSize x)

  ilt_s x y = (fromIntegral x :: Int32) < (fromIntegral y :: Int32)
  igt_s x y = (fromIntegral x :: Int32) > (fromIntegral y :: Int32)
  ile_s x y = (fromIntegral x :: Int32) <= (fromIntegral y :: Int32)
  ige_s x y = (fromIntegral x :: Int32) >= (fromIntegral y :: Int32)

instance IntType Int64 where
  intCvtOp op = case op of
    WrapI64          -> error "WrapI64 on Int64 has no meaning"
    ExtendSI32       -> fmap (toValue . i64_extend_s_i32) . fromValue 1
    ExtendUI32       -> fmap (toValue . i64_extend_u_i32) . fromValue 1
    TruncSF32        -> fmap (toValue . i64_trunc_s_f32) . fromValue 1
    TruncUF32        -> fmap (toValue . i64_trunc_u_f32) . fromValue 1
    TruncSF64        -> fmap (toValue . i64_trunc_s_f64) . fromValue 1
    TruncUF64        -> fmap (toValue . i64_trunc_u_f64) . fromValue 1
    ReinterpretFloat -> fmap (toValue . i64_reinterpret_f64) . fromValue 1

  idiv_u = checkDiv0 $ \x -> fromIntegral . quot   (fromIntegral x :: Word64) . fromIntegral
  irem_u = checkDiv0 $ \x -> fromIntegral . rem    (fromIntegral x :: Word64) . fromIntegral
  ishr_u x y = fromIntegral $
    shiftR (fromIntegral x :: Word64) (fromIntegral y `mod` finiteBitSize x)

  ilt_u x y = (fromIntegral x :: Word64) < (fromIntegral y :: Word64)
  igt_u x y = (fromIntegral x :: Word64) > (fromIntegral y :: Word64)
  ile_u x y = (fromIntegral x :: Word64) <= (fromIntegral y :: Word64)
  ige_u x y = (fromIntegral x :: Word64) >= (fromIntegral y :: Word64)

instance IntType Word64 where
  intCvtOp op = case op of
    WrapI64          -> error "WrapI64 on Word64 has no meaning"
    ExtendSI32       -> fmap (toValue . i64_extend_s_i32) . fromValue 1
    ExtendUI32       -> fmap (toValue . i64_extend_u_i32) . fromValue 1
    TruncSF32        -> fmap (toValue . i64_trunc_s_f32) . fromValue 1
    TruncUF32        -> fmap (toValue . i64_trunc_u_f32) . fromValue 1
    TruncSF64        -> fmap (toValue . i64_trunc_s_f64) . fromValue 1
    TruncUF64        -> fmap (toValue . i64_trunc_u_f64) . fromValue 1
    ReinterpretFloat -> fmap (toValue . i64_reinterpret_f64) . fromValue 1

  idiv_s = checkDiv0Minus1 $ \x -> fromIntegral . quot   (fromIntegral x :: Int64) . fromIntegral
  irem_s = checkDiv0 $ \x -> fromIntegral . rem    (fromIntegral x :: Int64) . fromIntegral
  ishr_s x y = fromIntegral $
    shiftR (fromIntegral x :: Word64) (fromIntegral y `mod` finiteBitSize x)

  ilt_s x y = (fromIntegral x :: Int64) < (fromIntegral y :: Int64)
  igt_s x y = (fromIntegral x :: Int64) > (fromIntegral y :: Int64)
  ile_s x y = (fromIntegral x :: Int64) <= (fromIntegral y :: Int64)
  ige_s x y = (fromIntegral x :: Int64) >= (fromIntegral y :: Int64)

instance FloatType Float where
  floatCvtOp op = case op of
    DemoteF64      -> fmap (toValue . f32_demote_f64) . fromValue 1
    PromoteF32     -> error "PromoteF32 on Float has no meaning"
    ConvertSI32    -> fmap (toValue . f32_convert_s_i32) . fromValue 1
    ConvertUI32    -> fmap (toValue . f32_convert_u_i32) . fromValue 1
    ConvertSI64    -> fmap (toValue . f32_convert_s_i64) . fromValue 1
    ConvertUI64    -> fmap (toValue . f32_convert_u_i64) . fromValue 1
    ReinterpretInt -> fmap (toValue . f32_reinterpret_i32) . fromValue 1

instance FloatType Double where
  floatCvtOp op = case op of
    DemoteF64      -> error "DemoteF64 on Double has no meaning"
    PromoteF32     -> fmap (toValue . f64_promote_f64) . fromValue 1
    ConvertSI32    -> fmap (toValue . f64_convert_s_i32) . fromValue 1
    ConvertUI32    -> fmap (toValue . f64_convert_u_i32) . fromValue 1
    ConvertSI64    -> fmap (toValue . f64_convert_s_i64) . fromValue 1
    ConvertUI64    -> fmap (toValue . f64_convert_u_i64) . fromValue 1
    ReinterpretInt -> fmap (toValue . f64_reinterpret_i64) . fromValue 1
