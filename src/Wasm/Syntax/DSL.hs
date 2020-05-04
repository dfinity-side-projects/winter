{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Wasm.Syntax.DSL where

import Control.Monad.Writer
import Data.Fix
import Data.Int
import Data.Void

import Wasm.Syntax.AST
import Wasm.Syntax.Values
import Wasm.Syntax.Ops
import Wasm.Syntax.Ops.Float as F
import Wasm.Syntax.Ops.Int as I
import Wasm.Syntax.Types

class Monad (WasmM t) => Wasm t where
  data WasmM t :: * -> *

  -- Control flow operators.
  unreachable :: WasmM t Void
  nop :: WasmM t ()
  -- This only allows pre-multi-value blocks
  block :: Maybe ValueType -> WasmM t a -> WasmM t ()
  loop :: Maybe ValueType -> WasmM t a -> WasmM t ()
  if_ :: Maybe ValueType -> WasmM t a -> WasmM t ()
  if_else_ :: Maybe ValueType -> WasmM t a -> WasmM t a -> WasmM t ()
  br :: Int -> WasmM t ()
  br_if :: Int -> WasmM t ()
  br_table :: [Int] -> Int -> WasmM t ()
  return_ :: WasmM t ()

  -- Call operators.
  call :: Int -> WasmM t ()
  call_indirect :: Int -> WasmM t ()

  -- Parametric operators.
  drop :: WasmM t ()
  select :: WasmM t ()

  -- Variable access.
  get_local :: Int -> WasmM t ()
  set_local :: Int -> WasmM t ()
  tee_local :: Int -> WasmM t ()
  get_global :: Int -> WasmM t ()
  set_global :: Int -> WasmM t ()

  -- Load operators.
  i32_load :: Int -> Int -> WasmM t ()
  i64_load :: Int -> Int -> WasmM t ()
  f32_load :: Int -> Int -> WasmM t ()
  f64_load :: Int -> Int -> WasmM t ()
  i32_load8_s :: Int -> Int -> WasmM t ()
  i32_load8_u :: Int -> Int -> WasmM t ()
  i32_load16_s :: Int -> Int -> WasmM t ()
  i32_load16_u :: Int -> Int -> WasmM t ()
  i64_load8_s :: Int -> Int -> WasmM t ()
  i64_load8_u :: Int -> Int -> WasmM t ()
  i64_load16_s :: Int -> Int -> WasmM t ()
  i64_load16_u :: Int -> Int -> WasmM t ()
  i64_load32_s :: Int -> Int -> WasmM t ()
  i64_load32_u :: Int -> Int -> WasmM t ()

  -- Store operators.
  i32_store :: Int -> Int -> WasmM t ()
  i64_store :: Int -> Int -> WasmM t ()
  f32_store :: Int -> Int -> WasmM t ()
  f64_store :: Int -> Int -> WasmM t ()
  i32_store8 :: Int -> Int -> WasmM t ()
  i32_store16 :: Int -> Int -> WasmM t ()
  i64_store8 :: Int -> Int -> WasmM t ()
  i64_store16 :: Int -> Int -> WasmM t ()
  i64_store32 :: Int -> Int -> WasmM t ()

  -- Memory operators.
  memory_size :: WasmM t ()
  memory_grow :: WasmM t ()

  -- Constants.
  i32_const :: Int32 -> WasmM t ()
  i64_const :: Int64 -> WasmM t ()
  f32_const :: Float -> WasmM t ()
  f64_const :: Double -> WasmM t ()

  -- Unary operators for 32-bit integers.
  i32_clz :: WasmM t ()
  i32_ctz :: WasmM t ()
  i32_popcnt :: WasmM t ()

  -- Unary operators for 64-bit integers.
  i64_clz :: WasmM t ()
  i64_ctz :: WasmM t ()
  i64_popcnt :: WasmM t ()

  -- Unary operators for 32-bit floats.
  f32_abs :: WasmM t ()
  f32_neg :: WasmM t ()
  f32_ceil :: WasmM t ()
  f32_floor :: WasmM t ()
  f32_trunc :: WasmM t ()
  f32_nearest :: WasmM t ()
  f32_sqrt :: WasmM t ()

  -- Unary operators for 64-bit floats.
  f64_abs :: WasmM t ()
  f64_neg :: WasmM t ()
  f64_ceil :: WasmM t ()
  f64_floor :: WasmM t ()
  f64_trunc :: WasmM t ()
  f64_nearest :: WasmM t ()
  f64_sqrt :: WasmM t ()

  -- Binary operators for 32-bit integers.
  i32_add :: WasmM t ()
  i32_sub :: WasmM t ()
  i32_mul :: WasmM t ()
  i32_div_s :: WasmM t ()
  i32_div_u :: WasmM t ()
  i32_rem_s :: WasmM t ()
  i32_rem_u :: WasmM t ()
  i32_and :: WasmM t ()
  i32_or :: WasmM t ()
  i32_xor :: WasmM t ()
  i32_shl :: WasmM t ()
  i32_shr_s :: WasmM t ()
  i32_shr_u :: WasmM t ()
  i32_rotl :: WasmM t ()
  i32_rotr :: WasmM t ()

  -- Binary operators for 64-bit integers.
  i64_add :: WasmM t ()
  i64_sub :: WasmM t ()
  i64_mul :: WasmM t ()
  i64_div_s :: WasmM t ()
  i64_div_u :: WasmM t ()
  i64_rem_s :: WasmM t ()
  i64_rem_u :: WasmM t ()
  i64_and :: WasmM t ()
  i64_or :: WasmM t ()
  i64_xor :: WasmM t ()
  i64_shl :: WasmM t ()
  i64_shr_s :: WasmM t ()
  i64_shr_u :: WasmM t ()
  i64_rotl :: WasmM t ()
  i64_rotr :: WasmM t ()

  -- Binary operators for 32-bit floats.
  f32_add :: WasmM t ()
  f32_sub :: WasmM t ()
  f32_mul :: WasmM t ()
  f32_div :: WasmM t ()
  f32_min :: WasmM t ()
  f32_max :: WasmM t ()
  f32_copysign :: WasmM t ()

  -- Binary operators for 64-bit floats.
  f64_add :: WasmM t ()
  f64_sub :: WasmM t ()
  f64_mul :: WasmM t ()
  f64_div :: WasmM t ()
  f64_min :: WasmM t ()
  f64_max :: WasmM t ()
  f64_copysign :: WasmM t ()

  -- Test operators for 32-bit integers.
  i32_eqz :: WasmM t ()

  -- Test operators for 64-bit integers.
  i64_eqz :: WasmM t ()

  -- Comparison operators for 32-bit integers.
  i32_eq :: WasmM t ()
  i32_ne :: WasmM t ()
  i32_lt_s :: WasmM t ()
  i32_lt_u :: WasmM t ()
  i32_gt_s :: WasmM t ()
  i32_gt_u :: WasmM t ()
  i32_le_s :: WasmM t ()
  i32_le_u :: WasmM t ()
  i32_ge_s :: WasmM t ()
  i32_ge_u :: WasmM t ()

  -- Comparison operators for 64-bit integers.
  i64_eq :: WasmM t ()
  i64_ne :: WasmM t ()
  i64_lt_s :: WasmM t ()
  i64_lt_u :: WasmM t ()
  i64_gt_s :: WasmM t ()
  i64_gt_u :: WasmM t ()
  i64_le_s :: WasmM t ()
  i64_le_u :: WasmM t ()
  i64_ge_s :: WasmM t ()
  i64_ge_u :: WasmM t ()

  -- Comparison operators for 32-bit floats.
  f32_eq :: WasmM t ()
  f32_ne :: WasmM t ()
  f32_lt :: WasmM t ()
  f32_gt :: WasmM t ()
  f32_le :: WasmM t ()
  f32_ge :: WasmM t ()

  -- Comparison operators for 64-bit floats.
  f64_eq :: WasmM t ()
  f64_ne :: WasmM t ()
  f64_lt :: WasmM t ()
  f64_gt :: WasmM t ()
  f64_le :: WasmM t ()
  f64_ge :: WasmM t ()

  -- Conversion operators for 32-bit integers.
  i32_wrap_i64 :: WasmM t ()
  i32_trunc_s_f32 :: WasmM t ()
  i32_trunc_u_f32 :: WasmM t ()
  i32_trunc_s_f64 :: WasmM t ()
  i32_trunc_u_f64 :: WasmM t ()
  i32_reinterpret_float :: WasmM t ()

  -- Conversion operators for 64-bit integers.
  i64_extend_s_i32 :: WasmM t ()
  i64_extend_u_i32 :: WasmM t ()
  i64_trunc_s_f32 :: WasmM t ()
  i64_trunc_u_f32 :: WasmM t ()
  i64_trunc_s_f64 :: WasmM t ()
  i64_trunc_u_f64 :: WasmM t ()
  i64_reinterpret_float :: WasmM t ()

  -- Conversion operators for 32-bit floats.
  f32_convert_s_i32 :: WasmM t ()
  f32_convert_u_i32 :: WasmM t ()
  f32_convert_s_i64 :: WasmM t ()
  f32_convert_u_i64 :: WasmM t ()
  f32_demote_f64 :: WasmM t ()
  f32_reinterpret_int :: WasmM t ()

  -- Conversion operators for 64-bit floats.
  f64_convert_s_i32 :: WasmM t ()
  f64_convert_u_i32 :: WasmM t ()
  f64_convert_s_i64 :: WasmM t ()
  f64_convert_u_i64 :: WasmM t ()
  f64_promote_f32 :: WasmM t ()
  f64_reinterpret_int :: WasmM t ()

instance Applicative f => Wasm [Instr f] where
  newtype WasmM [Instr f] a = WasmM { runWasmM :: Writer [Instr f] a }
    deriving (Functor, Applicative, Monad)

  -- Control flow operators.
  unreachable    = WasmM $ error "unreachable!" <$ tell [Fix Unreachable]
  nop            = WasmM $ tell [Fix Nop]
  block l b      = WasmM $ tell [Fix $ Block (ValBlockType l) (map pure (execWriter (runWasmM b)))]
  loop l b       = WasmM $ tell [Fix $ Loop (ValBlockType l) (map pure (execWriter (runWasmM b)))]
  if_ l x        = WasmM $ tell [Fix $ If (ValBlockType l) (map pure (execWriter (runWasmM x))) [pure (Fix Nop)]]
  if_else_ l x y = WasmM $ tell [Fix $ If (ValBlockType l) (map pure (execWriter (runWasmM x)))
                              (map pure (execWriter (runWasmM y)))]
  br n           = WasmM $ tell [Fix $ Br (pure n)]
  br_if n        = WasmM $ tell [Fix $ BrIf (pure n)]
  br_table ns n  = WasmM $ tell [Fix $ BrTable (map pure ns) (pure n)]
  return_        = WasmM $ tell [Fix $ Return]

  -- Call operators.
  call n          = WasmM $ tell [Fix $ Call (pure n)]
  call_indirect n = WasmM $ tell [Fix $ CallIndirect (pure n)]

  -- Parametric operators.
  drop   = WasmM $ tell [Fix Drop]
  select = WasmM $ tell [Fix Select]

  -- Variable access.
  get_local n  = WasmM $ tell [Fix $ GetLocal (pure n)]
  set_local n  = WasmM $ tell [Fix $ SetLocal (pure n)]
  tee_local n  = WasmM $ tell [Fix $ TeeLocal (pure n)]
  get_global n = WasmM $ tell [Fix $ GetGlobal (pure n)]
  set_global n = WasmM $ tell [Fix $ SetGlobal (pure n)]

  -- Load operators.
  i32_load x y     = WasmM $ tell [Fix $ Load (MemoryOp I32Type x y Nothing)]
  i64_load x y     = WasmM $ tell [Fix $ Load (MemoryOp I64Type x y Nothing)]
  f32_load x y     = WasmM $ tell [Fix $ Load (MemoryOp F32Type x y Nothing)]
  f64_load x y     = WasmM $ tell [Fix $ Load (MemoryOp F64Type x y Nothing)]
  i32_load8_s x y  = WasmM $ tell [Fix $ Load (MemoryOp I32Type x y (Just (Pack8, SX)))]
  i32_load8_u x y  = WasmM $ tell [Fix $ Load (MemoryOp I32Type x y (Just (Pack8, ZX)))]
  i32_load16_s x y = WasmM $ tell [Fix $ Load (MemoryOp I32Type x y (Just (Pack16, SX)))]
  i32_load16_u x y = WasmM $ tell [Fix $ Load (MemoryOp I32Type x y (Just (Pack16, ZX)))]
  i64_load8_s x y  = WasmM $ tell [Fix $ Load (MemoryOp I64Type x y (Just (Pack8, SX)))]
  i64_load8_u x y  = WasmM $ tell [Fix $ Load (MemoryOp I64Type x y (Just (Pack8, ZX)))]
  i64_load16_s x y = WasmM $ tell [Fix $ Load (MemoryOp I64Type x y (Just (Pack16, SX)))]
  i64_load16_u x y = WasmM $ tell [Fix $ Load (MemoryOp I64Type x y (Just (Pack16, ZX)))]
  i64_load32_s x y = WasmM $ tell [Fix $ Load (MemoryOp I64Type x y (Just (Pack32, SX)))]
  i64_load32_u x y = WasmM $ tell [Fix $ Load (MemoryOp I64Type x y (Just (Pack32, ZX)))]

  -- Store operators.
  i32_store x y   = WasmM $ tell [Fix $ Store (MemoryOp I32Type x y Nothing)]
  i64_store x y   = WasmM $ tell [Fix $ Store (MemoryOp I64Type x y Nothing)]
  f32_store x y   = WasmM $ tell [Fix $ Store (MemoryOp F32Type x y Nothing)]
  f64_store x y   = WasmM $ tell [Fix $ Store (MemoryOp F64Type x y Nothing)]
  i32_store8 x y  = WasmM $ tell [Fix $ Store (MemoryOp I32Type x y (Just Pack8))]
  i32_store16 x y = WasmM $ tell [Fix $ Store (MemoryOp I32Type x y (Just Pack16))]
  i64_store8 x y  = WasmM $ tell [Fix $ Store (MemoryOp I64Type x y (Just Pack8))]
  i64_store16 x y = WasmM $ tell [Fix $ Store (MemoryOp I64Type x y (Just Pack16))]
  i64_store32 x y = WasmM $ tell [Fix $ Store (MemoryOp I64Type x y (Just Pack32))]

  -- Memory operators.
  memory_size = WasmM $ tell [Fix $ MemorySize]
  memory_grow = WasmM $ tell [Fix $ MemoryGrow]

  -- Constants.
  i32_const x = WasmM $ tell [Fix $ Const (pure (I32 x))]
  i64_const x = WasmM $ tell [Fix $ Const (pure (I64 x))]
  f32_const x = WasmM $ tell [Fix $ Const (pure (F32 x))]
  f64_const x = WasmM $ tell [Fix $ Const (pure (F64 x))]

  -- Unary operators for 32-bit integers.
  i32_clz    = WasmM $ tell [Fix $ Unary (I32UnaryOp Clz)]
  i32_ctz    = WasmM $ tell [Fix $ Unary (I32UnaryOp Ctz)]
  i32_popcnt = WasmM $ tell [Fix $ Unary (I32UnaryOp Popcnt)]

  -- Unary operators for 64-bit integers.
  i64_clz    = WasmM $ tell [Fix $ Unary (I64UnaryOp Clz)]
  i64_ctz    = WasmM $ tell [Fix $ Unary (I64UnaryOp Ctz)]
  i64_popcnt = WasmM $ tell [Fix $ Unary (I64UnaryOp Popcnt)]

  -- Unary operators for 32-bit floats.
  f32_abs     = WasmM $ tell [Fix $ Unary (F32UnaryOp Abs)]
  f32_neg     = WasmM $ tell [Fix $ Unary (F32UnaryOp Neg)]
  f32_ceil    = WasmM $ tell [Fix $ Unary (F32UnaryOp Ceil)]
  f32_floor   = WasmM $ tell [Fix $ Unary (F32UnaryOp Floor)]
  f32_trunc   = WasmM $ tell [Fix $ Unary (F32UnaryOp Trunc)]
  f32_nearest = WasmM $ tell [Fix $ Unary (F32UnaryOp Nearest)]
  f32_sqrt    = WasmM $ tell [Fix $ Unary (F32UnaryOp Sqrt)]

  -- Unary operators for 64-bit floats.
  f64_abs     = WasmM $ tell [Fix $ Unary (F64UnaryOp Abs)]
  f64_neg     = WasmM $ tell [Fix $ Unary (F64UnaryOp Neg)]
  f64_ceil    = WasmM $ tell [Fix $ Unary (F64UnaryOp Ceil)]
  f64_floor   = WasmM $ tell [Fix $ Unary (F64UnaryOp Floor)]
  f64_trunc   = WasmM $ tell [Fix $ Unary (F64UnaryOp Trunc)]
  f64_nearest = WasmM $ tell [Fix $ Unary (F64UnaryOp Nearest)]
  f64_sqrt    = WasmM $ tell [Fix $ Unary (F64UnaryOp Sqrt)]

  -- Binary operators for 32-bit integers.
  i32_add   = WasmM $ tell [Fix $ Binary (I32BinaryOp I.Add)]
  i32_sub   = WasmM $ tell [Fix $ Binary (I32BinaryOp I.Sub)]
  i32_mul   = WasmM $ tell [Fix $ Binary (I32BinaryOp I.Mul)]
  i32_div_s = WasmM $ tell [Fix $ Binary (I32BinaryOp DivS)]
  i32_div_u = WasmM $ tell [Fix $ Binary (I32BinaryOp DivU)]
  i32_rem_s = WasmM $ tell [Fix $ Binary (I32BinaryOp RemS)]
  i32_rem_u = WasmM $ tell [Fix $ Binary (I32BinaryOp RemU)]
  i32_and   = WasmM $ tell [Fix $ Binary (I32BinaryOp And)]
  i32_or    = WasmM $ tell [Fix $ Binary (I32BinaryOp Or)]
  i32_xor   = WasmM $ tell [Fix $ Binary (I32BinaryOp Xor)]
  i32_shl   = WasmM $ tell [Fix $ Binary (I32BinaryOp Shl)]
  i32_shr_s = WasmM $ tell [Fix $ Binary (I32BinaryOp ShrS)]
  i32_shr_u = WasmM $ tell [Fix $ Binary (I32BinaryOp ShrU)]
  i32_rotl  = WasmM $ tell [Fix $ Binary (I32BinaryOp Rotl)]
  i32_rotr  = WasmM $ tell [Fix $ Binary (I32BinaryOp Rotr)]

  -- Binary operators for 64-bit integers.
  i64_add   = WasmM $ tell [Fix $ Binary (I64BinaryOp I.Add)]
  i64_sub   = WasmM $ tell [Fix $ Binary (I64BinaryOp I.Sub)]
  i64_mul   = WasmM $ tell [Fix $ Binary (I64BinaryOp I.Mul)]
  i64_div_s = WasmM $ tell [Fix $ Binary (I64BinaryOp DivS)]
  i64_div_u = WasmM $ tell [Fix $ Binary (I64BinaryOp DivU)]
  i64_rem_s = WasmM $ tell [Fix $ Binary (I64BinaryOp RemS)]
  i64_rem_u = WasmM $ tell [Fix $ Binary (I64BinaryOp RemU)]
  i64_and   = WasmM $ tell [Fix $ Binary (I64BinaryOp And)]
  i64_or    = WasmM $ tell [Fix $ Binary (I64BinaryOp Or)]
  i64_xor   = WasmM $ tell [Fix $ Binary (I64BinaryOp Xor)]
  i64_shl   = WasmM $ tell [Fix $ Binary (I64BinaryOp Shl)]
  i64_shr_s = WasmM $ tell [Fix $ Binary (I64BinaryOp ShrS)]
  i64_shr_u = WasmM $ tell [Fix $ Binary (I64BinaryOp ShrU)]
  i64_rotl  = WasmM $ tell [Fix $ Binary (I64BinaryOp Rotl)]
  i64_rotr  = WasmM $ tell [Fix $ Binary (I64BinaryOp Rotr)]

  -- Binary operators for 32-bit floats.
  f32_add      = WasmM $ tell [Fix $ Binary (F32BinaryOp F.Add)]
  f32_sub      = WasmM $ tell [Fix $ Binary (F32BinaryOp F.Sub)]
  f32_mul      = WasmM $ tell [Fix $ Binary (F32BinaryOp F.Mul)]
  f32_div      = WasmM $ tell [Fix $ Binary (F32BinaryOp Div)]
  f32_min      = WasmM $ tell [Fix $ Binary (F32BinaryOp Min)]
  f32_max      = WasmM $ tell [Fix $ Binary (F32BinaryOp Max)]
  f32_copysign = WasmM $ tell [Fix $ Binary (F32BinaryOp CopySign)]

  -- Binary operators for 64-bit floats.
  f64_add      = WasmM $ tell [Fix $ Binary (F64BinaryOp F.Add)]
  f64_sub      = WasmM $ tell [Fix $ Binary (F64BinaryOp F.Sub)]
  f64_mul      = WasmM $ tell [Fix $ Binary (F64BinaryOp F.Mul)]
  f64_div      = WasmM $ tell [Fix $ Binary (F64BinaryOp Div)]
  f64_min      = WasmM $ tell [Fix $ Binary (F64BinaryOp Min)]
  f64_max      = WasmM $ tell [Fix $ Binary (F64BinaryOp Max)]
  f64_copysign = WasmM $ tell [Fix $ Binary (F64BinaryOp CopySign)]

  -- Test operators for 32-bit integers.
  i32_eqz = WasmM $ tell [Fix $ Test (I32TestOp Eqz)]

  -- Test operators for 64-bit integers.
  i64_eqz = WasmM $ tell [Fix $ Test (I64TestOp Eqz)]

  -- Comparison operators for 32-bit integers.
  i32_eq   = WasmM $ tell [Fix $ Compare (I32CompareOp I.Eq)]
  i32_ne   = WasmM $ tell [Fix $ Compare (I32CompareOp I.Ne)]
  i32_lt_s = WasmM $ tell [Fix $ Compare (I32CompareOp LtS)]
  i32_lt_u = WasmM $ tell [Fix $ Compare (I32CompareOp LtU)]
  i32_gt_s = WasmM $ tell [Fix $ Compare (I32CompareOp GtS)]
  i32_gt_u = WasmM $ tell [Fix $ Compare (I32CompareOp GtU)]
  i32_le_s = WasmM $ tell [Fix $ Compare (I32CompareOp LeS)]
  i32_le_u = WasmM $ tell [Fix $ Compare (I32CompareOp LeU)]
  i32_ge_s = WasmM $ tell [Fix $ Compare (I32CompareOp GeS)]
  i32_ge_u = WasmM $ tell [Fix $ Compare (I32CompareOp GeU)]

  -- Comparison operators for 64-bit integers.
  i64_eq   = WasmM $ tell [Fix $ Compare (I64CompareOp I.Eq)]
  i64_ne   = WasmM $ tell [Fix $ Compare (I64CompareOp I.Ne)]
  i64_lt_s = WasmM $ tell [Fix $ Compare (I64CompareOp LtS)]
  i64_lt_u = WasmM $ tell [Fix $ Compare (I64CompareOp LtU)]
  i64_gt_s = WasmM $ tell [Fix $ Compare (I64CompareOp GtS)]
  i64_gt_u = WasmM $ tell [Fix $ Compare (I64CompareOp GtU)]
  i64_le_s = WasmM $ tell [Fix $ Compare (I64CompareOp LeS)]
  i64_le_u = WasmM $ tell [Fix $ Compare (I64CompareOp LeU)]
  i64_ge_s = WasmM $ tell [Fix $ Compare (I64CompareOp GeS)]
  i64_ge_u = WasmM $ tell [Fix $ Compare (I64CompareOp GeU)]

  -- Comparison operators for 32-bit floats.
  f32_eq = WasmM $ tell [Fix $ Compare (F32CompareOp F.Eq)]
  f32_ne = WasmM $ tell [Fix $ Compare (F32CompareOp F.Ne)]
  f32_lt = WasmM $ tell [Fix $ Compare (F32CompareOp Lt)]
  f32_gt = WasmM $ tell [Fix $ Compare (F32CompareOp Gt)]
  f32_le = WasmM $ tell [Fix $ Compare (F32CompareOp Le)]
  f32_ge = WasmM $ tell [Fix $ Compare (F32CompareOp Ge)]

  -- Comparison operators for 64-bit floats.
  f64_eq = WasmM $ tell [Fix $ Compare (F64CompareOp F.Eq)]
  f64_ne = WasmM $ tell [Fix $ Compare (F64CompareOp F.Ne)]
  f64_lt = WasmM $ tell [Fix $ Compare (F64CompareOp Lt)]
  f64_gt = WasmM $ tell [Fix $ Compare (F64CompareOp Gt)]
  f64_le = WasmM $ tell [Fix $ Compare (F64CompareOp Le)]
  f64_ge = WasmM $ tell [Fix $ Compare (F64CompareOp Ge)]

  -- Conversion operators for 32-bit integers.
  i32_wrap_i64          = WasmM $ tell [Fix $ Convert (I32ConvertOp WrapI64)]
  i32_trunc_s_f32       = WasmM $ tell [Fix $ Convert (I32ConvertOp TruncSF32)]
  i32_trunc_u_f32       = WasmM $ tell [Fix $ Convert (I32ConvertOp TruncUF32)]
  i32_trunc_s_f64       = WasmM $ tell [Fix $ Convert (I32ConvertOp TruncSF64)]
  i32_trunc_u_f64       = WasmM $ tell [Fix $ Convert (I32ConvertOp TruncUF64)]
  i32_reinterpret_float = WasmM $ tell [Fix $ Convert (I32ConvertOp ReinterpretFloat)]

  -- Conversion operators for 64-bit integers.
  i64_extend_s_i32      = WasmM $ tell [Fix $ Convert (I64ConvertOp ExtendSI32)]
  i64_extend_u_i32      = WasmM $ tell [Fix $ Convert (I64ConvertOp ExtendUI32)]
  i64_trunc_s_f32       = WasmM $ tell [Fix $ Convert (I64ConvertOp TruncSF32)]
  i64_trunc_u_f32       = WasmM $ tell [Fix $ Convert (I64ConvertOp TruncUF32)]
  i64_trunc_s_f64       = WasmM $ tell [Fix $ Convert (I64ConvertOp TruncSF64)]
  i64_trunc_u_f64       = WasmM $ tell [Fix $ Convert (I64ConvertOp TruncUF64)]
  i64_reinterpret_float = WasmM $ tell [Fix $ Convert (I64ConvertOp ReinterpretFloat)]

  -- Conversion operators for 32-bit floats.
  f32_convert_s_i32   = WasmM $ tell [Fix $ Convert (F32ConvertOp ConvertSI32)]
  f32_convert_u_i32   = WasmM $ tell [Fix $ Convert (F32ConvertOp ConvertUI32)]
  f32_convert_s_i64   = WasmM $ tell [Fix $ Convert (F32ConvertOp ConvertSI64)]
  f32_convert_u_i64   = WasmM $ tell [Fix $ Convert (F32ConvertOp ConvertUI64)]
  f32_demote_f64      = WasmM $ tell [Fix $ Convert (F32ConvertOp DemoteF64)]
  f32_reinterpret_int = WasmM $ tell [Fix $ Convert (F32ConvertOp ReinterpretInt)]

  -- Conversion operators for 64-bit floats.
  f64_convert_s_i32   = WasmM $ tell [Fix $ Convert (F64ConvertOp ConvertSI32)]
  f64_convert_u_i32   = WasmM $ tell [Fix $ Convert (F64ConvertOp ConvertUI32)]
  f64_convert_s_i64   = WasmM $ tell [Fix $ Convert (F64ConvertOp ConvertSI64)]
  f64_convert_u_i64   = WasmM $ tell [Fix $ Convert (F64ConvertOp ConvertUI64)]
  f64_promote_f32     = WasmM $ tell [Fix $ Convert (F64ConvertOp PromoteF32)]
  f64_reinterpret_int = WasmM $ tell [Fix $ Convert (F64ConvertOp ReinterpretInt)]
