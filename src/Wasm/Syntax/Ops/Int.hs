{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Wasm.Syntax.Ops.Int where

import           Control.DeepSeq
import           Data.Kind (Type)
import           GHC.Generics
import           GHC.TypeLits

import           Wasm.Syntax.Types
import           Wasm.Syntax.Ops.Kind

data family IntOp :: Nat -> Type -> Type

data instance IntOp bits Unary
  = Clz | Ctz | Popcnt | ExtendS PackSize
  deriving (Generic, NFData, Show)

data instance IntOp bits Binary
  = Add | Sub | Mul | DivS | DivU | RemS | RemU | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
  deriving (Generic, NFData, Show)

data instance IntOp bits Test
  = Eqz
  deriving (Generic, NFData, Show)

data instance IntOp bits Compare
  = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  deriving (Generic, NFData, Show)

data instance IntOp bits Convert
  = ExtendSI32 | ExtendUI32
  | WrapI64
  | TruncSF32 | TruncUF32 | TruncSF64 | TruncUF64
  | TruncSSatF32 | TruncUSatF32 | TruncSSatF64 | TruncUSatF64
  | ReinterpretFloat
  deriving (Generic, NFData, Show)

type I32Op = IntOp 32

type I64Op = IntOp 64
