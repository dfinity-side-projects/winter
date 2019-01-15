{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Wasm.Syntax.Ops.Float where

import           Control.DeepSeq
import           GHC.Generics
import           GHC.TypeLits

import           Wasm.Syntax.Ops.Kind

data family FloatOp :: Nat -> * -> *

data instance FloatOp bits Unary
  = Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt
  deriving (Generic, NFData, Show)

data instance FloatOp bits Binary
  = Add | Sub | Mul | Div | Min | Max | CopySign
  deriving (Generic, NFData, Show)

data instance FloatOp bits Compare
  = Eq | Ne | Lt | Gt | Le | Ge
  deriving (Generic, NFData, Show)

data instance FloatOp bits Convert
  = ConvertSI32 | ConvertUI32 | ConvertSI64 | ConvertUI64 | PromoteF32 | DemoteF64 | ReinterpretInt
  deriving (Generic, NFData, Show)

type F32Op = FloatOp 32

type F64Op = FloatOp 64
