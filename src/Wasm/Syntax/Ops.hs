{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Syntax.Ops where

import           Control.DeepSeq
import           GHC.Generics
import           Lens.Micro.Platform

import           Wasm.Syntax.Memory
import           Wasm.Syntax.Ops.Float
import           Wasm.Syntax.Ops.Int
import           Wasm.Syntax.Ops.Kind
import           Wasm.Syntax.Types

data UnaryOp
  = I32UnaryOp (I32Op Unary)
  | I64UnaryOp (I64Op Unary)
  | F32UnaryOp (F32Op Unary)
  | F64UnaryOp (F64Op Unary)
  deriving (Generic, NFData, Show)

data BinaryOp
  = I32BinaryOp (I32Op Binary)
  | I64BinaryOp (I64Op Binary)
  | F32BinaryOp (F32Op Binary)
  | F64BinaryOp (F64Op Binary)
  deriving (Generic, NFData, Show)

data TestOp
  = I32TestOp (I32Op Test)
  | I64TestOp (I64Op Test)
  deriving (Generic, NFData, Show)

data CompareOp
  = I32CompareOp (I32Op Compare)
  | I64CompareOp (I64Op Compare)
  | F32CompareOp (F32Op Compare)
  | F64CompareOp (F64Op Compare)
  deriving (Generic, NFData, Show)

data ConvertOp
  = I32ConvertOp (I32Op Convert)
  | I64ConvertOp (I64Op Convert)
  | F32ConvertOp (F32Op Convert)
  | F64ConvertOp (F64Op Convert)
  deriving (Generic, NFData, Show)

data MemoryOp size
  = MemoryOp
  { _memoryValueType :: ValueType
  , _memoryAlignment :: Int
  , _memoryOffset :: Int
  , _memorySize :: Maybe size
  } deriving (Generic, Generic1, NFData, NFData1, Show)

makeLenses ''MemoryOp

type LoadOp = MemoryOp (PackSize, Extension)

type StoreOp = MemoryOp PackSize
