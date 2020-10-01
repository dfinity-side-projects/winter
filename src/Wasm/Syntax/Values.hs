{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Wasm.Syntax.Values where

import           Control.DeepSeq
import           Data.Bool
import           Data.Int
import           GHC.Generics

import           Wasm.Syntax.Types

data Value
  = I32 {-# UNPACK #-} !Int32
  | I64 {-# UNPACK #-} !Int64
  | F32 {-# UNPACK #-} !Float
  | F64 {-# UNPACK #-} !Double
  deriving (Generic, NFData, Show)

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
