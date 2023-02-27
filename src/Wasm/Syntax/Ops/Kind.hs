{-# LANGUAGE KindSignatures #-}

module Wasm.Syntax.Ops.Kind where

import Data.Kind (Type)

data Unary :: Type

data Binary :: Type

data Test :: Type

data Compare :: Type

data Convert :: Type
