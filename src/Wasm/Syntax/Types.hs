{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Syntax.Types where

import           Control.DeepSeq
import           Data.Int
import           GHC.Generics
import           Lens.Micro.Platform

data ValueType
  = I32Type
  | I64Type
  | F32Type
  | F64Type
  deriving (Eq, Generic, NFData, Ord, Show)

data ElemType
  = AnyFuncType
  deriving (Eq, Generic, NFData, Ord, Show)

type StackType = [ValueType]

data FuncType
  = FuncType
  { _funcInput :: StackType
  , _funcOutput :: StackType
  } deriving (Eq, Generic, NFData, Ord, Show)

makeLenses ''FuncType

data Limits a
  = Limits
  { _limitLower :: a
  , _limitUpper :: Maybe a
  } deriving (Eq, Generic, Generic1, NFData, NFData1, Ord, Show)

makeLenses ''Limits

data Mutability
  = Immutable
  | Mutable
  deriving (Eq, Generic, NFData, Ord, Show)

data TableType
  = TableType
  { _tableElemType :: ElemType
  , _tableLimits :: Limits Int32
  } deriving (Eq, Generic, NFData, Ord, Show)

makeLenses ''TableType

type MemoryType = Limits Int32

data GlobalType
  = GlobalType
  { _globalValueType :: ValueType
  , _globalMutability :: Mutability
  } deriving (Eq, Generic, NFData, Ord, Show)

makeLenses ''GlobalType

data ExternType
  = ExternFuncType FuncType
  | ExternTableType TableType
  | ExternMemoryType MemoryType
  | ExternGlobalType GlobalType
  deriving (Eq, Generic, NFData, Ord, Show)

_ExternFuncType :: Traversal' ExternType FuncType
_ExternFuncType f (ExternFuncType x) = ExternFuncType <$> f x
_ExternFuncType _ x                  = pure x

_ExternTableType :: Traversal' ExternType TableType
_ExternTableType f (ExternTableType x) = ExternTableType <$> f x
_ExternTableType _ x                   = pure x

_ExternMemoryType :: Traversal' ExternType MemoryType
_ExternMemoryType f (ExternMemoryType x) = ExternMemoryType <$> f x
_ExternMemoryType _ x                    = pure x

_ExternGlobalType :: Traversal' ExternType GlobalType
_ExternGlobalType f (ExternGlobalType x) = ExternGlobalType <$> f x
_ExternGlobalType _ x                    = pure x

data PackSize = Pack8 | Pack16 | Pack32
  deriving (Eq, Generic, NFData, Ord, Show)

packedSize :: PackSize -> Int32
packedSize = \case
  Pack8 -> 1
  Pack16 -> 2
  Pack32 -> 4

data Extension = SX | ZX
  deriving (Eq, Generic, NFData, Ord, Show)

valueTypeSize :: ValueType -> Int32
valueTypeSize = \case
  I32Type -> 4
  F32Type -> 4
  I64Type -> 8
  F64Type -> 8

matchFuncType :: FuncType -> FuncType -> Bool
matchFuncType = (==)

matchLimits :: Ord a => Limits a -> Limits a -> Bool
matchLimits lim1 lim2 = matchLower && matchUpper
 where
  matchLower = lim1 ^. limitLower >= lim2 ^. limitLower
  matchUpper = case (lim1 ^. limitUpper, lim2 ^. limitUpper) of
    (_      , Nothing) -> True
    (Nothing, Just _ ) -> False
    (Just i , Just j ) -> i <= j

{-# SPECIALIZE matchLimits :: Limits Int32 -> Limits Int32 -> Bool #-}

matchTableType :: TableType -> TableType -> Bool
matchTableType (TableType et1 lim1) (TableType et2 lim2) =
  et1 == et2 && matchLimits lim1 lim2

matchMemoryType :: MemoryType -> MemoryType -> Bool
matchMemoryType = matchLimits

matchGlobalType :: GlobalType -> GlobalType -> Bool
matchGlobalType = (==)

matchExternType :: ExternType -> ExternType -> Bool
matchExternType et1 et2 = case (et1, et2) of
  (ExternFuncType   ft1, ExternFuncType ft2  ) -> matchFuncType ft1 ft2
  (ExternTableType  tt1, ExternTableType tt2 ) -> matchTableType tt1 tt2
  (ExternMemoryType mt1, ExternMemoryType mt2) -> matchMemoryType mt1 mt2
  (ExternGlobalType gt1, ExternGlobalType gt2) -> matchGlobalType gt1 gt2
  (_                   , _                   ) -> False
