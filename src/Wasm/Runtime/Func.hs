{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Wasm.Runtime.Func where

import Data.Functor.Classes
import Wasm.Syntax.AST
import Wasm.Syntax.Types
import Wasm.Syntax.Values
import Wasm.Util.Source
import Lens.Micro.Platform

data FuncInst f m a
  = AstFunc FuncType a (f (Func f))
  | HostFunc FuncType ([Value] -> [Value])
  | HostFuncEff FuncType ([Value] -> m (Either String [Value]))
  deriving (Functor, Foldable, Traversable)

_AstFunc :: Traversal' (FuncInst f m a) (FuncType, a, f (Func f))
_AstFunc f (AstFunc x y z) = (\(x',y',z') -> AstFunc x' y' z') <$> f (x, y, z)
_AstFunc _ x = pure x

_HostFunc :: Traversal' (FuncInst f m a) (FuncType, [Value] -> [Value])
_HostFunc f (HostFunc x y) = (\(x',y') -> HostFunc x' y') <$> f (x, y)
_HostFunc _ x = pure x

_HostFuncEff :: Traversal' (FuncInst f m a) (FuncType, [Value] -> m (Either String [Value]))
_HostFuncEff f (HostFuncEff x y) = (\(x',y') -> HostFuncEff x' y') <$> f (x, y)
_HostFuncEff _ x = pure x

instance (Regioned f, Show1 f, Show a) => Show (FuncInst f m a) where
  showsPrec d = \case
    AstFunc ty a f ->
      showParen (d > 10)
        $ showString "AstFunc "
        . showsPrec 11 ty
        . showString " "
        . showsPrec 11 a
        . showString " "
        . showsPrec1 11 f
    HostFunc ty _f ->
      showParen (d > 10)
        $ showString "HostFunc "
        . showsPrec 11 ty
    HostFuncEff ty _f ->
      showParen (d > 10)
        $ showString "HostFuncEff "
        . showsPrec 11 ty

instance (Regioned f, Show1 f) => Show1 (FuncInst f m) where
  liftShowsPrec h _k d = \case
    AstFunc ty a f ->
      showParen (d > 10)
        $ showString "AstFunc "
        . showsPrec 11 ty
        . showString " "
        . h 11 a
        . showString " "
        . showsPrec1 11 f
    HostFunc ty _f ->
      showParen (d > 10)
        $ showString "HostFunc "
        . showsPrec 11 ty
    HostFuncEff ty _f ->
      showParen (d > 10)
        $ showString "HostFuncEff "
        . showsPrec 11 ty

alloc :: FuncType -> a -> f (Func f) -> FuncInst f m a
alloc = AstFunc

allocHost :: FuncType -> ([Value] -> [Value]) -> FuncInst f m a
allocHost = HostFunc

allocHostEff :: FuncType -> ([Value] -> m (Either String [Value])) -> FuncInst f m a
allocHostEff = HostFuncEff

typeOf :: FuncInst f m a -> FuncType
typeOf = \case
  AstFunc ft _ _ -> ft
  HostFunc ft _ -> ft
  HostFuncEff ft _ -> ft
