{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is a parser for Wast scripts, used by the WebAssembly specification.

module Wasm.Text.Winter where

import           Control.Monad.Except
import           Data.Binary.Get
import           Data.Default.Class (Default(..))
import           Data.Functor.Classes

import           Wasm.Text.Wast
import qualified Wasm.Binary.Decode as Decode
import qualified Wasm.Exec.Eval as Eval
import qualified Wasm.Runtime.Instance as Instance
import           Wasm.Runtime.Mutable
import qualified Wasm.Syntax.AST as AST
import qualified Wasm.Syntax.Values as Values
import           Wasm.Util.Source

data Winter (f :: * -> *) = Winter

instance (MonadRef m, Regioned f, Decode.Decodable f, Show1 f)
    => WasmEngine (Winter f) m where
  type Value (Winter f) = Values.Value
  type Module (Winter f) = AST.Module f
  type ModuleInst (Winter f) m = Instance.ModuleInst f m

  const_i32 = Values.I32
  const_i64 = Values.I64
  const_f32 = Values.F32
  const_f64 = Values.F64

  decodeModule = Right . runGet Decode.getModule
  initializeModule m names mods =
    fmap (either (Left . show) Right)
      $ runExceptT $ Eval.initialize (m @@ def) names mods

  invokeByName mods inst name stack =
    fmap (either (Left . show) (Right . (,inst)))
      $ runExceptT $ Eval.invokeByName mods inst name stack
  getByName inst name  =
    fmap (either (Left . show) (Right . (,inst)))
      $ runExceptT $ Eval.getByName inst name
