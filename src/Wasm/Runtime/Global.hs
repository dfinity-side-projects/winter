{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Runtime.Global where

import           Control.Monad.Except
import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import           Lens.Micro.Platform


import           Wasm.Syntax.Types
import           Wasm.Syntax.Values (Value)
import qualified Wasm.Syntax.Values as Values

data GlobalInst m = GlobalInst
  { _giContent :: MutVar (PrimState m) Value
  , _giMut :: Mutability
  }

instance Show (GlobalInst m) where
  showsPrec _d GlobalInst {..} = showString "GlobalInst"

makeLenses ''GlobalInst

data GlobalError
  = GlobalTypeError
  | GlobalNotMutable
  deriving (Show, Eq)

alloc :: PrimMonad m
      => GlobalType -> Value -> ExceptT GlobalError m (GlobalInst m)
alloc (GlobalType t mut) v =
  if Values.typeOf v /= t
  then throwError GlobalTypeError
  else do
    m <- newMutVar v
    pure $ GlobalInst m mut

typeOf :: PrimMonad m => GlobalInst m -> m GlobalType
typeOf glob = do
  content <- readMutVar (glob^.giContent)
  pure $ GlobalType (Values.typeOf content) (glob^.giMut)

load :: PrimMonad m => GlobalInst m -> m Value
load glob = readMutVar (glob^.giContent)

store :: PrimMonad m
      => GlobalInst m -> Value -> ExceptT GlobalError m ()
store glob v = do
  content <- readMutVar (glob^.giContent)
  case glob ^. giMut of
    Immutable -> throwError GlobalNotMutable
    Mutable
      | Values.typeOf v /= Values.typeOf content ->
        throwError GlobalTypeError
      | otherwise -> writeMutVar (glob^.giContent) v
