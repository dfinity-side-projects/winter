{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Runtime.Global where

import           Control.Monad.Except
import           Lens.Micro.Platform

import           Wasm.Syntax.Types
import           Wasm.Syntax.Values (Value)
import           Wasm.Runtime.Mutable
import qualified Wasm.Syntax.Values as Values

data GlobalInst m = GlobalInst
  { _giContent :: Mutable m Value
  , _giMut :: Mutability
  }

instance Show (GlobalInst m) where
  showsPrec _d GlobalInst {..} = showString "GlobalInst"

makeLenses ''GlobalInst

data GlobalError
  = GlobalTypeError
  | GlobalNotMutable
  deriving (Show, Eq)

alloc :: (MonadRef m, Monad m)
      => GlobalType -> Value -> ExceptT GlobalError m (GlobalInst m)
alloc (GlobalType t mut) v =
  if Values.typeOf v /= t
  then throwError GlobalTypeError
  else do
    m <- lift $ newMut v
    pure $ GlobalInst m mut

typeOf :: (MonadRef m, Monad m) => GlobalInst m -> m GlobalType
typeOf glob = do
  content <- getMut (glob^.giContent)
  pure $ GlobalType (Values.typeOf content) (glob^.giMut)

load :: MonadRef m => GlobalInst m -> m Value
load glob = getMut (glob^.giContent)

store :: (MonadRef m, Monad m)
      => GlobalInst m -> Value -> ExceptT GlobalError m ()
store glob v = do
  content <- lift $ getMut (glob^.giContent)
  case glob ^. giMut of
    Immutable -> throwError GlobalNotMutable
    Mutable
      | Values.typeOf v /= Values.typeOf content ->
        throwError GlobalTypeError
      | otherwise -> lift $ setMut (glob^.giContent) v
