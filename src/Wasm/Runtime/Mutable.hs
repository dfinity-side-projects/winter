{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Wasm.Runtime.Mutable where

import Data.IORef

import Control.Monad.Primitive
import Data.Primitive.MutVar

type MonadRef m = PrimMonad m

type Mutable m a = MutVar (PrimState m) a

newMut    :: MonadRef m => a -> m (Mutable m a)
newMut = newMutVar
getMut    :: MonadRef m => Mutable m a -> m a
getMut = readMutVar
setMut    :: MonadRef m => Mutable m a -> a -> m ()
setMut = writeMutVar
modifyMut :: MonadRef m => Mutable m a -> (a -> a) -> m ()
modifyMut = modifyMutVar
