{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Wasm.Runtime.Mutable where

import Data.IORef

class MonadRef m where
  data Mutable m a :: *

  newMut    :: a -> m (Mutable m a)
  getMut    :: Mutable m a -> m a
  setMut    :: Mutable m a -> a -> m ()
  modifyMut :: Mutable m a -> (a -> a) -> m ()

instance MonadRef IO where
  newtype Mutable IO a = IOMutable { getIOMutable :: IORef a }

  newMut    = fmap IOMutable . newIORef
  getMut    = readIORef . getIOMutable
  setMut    = writeIORef . getIOMutable
  modifyMut = modifyIORef . getIOMutable
