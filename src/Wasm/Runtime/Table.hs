{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Runtime.Table where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Int (Int32)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Lens.Micro.Platform

import           Wasm.Runtime.Mutable
import           Wasm.Syntax.Types

type Size = Int32
type Index = Int32

data TableInst m elem = TableInst
  { _tiContent :: Mutable m (Vector (Maybe elem))
  , _tiMax :: Maybe Size
  , _tiElemType :: ElemType
  }

instance Show (TableInst m elem) where
  showsPrec _d TableInst {..} = showString "TableInst"

makeLenses ''TableInst

data TableError
  = TableBoundsError
  | TableSizeOverflow
  | TableSizeLimit
  deriving (Show, Eq)

withinLimits :: Size -> Maybe Size -> Bool
withinLimits sz = \case
  Nothing -> True
  Just m  -> sz <= m

create :: Size -> Vector (Maybe a)
create sz = V.replicate (fromIntegral sz) Nothing

alloc :: (MonadRef m, Monad m)
      => TableType -> ExceptT TableError m (TableInst m a)
alloc (TableType elemType (Limits min' mmax)) = do
  tbl <- lift $ newMut (create min')
  pure $ assert (withinLimits min' mmax) $
    TableInst
      { _tiContent = tbl
      , _tiMax = mmax
      , _tiElemType = elemType
      }

size :: (MonadRef m, Monad m) => TableInst m a -> m Size
size tab = do
  content <- getMut (tab^.tiContent)
  pure $ fromIntegral $ V.length content

typeOf :: (MonadRef m, Monad m) => TableInst m a -> m TableType
typeOf tab = do
  sz <- size tab
  pure $ TableType (tab^.tiElemType) (Limits sz (tab^.tiMax))

grow :: (MonadRef m, Monad m)
     => TableInst m a -> Size -> ExceptT TableError m ()
grow tab delta = do
  oldSize <- lift $ size tab
  let newSize = oldSize + delta
  if oldSize > newSize
    then throwError TableSizeOverflow
    else if not (withinLimits newSize (tab^.tiMax))
         then throwError TableSizeLimit
         else lift $ modifyMut (tab^.tiContent) $ \v -> V.create $ do
           mv <- V.thaw v
           VM.grow mv (fromIntegral (newSize - oldSize))

load :: (MonadRef m, Monad m) => TableInst m a -> Index -> m (Maybe a)
load tab i = do
  content <- getMut (tab^.tiContent)
  pure $ join $ content V.!? fromIntegral i

store :: MonadRef m => TableInst m a -> Index -> a -> m ()
store tab i v =
  modifyMut (tab^.tiContent) $
    V.modify (\vec -> VM.write vec (fromIntegral i) (Just v))

blit :: MonadRef m => TableInst m a -> Index -> Vector a -> m ()
blit tab offset elems =
  -- V.blit dat 0l (tab^.tiContent) offset (V.length dat)
  modifyMut (tab^.tiContent)
    (V.// zip [fromIntegral offset..] (map Just (V.toList elems)))
