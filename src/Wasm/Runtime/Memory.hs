{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Runtime.Memory
  ( MemoryInst(..)
  , doubleFromBits, doubleToBits
  , floatFromBits, floatToBits
  , typeOf
  , MemoryError(..)
  , alloc
  , bound
  , grow
  , size
  , storeBytes
  , storeValue
  , storePacked
  , loadPacked
  , loadBytes
  , loadValue
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Data.Array.ST (newArray, readArray, MArray, STUArray)
import           Data.Array.Unsafe (castSTUArray)
import           Data.Bits
import           Data.Int
import           Data.Primitive.MutVar
import           Data.Primitive.ByteArray
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word
import           GHC.ST (runST, ST)
import           Lens.Micro.Platform

import           Wasm.Syntax.Memory
import           Wasm.Syntax.Types
import           Wasm.Syntax.Values (Value)
import qualified Wasm.Syntax.Values as Values

data MemoryInst m = MemoryInst
  { _miContent :: MutVar (PrimState m) (MutableByteArray (PrimState m))
  , _miMax :: Maybe Size
  }

instance Show (MemoryInst m) where
  showsPrec _d MemoryInst {..} = showString "MemoryInst"

makeLenses ''MemoryInst

data MemoryError
  =  MemoryTypeError
  |  MemoryBoundsError
  |  MemorySizeOverflow
  |  MemorySizeLimit
  |  MemoryOutOfMemory
  deriving (Show, Eq)

pageSize :: Size
pageSize = 0x10000 {- 64 KiB -}

packedSize :: PackSize -> Size
packedSize = \case
  Pack8 -> 1
  Pack16 -> 2
  Pack32 -> 4

withinLimits :: Size -> Maybe Size -> Bool
withinLimits n = \case
  Nothing -> True
  Just m -> n <= m

create :: PrimMonad m => Size -> ExceptT MemoryError m (MutableByteArray (PrimState m))
create n
  | n > 0x10000 = throwError MemorySizeOverflow
  | otherwise   = lift $ do
    m <- newByteArray (fromIntegral $ n * pageSize)
    fillByteArray m 0 (fromIntegral $ n * pageSize) 0
    return m

alloc :: PrimMonad m
      => MemoryType -> ExceptT MemoryError m (MemoryInst m)
alloc (Limits min' mmax) = do
  m <- create min'
  mem <- newMutVar m
  pure $ assert (withinLimits min' mmax) $
    MemoryInst { _miContent = mem , _miMax = mmax }

bound :: PrimMonad m => MemoryInst m -> m Size
bound mem = do
  m <- readMutVar (mem^.miContent)
  b <- getSizeofMutableByteArray m
  pure $ fromIntegral b

size :: PrimMonad m => MemoryInst m -> m Size
size mem = liftM2 div (bound mem) (pure pageSize)

typeOf :: PrimMonad m => MemoryInst m -> m MemoryType
typeOf mem = Limits <$> size mem <*> pure (mem^.miMax)

grow :: PrimMonad m
     => MemoryInst m -> Size -> ExceptT MemoryError m ()
grow mem delta = do
  oldSize <- lift $ size mem
  let newSize = oldSize + delta
  let oldSizeBytes = fromIntegral $ oldSize * pageSize
  let newSizeBytes = fromIntegral $ newSize * pageSize
  if | oldSize > newSize ->
         throwError MemorySizeOverflow
     | not (withinLimits newSize (mem^.miMax)) ->
         throwError MemorySizeLimit
     | newSize > 0x10000 ->
         throwError MemorySizeOverflow
     | otherwise -> lift $ do
        m <- readMutVar (mem^.miContent)
        m' <- resizeMutableByteArray m newSizeBytes
        fillByteArray m' oldSizeBytes (newSizeBytes - oldSizeBytes) 0
        writeMutVar (mem^.miContent) m'


loadByte :: PrimMonad m
         => MemoryInst m -> Address -> ExceptT MemoryError m Word8
loadByte mem a = do
  bnd <- lift $ bound mem
  if | a >= fromIntegral bnd -> throwError MemoryBoundsError
     | otherwise -> lift $ do
        m <- readMutVar (mem^.miContent)
        readByteArray m (fromIntegral a)

storeByte :: PrimMonad m
          => MemoryInst m -> Address -> Word8
          -> ExceptT MemoryError m ()
storeByte mem a b = do
  bnd <- lift $ bound mem
  if | a >= fromIntegral bnd ->
       throwError MemoryBoundsError
     | otherwise -> lift $ do
        m <- readMutVar (mem^.miContent)
        writeByteArray m (fromIntegral a) b

loadBytes :: PrimMonad m
          => MemoryInst m -> Address -> Size
          -> ExceptT MemoryError m (Vector Word8)
loadBytes mem a n = V.generateM (fromIntegral n) $ \i ->
  loadByte mem (a + fromIntegral i)

storeBytes :: PrimMonad m
           => MemoryInst m -> Address -> Vector Word8
           -> ExceptT MemoryError m ()
storeBytes mem a bs = do
  bnd <- lift $ bound mem
  if | fromIntegral a + V.length bs > fromIntegral bnd ->
       throwError MemoryBoundsError
     | otherwise -> lift $ do
        m <- readMutVar (mem^.miContent)
        zipWithM_ (writeByteArray m) [fromIntegral a..] (V.toList bs)

effectiveAddress :: Monad m
                 => Address -> Offset -> ExceptT MemoryError m Address
effectiveAddress a o = do
  let ea = a + fromIntegral o
  if ea < a
    then throwError MemoryBoundsError
    else pure ea

loadn :: PrimMonad m
      => MemoryInst m -> Address -> Offset -> Size
      -> ExceptT MemoryError m Int64
loadn mem a o n =
  assert (n > 0 && n <= 8) $ do
    addr <- effectiveAddress a o
    loop addr n
 where
  loop a' n' =
     if n' == 0
     then pure 0
     else do
       r <- loop (a' + 1) (n' - 1)
       let x = shiftL r 8
       b <- loadByte mem a'
       pure $ fromIntegral b .|. x

storen :: PrimMonad m
       => MemoryInst m -> Address -> Offset -> Size -> Int64
       -> ExceptT MemoryError m ()
storen mem a o n x =
  assert (n > 0 && n <= 8) $ do
    addr <- effectiveAddress a o
    loop addr n x
 where
  loop a' n' x'
    | n' <= 0 = return ()
    | otherwise = do
      loop (a' + 1) (n' - 1) (shiftR x' 8)
      storeByte mem a' (fromIntegral x' .&. 0xff)

cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s))
     => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
{-# INLINE cast #-}

floatToBits :: Float -> Int32
floatToBits x = runST (cast x)

floatFromBits :: Int32 -> Float
floatFromBits x = runST (cast x)

doubleToBits :: Double -> Int64
doubleToBits x = runST (cast x)

doubleFromBits :: Int64 -> Double
doubleFromBits x = runST (cast x)

loadValue :: (PrimMonad m)
          => MemoryInst m -> Address -> Offset -> ValueType
          -> ExceptT MemoryError m Value
loadValue mem a o t =
  loadn mem a o (valueTypeSize t) >>= \n -> pure $ case t of
    I32Type -> Values.I32 (fromIntegral n)
    I64Type -> Values.I64 n
    F32Type -> Values.F32 (floatFromBits (fromIntegral n))
    F64Type -> Values.F64 (doubleFromBits n)

storeValue :: (PrimMonad m)
           => MemoryInst m -> Address -> Offset -> Value
           -> ExceptT MemoryError m ()
storeValue mem a o v =
  let x = case v of
        Values.I32 y -> fromIntegral y
        Values.I64 y -> y
        Values.F32 y -> fromIntegral $ floatToBits y
        Values.F64 y -> doubleToBits y
  in storen mem a o (valueTypeSize (Values.typeOf v)) x

-- jww (2018-10-31): Is this type signature correct?
extend :: Address -> Offset -> Extension -> Address
extend x n = \case
  ZX -> x
  SX -> let sh = 64 - 8 * fromIntegral n in shiftR (shiftL x sh) sh

loadPacked :: PrimMonad m
           => PackSize
           -> Extension
           -> MemoryInst m
           -> Address
           -> Offset
           -> ValueType
           -> ExceptT MemoryError m Value
loadPacked sz ext mem a o t =
  assert (packedSize sz <= valueTypeSize t) $ do
    let n = packedSize sz
    v <- loadn mem a o n
    let x = extend v n ext
    case t of
      I32Type -> pure $ Values.I32 (fromIntegral x)
      I64Type -> pure $ Values.I64 x
      _ -> throwError MemoryTypeError

storePacked :: PrimMonad m
            => PackSize -> MemoryInst m -> Address -> Offset -> Value
            -> ExceptT MemoryError m ()
storePacked sz mem a o v =
  assert (packedSize sz <= valueTypeSize (Values.typeOf v)) $ do
    let n = packedSize sz
    x <- case v of
          Values.I32 y -> pure $ fromIntegral y
          Values.I64 y -> pure y
          _ -> throwError MemoryTypeError
    storen mem a o n x
