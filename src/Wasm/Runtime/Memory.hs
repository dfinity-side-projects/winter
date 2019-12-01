{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Wasm.Runtime.Memory
  ( MemoryInst
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
  , loadBytes
  , loadPacked
  , loadValue
  , exportMemory
  , importMemory
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Data.Array.ST (newArray, readArray, MArray, STUArray)
import           Data.Array.Unsafe (castSTUArray)
import           Data.Int
import           Data.Primitive.MutVar
import           Data.Primitive.ByteArray
import qualified Data.Primitive.ByteArray.Unaligned    as UABA
import qualified Data.Primitive.ByteArray.LittleEndian as LEBA
import           Data.Word
import           GHC.ST (runST, ST)
import           Lens.Micro.Platform
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Short.Internal (ShortByteString(..), toShort, fromShort)

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

-- Bulk access

loadBytes :: PrimMonad m
           => MemoryInst m -> Address -> Size
           -> ExceptT MemoryError m ByteString
loadBytes mem a n = do
  bnd <- lift $ bound mem
  when (fromIntegral a + n > fromIntegral bnd) $
       throwError MemoryBoundsError
  lift $ do
    m <- readMutVar (mem^.miContent)
    m' <- newByteArray (fromIntegral n)
    copyMutableByteArray m' 0 m (fromIntegral a) (fromIntegral n)
    ByteArray ba <- unsafeFreezeByteArray m'
    return $ fromShort (SBS ba)

storeBytes :: PrimMonad m
           => MemoryInst m -> Address -> ByteString
           -> ExceptT MemoryError m ()
storeBytes mem a bs = do
  bnd <- lift $ bound mem
  when (fromIntegral a + BS.length bs > fromIntegral bnd) $
       throwError MemoryBoundsError
  lift $ do
    m <- readMutVar (mem^.miContent)
    let !(SBS ba) = toShort bs
    copyByteArray m (fromIntegral a) (ByteArray ba) 0 (BS.length bs)

-- Value access

effectiveAddress :: Monad m
                 => Address -> Offset -> ExceptT MemoryError m Address
effectiveAddress a o = do
  let ea = a + fromIntegral o
  if ea < a
    then throwError MemoryBoundsError
    else pure ea

loadValue :: (PrimMonad m)
          => MemoryInst m -> Address -> Offset -> ValueType
          -> ExceptT MemoryError m Value
loadValue mem a o t = do
  bnd <- lift $ bound mem
  addr <- effectiveAddress a o
  when (addr + fromIntegral (valueTypeSize t) > fromIntegral bnd) $
    throwError MemoryBoundsError
  lift $ do
    m <- readMutVar (mem^.miContent)
    case t of
      I32Type -> Values.I32 <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
      I64Type -> Values.I64 <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
      F32Type -> Values.F32 <$> UABA.readUnalignedByteArray m (fromIntegral addr)
      F64Type -> Values.F64 <$> UABA.readUnalignedByteArray m (fromIntegral addr)

storeValue :: (PrimMonad m)
           => MemoryInst m -> Address -> Offset -> Value
           -> ExceptT MemoryError m ()
storeValue mem a o v = do
  bnd <- lift $ bound mem
  addr <- effectiveAddress a o
  when (addr + fromIntegral (valueTypeSize (Values.typeOf v)) > fromIntegral bnd) $
    throwError MemoryBoundsError
  lift $ do
    m <- readMutVar (mem^.miContent)
    case v of
      Values.I32 y -> LEBA.writeUnalignedByteArray m (fromIntegral addr) y
      Values.I64 y -> LEBA.writeUnalignedByteArray m (fromIntegral addr) y
      Values.F32 y -> UABA.writeUnalignedByteArray m (fromIntegral addr) y
      Values.F64 y -> UABA.writeUnalignedByteArray m (fromIntegral addr) y

-- Packed access

loadPacked :: PrimMonad m
           => PackSize
           -> Extension
           -> MemoryInst m
           -> Address
           -> Offset
           -> ValueType
           -> ExceptT MemoryError m Value
loadPacked sz ext mem a o t = do
  let n = packedSize sz
  assert (n <= valueTypeSize t) $ do
    bnd <- lift $ bound mem
    addr <- effectiveAddress a o
    when (addr + fromIntegral n > fromIntegral bnd) $
      throwError MemoryBoundsError
    m <- lift $ readMutVar (mem^.miContent)
    case t of
      I32Type -> lift $ Values.I32 <$> case (ext, sz) of
        (ZX, Pack8)  -> fromIntegral @Word8  <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (SX, Pack8)  -> fromIntegral @Int8   <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (ZX, Pack16) -> fromIntegral @Word16 <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (SX, Pack16) -> fromIntegral @Int16  <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (ZX, Pack32) -> fromIntegral @Word32 <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (SX, Pack32) -> fromIntegral @Int32  <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
      I64Type -> lift $ Values.I64 <$> case (ext, sz) of
        (ZX, Pack8)  -> fromIntegral @Word8  <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (SX, Pack8)  -> fromIntegral @Int8   <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (ZX, Pack16) -> fromIntegral @Word16 <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (SX, Pack16) -> fromIntegral @Int16  <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (ZX, Pack32) -> fromIntegral @Word32 <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
        (SX, Pack32) -> fromIntegral @Int32  <$> LEBA.readUnalignedByteArray m (fromIntegral addr)
      _ -> throwError MemoryTypeError

storePacked :: PrimMonad m
            => PackSize -> MemoryInst m -> Address -> Offset -> Value
            -> ExceptT MemoryError m ()
storePacked sz mem a o v = do
  let n = packedSize sz
  assert (n <= valueTypeSize (Values.typeOf v)) $ do
    bnd <- lift $ bound mem
    addr <- effectiveAddress a o
    when (addr + fromIntegral n > fromIntegral bnd) $
      throwError MemoryBoundsError
    m <- lift $ readMutVar (mem^.miContent)
    case v of
      Values.I32 y -> lift $ case sz of
        Pack8  -> LEBA.writeUnalignedByteArray m (fromIntegral addr) (fromIntegral @_ @Word8 y)
        Pack16 -> LEBA.writeUnalignedByteArray m (fromIntegral addr) (fromIntegral @_ @Word16 y)
        Pack32 -> LEBA.writeUnalignedByteArray m (fromIntegral addr) (fromIntegral @_ @Word32 y)
      Values.I64 y -> lift $ case sz of
        Pack8  -> LEBA.writeUnalignedByteArray m (fromIntegral addr) (fromIntegral @_ @Word8 y)
        Pack16 -> LEBA.writeUnalignedByteArray m (fromIntegral addr) (fromIntegral @_ @Word16 y)
        Pack32 -> LEBA.writeUnalignedByteArray m (fromIntegral addr) (fromIntegral @_ @Word32 y)
      _ -> throwError MemoryTypeError

-- Persistence/Restore

exportMemory :: PrimMonad m => MemoryInst m -> m ByteString
exportMemory mem = do
    m <- readMutVar (mem^.miContent)
    s <- getSizeofMutableByteArray m
    m' <- newByteArray s
    copyMutableByteArray m' 0 m 0 s
    ByteArray ba <- unsafeFreezeByteArray m'
    return $ fromShort (SBS ba)

importMemory :: PrimMonad m => MemoryInst m -> ByteString -> m ()
importMemory mem bs = do
    m <- readMutVar (mem^.miContent)
    m' <- resizeMutableByteArray m (BS.length bs)
    let !(SBS ba) = toShort bs
    copyByteArray m' 0 (ByteArray ba) 0 (BS.length bs)
    writeMutVar (mem^.miContent) m'

-- Conversions used in "Wasm.Exec.EvalNumeric"

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
