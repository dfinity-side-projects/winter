module Wasm.Binary.Guard where

import           Data.Binary.Get
import           Data.Word
import           Numeric.Natural
import           Text.Printf

byteGuard :: Word8 -> Get ()
byteGuard expect = do
  actual <- getWord8
  if expect == actual
    then return ()
    else fail $ printf "byteGuard: expect 0x%02X != actual 0x%02X" expect actual

sizeGuard :: Natural -> Get a -> Get a
sizeGuard expect getValue = do
  before <- bytesRead
  val    <- getValue
  after  <- bytesRead
  let actual = fromIntegral $ after - before
  if expect == actual
    then return val
    else fail $ printf "sizeGuard: expect %d != actual %d" expect actual
