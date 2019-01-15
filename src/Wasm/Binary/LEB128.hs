{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}

module Wasm.Binary.LEB128 where

import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.Bool
import           Data.Int
import           Text.Printf

getULEB128 :: (Bits a, Integral a) => Int -> Get a
getULEB128 budget
  | budget <= 0 = fail $ printf "getULEB128: invalid budget: %d" budget
  | otherwise = do
    byte <- getWord8
    let overflow =
          budget < 7 && byte .&. 0b01111111 >= shiftL 0b00000001 budget
    when overflow $ fail "getULEB128: integer overflow"
    if not $ testBit byte 7
      then return $ fromIntegral byte
      else do
        residue <- getULEB128 $ budget - 7
        let value = fromIntegral $ byte .&. 0b01111111
        return $ value .|. shiftL residue 7

putULEB128 :: (Bits a, Integral a) => a -> Put
putULEB128 value = if value < 0b10000000
  then putWord8 $ fromIntegral value
  else do
    putWord8 $ 0b01111111 .&. fromIntegral value + 0b10000000
    putULEB128 $ shiftR value 7

getSLEB128 :: (Bits a, Integral a) => Int -> Get a
getSLEB128 budget
  | budget <= 0 = fail $ printf "getSLEB128: invalid budget: %d" budget
  | otherwise = do
    byte <- getWord8
    let mask = shiftL 0b11111111 (budget - 1) .&. 0b01111111
    let overflow =
          budget < 7 && byte .&. mask /= 0b00000000 && byte .&. mask /= mask
    when overflow $ fail "getSLEB128: integer overflow"
    if not $ testBit byte 7
      then return $ coerce $ shiftL byte 1 .&. 0b10000000 .|. byte
      else do
        residue <- getSLEB128 $ budget - 7
        let value = fromIntegral $ byte .&. 0b01111111
        return $ value .|. shiftL residue 7
  where coerce = fromIntegral @Int8 . fromIntegral

putSLEB128 :: (Bits a, Integral a) => a -> Put
putSLEB128 value = go value
 where
  negative    = value < 0
  nonnegative = not negative
  mask        = bool complement id nonnegative 0b00000000
  go residue = do
    let byte     = fromIntegral $ residue .&. 0b01111111
    let residue' = shiftR residue 7
    if residue' /= mask
      then do
        putWord8 $ byte .|. 0b10000000
        go residue'
      else do
        let signed   = testBit byte 6
        let unsigned = not signed
        if unsigned && nonnegative || signed && negative
          then putWord8 byte
          else do
            putWord8 $ byte .|. 0b10000000
            putWord8 $ fromIntegral mask .&. 0b01111111
