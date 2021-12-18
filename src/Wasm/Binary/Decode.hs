{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Wasm.Binary.Decode where

import           Control.Applicative           as App
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Binary.Get
import           Data.ByteString.Builder
import           Data.ByteString.Lazy
import           Data.Fix
import           Data.Functor.Identity
import           Data.Int
import           Data.Bits
import           Data.List                     as List
import qualified Data.Vector                   as V
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Data.Word
import           Numeric.Natural
import           Text.Printf

import           Wasm.Binary.Guard
import           Wasm.Binary.LEB128
import           Wasm.Binary.Lift
import           Wasm.Syntax.AST       as AST
import           Wasm.Syntax.Ops
import           Wasm.Syntax.Ops.Int   as Int
import           Wasm.Syntax.Ops.Float as Float
import           Wasm.Syntax.Types
import           Wasm.Syntax.Values
import           Wasm.Util.Source

class (Functor phrase, Get1 phrase) => Decodable phrase where

instance Decodable Identity

instance Decodable Phrase

type SectionT = StateT [Custom]

getList :: forall a . Int -> Get a -> Get [a]
getList budget getValue = do
  n <- getULEB128 budget
  go n
 where
  go :: Natural -> Get [a]
  go 0 = return []
  go n = (:) <$> getValue <*> go (n - 1)

getByteSlice :: Int -> Get ByteString
getByteSlice budget = do
  n <- getULEB128 budget
  getByteStream n

getByteStream :: Natural -> Get ByteString
getByteStream = go mempty
 where
  m = fromIntegral @Int maxBound
  go accum n
    | n > m = do
      chunks <- getLazyByteString $ fromIntegral m
      let stream = accum <> lazyByteString chunks
      go stream $ n - m
    | otherwise = do
      chunks <- getLazyByteString $ fromIntegral n
      let stream = accum <> lazyByteString chunks
      return $ toLazyByteString stream

getText :: Int -> Get Text
getText budget = do
  bytes <- getByteSlice budget
  case decodeUtf8' bytes of
    Left  _    -> fail "getText: malformed UTF-8 encoding"
    Right text -> return text

getValueType :: Get ValueType
getValueType = do
  byte <- getWord8
  case byte of
    0x7F -> return I32Type
    0x7E -> return I64Type
    0x7D -> return F32Type
    0x7C -> return F64Type
    _    -> fail $ printf "getValueType: malformed value type: 0x%02X" byte

getElemType :: Get ElemType
getElemType = do
  byte <- getWord8
  case byte of
    0x70 -> return AnyFuncType
    _    -> fail $ printf "getElemType: malformed element type: 0x%02X" byte

getStackType :: Get StackType
getStackType = getList 32 getValueType

getFuncType :: Get FuncType
getFuncType = do
  byteGuard 0x60
  FuncType <$> getStackType <*> getStackType

getLimits :: Get (Limits Int32)
getLimits = do
  flag  <- getULEB128 1 :: Get Word8
  lower <- getULEB128 32
  if flag == 0x00
    then return $ Limits lower Nothing
    else do
      upper <- getULEB128 32
      return $ Limits lower $ Just upper

getMutability :: Get Mutability
getMutability = do
  byte <- getWord8
  case byte of
    0x00 -> return Immutable
    0x01 -> return Mutable
    _    -> fail $ printf "getMutability: malformed mutability type: 0x%02X" byte

getTableType :: Get TableType
getTableType = TableType <$> getElemType <*> getLimits

getMemoryType :: Get MemoryType
getMemoryType = getLimits

getGlobalType :: Get GlobalType
getGlobalType = GlobalType <$> getValueType <*> getMutability

getVar :: Decodable phrase => Get (Var phrase)
getVar = liftGet $ getULEB128 32

{-# SPECIALIZE getVar :: Get (Var Identity) #-}
{-# SPECIALIZE getVar :: Get (Var Phrase) #-}

getGlobal :: Decodable phrase => Get (Global phrase)
getGlobal = Global <$> getGlobalType <*> getExpression

{-# SPECIALIZE getGlobal :: Get (Global Identity) #-}
{-# SPECIALIZE getGlobal :: Get (Global Phrase) #-}

getFunc :: Decodable phrase => Get (Func phrase)
getFunc = do
  size <- getULEB128 32
  sizeGuard size $ do
    let _funcType = error "getFunc: undefined"
    _funcLocals <- getLocals
    _funcBody   <- getInstrBlock
    byteGuard 0x0B
    return Func {..}

{-# SPECIALIZE getFunc :: Get (Func Identity) #-}
{-# SPECIALIZE getFunc :: Get (Func Phrase) #-}

getBlockType :: Decodable phrase => Get (BlockType phrase)
getBlockType = do
  byte <- lookAhead getWord8
  case byte of
    0x40 ->
         ValBlockType Nothing <$ skip 1
    _ | byte .&. 0xc0 == 0x40 ->
         ValBlockType . Just <$> getValueType
    _ -> VarBlockType <$> getVar

getLocals :: Get [ValueType]
getLocals = do
  n <- getULEB128 32
  go 0 n
 where
  go :: Natural -> Natural -> Get [ValueType]
  go _ 0 = return []
  go i n = do
    j <- getULEB128 32
    let k = i + j
    if k > 1000000
      then fail "getLocals: too many locals"
      else do
        locals <- genericReplicate j <$> getValueType
        more   <- go k $ n - 1
        return $ locals ++ more

getTableSegment :: Decodable phrase => Get (TableSegment phrase)
getTableSegment = Segment <$> getVar <*> getExpression <*> getList 32 getVar

{-# SPECIALIZE getTableSegment :: Get (TableSegment Identity) #-}
{-# SPECIALIZE getTableSegment :: Get (TableSegment Phrase) #-}

getMemorySegment :: Decodable phrase => Get (MemorySegment phrase)
getMemorySegment = Segment <$> getVar <*> getExpression <*> getByteSlice 32

{-# SPECIALIZE getMemorySegment :: Get (MemorySegment Identity) #-}
{-# SPECIALIZE getMemorySegment :: Get (MemorySegment Phrase) #-}

getImportDesc :: Decodable phrase => Get (ImportDesc phrase)
getImportDesc = do
  byte <- getWord8
  case byte of
    0x00 -> FuncImport <$> getVar
    0x01 -> TableImport <$> getTableType
    0x02 -> MemoryImport <$> getMemoryType
    0x03 -> GlobalImport <$> getGlobalType
    _    -> fail
      $ printf "getImportDesc: malformed import description type: 0x%02X" byte

{-# SPECIALIZE getImportDesc :: Get (ImportDesc Identity) #-}
{-# SPECIALIZE getImportDesc :: Get (ImportDesc Phrase) #-}

getImport :: Decodable phrase => Get (Import phrase)
getImport = Import <$> getText 32 <*> getText 32 <*> getImportDesc

{-# SPECIALIZE getImport :: Get (Import Identity) #-}
{-# SPECIALIZE getImport :: Get (Import Phrase) #-}

getExportDesc :: Decodable phrase => Get (ExportDesc phrase)
getExportDesc = do
  byte <- getWord8
  case byte of
    0x00 -> FuncExport <$> getVar
    0x01 -> TableExport <$> getVar
    0x02 -> MemoryExport <$> getVar
    0x03 -> GlobalExport <$> getVar
    _    -> fail
      $ printf "getExportDesc: invalid export description type: 0x%02X" byte

{-# SPECIALIZE getExportDesc :: Get (ExportDesc Identity) #-}
{-# SPECIALIZE getExportDesc :: Get (ExportDesc Phrase) #-}

getExport :: Decodable phrase => Get (Export phrase)
getExport = Export <$> getText 32 <*> getExportDesc

{-# SPECIALIZE getExport :: Get (Export Identity) #-}
{-# SPECIALIZE getExport :: Get (Export Phrase) #-}

getMemoryOp :: ValueType -> Maybe size -> Get (MemoryOp size)
getMemoryOp valueType size = do
  alignment <- getULEB128 32
  if alignment > 32
    then fail (printf "getMemoryOp: invalid alignment: %d" alignment)
    else do
      offset <- getULEB128 32
      return $ MemoryOp valueType alignment offset size

getMathPrefix :: Get (InstrF phrase x)
getMathPrefix = do
  byte :: Word8 <- getULEB128 32
  case byte of
    0x00 -> return $ Convert $ I32ConvertOp Int.TruncSSatF32
    0x01 -> return $ Convert $ I32ConvertOp Int.TruncUSatF32
    0x02 -> return $ Convert $ I32ConvertOp Int.TruncSSatF64
    0x03 -> return $ Convert $ I32ConvertOp Int.TruncUSatF64
    0x04 -> return $ Convert $ I64ConvertOp Int.TruncSSatF32
    0x05 -> return $ Convert $ I64ConvertOp Int.TruncUSatF32
    0x06 -> return $ Convert $ I64ConvertOp Int.TruncSSatF64
    0x07 -> return $ Convert $ I64ConvertOp Int.TruncUSatF64
    0x0A -> do (0, 0) <- (,) <$> getWord8 <*> getWord8
               return $ MemoryCopy
    0x0B -> do 0 <- getWord8
               return $ MemoryFill
    _    -> fail (printf "getMathPrefix: illegal op %d" byte)


getInstr :: Decodable phrase => Get (Instr phrase)
getInstr = Fix <$> do
  byte <- getWord8
  case byte of

    -- Control flow operators.
    0x00 -> return Unreachable
    0x01 -> return Nop
    0x02 -> do
      result <- getBlockType
      expr   <- getInstrBlock
      byteGuard 0x0B
      return $ Block result expr
    0x03 -> do
      result <- getBlockType
      expr   <- getInstrBlock
      byteGuard 0x0B
      return $ Loop result expr
    0x04 -> do
      condition   <- getBlockType
      consequent  <- getInstrBlock
      alternative <- getAlternative <|> return []
      byteGuard 0x0B
      return $ If condition consequent alternative
    0x0C -> Br <$> getVar
    0x0D -> BrIf <$> getVar
    0x0E -> do
      table <- getList 32 getVar
      var   <- getVar
      return $ BrTable table var
    0x0F -> return Return

    -- Call operators.
    0x10 -> Call <$> getVar
    0x11 -> do
      var <- getVar
      byteGuard 0x00
      return $ CallIndirect var

    -- Parametric operators.
    0x1A -> return Drop
    0x1B -> return Select

    -- Variable access.
    0x20 -> GetLocal <$> getVar
    0x21 -> SetLocal <$> getVar
    0x22 -> TeeLocal <$> getVar
    0x23 -> GetGlobal <$> getVar
    0x24 -> SetGlobal <$> getVar

    -- Load operators.
    0x28 -> do
      op <- getMemoryOp I32Type Nothing
      return $ Load op
    0x29 -> do
      op <- getMemoryOp I64Type Nothing
      return $ Load op
    0x2A -> do
      op <- getMemoryOp F32Type Nothing
      return $ Load op
    0x2B -> do
      op <- getMemoryOp F64Type Nothing
      return $ Load op
    0x2C -> do
      op <- getMemoryOp I32Type $ Just (Pack8, SX)
      return $ Load op
    0x2D -> do
      op <- getMemoryOp I32Type $ Just (Pack8, ZX)
      return $ Load op
    0x2E -> do
      op <- getMemoryOp I32Type $ Just (Pack16, SX)
      return $ Load op
    0x2F -> do
      op <- getMemoryOp I32Type $ Just (Pack16, ZX)
      return $ Load op
    0x30 -> do
      op <- getMemoryOp I64Type $ Just (Pack8, SX)
      return $ Load op
    0x31 -> do
      op <- getMemoryOp I64Type $ Just (Pack8, ZX)
      return $ Load op
    0x32 -> do
      op <- getMemoryOp I64Type $ Just (Pack16, SX)
      return $ Load op
    0x33 -> do
      op <- getMemoryOp I64Type $ Just (Pack16, ZX)
      return $ Load op
    0x34 -> do
      op <- getMemoryOp I64Type $ Just (Pack32, SX)
      return $ Load op
    0x35 -> do
      op <- getMemoryOp I64Type $ Just (Pack32, ZX)
      return $ Load op

    -- Store operators.
    0x36 -> do
      op <- getMemoryOp I32Type Nothing
      return $ Store op
    0x37 -> do
      op <- getMemoryOp I64Type Nothing
      return $ Store op
    0x38 -> do
      op <- getMemoryOp F32Type Nothing
      return $ Store op
    0x39 -> do
      op <- getMemoryOp F64Type Nothing
      return $ Store op
    0x3A -> do
      op <- getMemoryOp I32Type $ Just Pack8
      return $ Store op
    0x3B -> do
      op <- getMemoryOp I32Type $ Just Pack16
      return $ Store op
    0x3C -> do
      op <- getMemoryOp I64Type $ Just Pack8
      return $ Store op
    0x3D -> do
      op <- getMemoryOp I64Type $ Just Pack16
      return $ Store op
    0x3E -> do
      op <- getMemoryOp I64Type $ Just Pack32
      return $ Store op

    -- Memory operators.
    0x3F -> do
      byteGuard 0x00
      return MemorySize
    0x40 -> do
      byteGuard 0x00
      return MemoryGrow

    -- Constants.
    0x41 -> do
      val <- liftGet $ I32 <$> getSLEB128 32
      return $ AST.Const $ val
    0x42 -> do
      val <- liftGet $ I64 <$> getSLEB128 64
      return $ AST.Const $ val
    0x43 -> do
      val <- liftGet $ F32 <$> getFloatle
      return $ AST.Const $ val
    0x44 -> do
      val <- liftGet $ F64 <$> getDoublele
      return $ AST.Const $ val

    -- Unary operators for 32-bit integers.
    0x67 -> return $ Unary $ I32UnaryOp Int.Clz
    0x68 -> return $ Unary $ I32UnaryOp Int.Ctz
    0x69 -> return $ Unary $ I32UnaryOp Int.Popcnt

    -- Unary operators for 64-bit integers.
    0x79 -> return $ Unary $ I64UnaryOp Int.Clz
    0x7A -> return $ Unary $ I64UnaryOp Int.Ctz
    0x7B -> return $ Unary $ I64UnaryOp Int.Popcnt

    -- Unary operators for 32-bit floats.
    0x8B -> return $ Unary $ F32UnaryOp Float.Abs
    0x8C -> return $ Unary $ F32UnaryOp Float.Neg
    0x8D -> return $ Unary $ F32UnaryOp Float.Ceil
    0x8E -> return $ Unary $ F32UnaryOp Float.Floor
    0x8F -> return $ Unary $ F32UnaryOp Float.Trunc
    0x90 -> return $ Unary $ F32UnaryOp Float.Nearest
    0x91 -> return $ Unary $ F32UnaryOp Float.Sqrt

    -- Unary operators for 64-bit floats.
    0x99 -> return $ Unary $ F64UnaryOp Float.Abs
    0x9A -> return $ Unary $ F64UnaryOp Float.Neg
    0x9B -> return $ Unary $ F64UnaryOp Float.Ceil
    0x9C -> return $ Unary $ F64UnaryOp Float.Floor
    0x9D -> return $ Unary $ F64UnaryOp Float.Trunc
    0x9E -> return $ Unary $ F64UnaryOp Float.Nearest
    0x9F -> return $ Unary $ F64UnaryOp Float.Sqrt

    -- Binary operators for 32-bit integers.
    0x6A -> return $ Binary $ I32BinaryOp Int.Add
    0x6B -> return $ Binary $ I32BinaryOp Int.Sub
    0x6C -> return $ Binary $ I32BinaryOp Int.Mul
    0x6D -> return $ Binary $ I32BinaryOp Int.DivS
    0x6E -> return $ Binary $ I32BinaryOp Int.DivU
    0x6F -> return $ Binary $ I32BinaryOp Int.RemS
    0x70 -> return $ Binary $ I32BinaryOp Int.RemU
    0x71 -> return $ Binary $ I32BinaryOp Int.And
    0x72 -> return $ Binary $ I32BinaryOp Int.Or
    0x73 -> return $ Binary $ I32BinaryOp Int.Xor
    0x74 -> return $ Binary $ I32BinaryOp Int.Shl
    0x75 -> return $ Binary $ I32BinaryOp Int.ShrS
    0x76 -> return $ Binary $ I32BinaryOp Int.ShrU
    0x77 -> return $ Binary $ I32BinaryOp Int.Rotl
    0x78 -> return $ Binary $ I32BinaryOp Int.Rotr

    -- Binary operators for 64-bit integers.
    0x7C -> return $ Binary $ I64BinaryOp Int.Add
    0x7D -> return $ Binary $ I64BinaryOp Int.Sub
    0x7E -> return $ Binary $ I64BinaryOp Int.Mul
    0x7F -> return $ Binary $ I64BinaryOp Int.DivS
    0x80 -> return $ Binary $ I64BinaryOp Int.DivU
    0x81 -> return $ Binary $ I64BinaryOp Int.RemS
    0x82 -> return $ Binary $ I64BinaryOp Int.RemU
    0x83 -> return $ Binary $ I64BinaryOp Int.And
    0x84 -> return $ Binary $ I64BinaryOp Int.Or
    0x85 -> return $ Binary $ I64BinaryOp Int.Xor
    0x86 -> return $ Binary $ I64BinaryOp Int.Shl
    0x87 -> return $ Binary $ I64BinaryOp Int.ShrS
    0x88 -> return $ Binary $ I64BinaryOp Int.ShrU
    0x89 -> return $ Binary $ I64BinaryOp Int.Rotl
    0x8A -> return $ Binary $ I64BinaryOp Int.Rotr

    -- Binary operators for 32-bit floats.
    0x92 -> return $ Binary $ F32BinaryOp Float.Add
    0x93 -> return $ Binary $ F32BinaryOp Float.Sub
    0x94 -> return $ Binary $ F32BinaryOp Float.Mul
    0x95 -> return $ Binary $ F32BinaryOp Float.Div
    0x96 -> return $ Binary $ F32BinaryOp Float.Min
    0x97 -> return $ Binary $ F32BinaryOp Float.Max
    0x98 -> return $ Binary $ F32BinaryOp Float.CopySign

    -- Binary operators for 64-bit floats.
    0xA0 -> return $ Binary $ F64BinaryOp Float.Add
    0xA1 -> return $ Binary $ F64BinaryOp Float.Sub
    0xA2 -> return $ Binary $ F64BinaryOp Float.Mul
    0xA3 -> return $ Binary $ F64BinaryOp Float.Div
    0xA4 -> return $ Binary $ F64BinaryOp Float.Min
    0xA5 -> return $ Binary $ F64BinaryOp Float.Max
    0xA6 -> return $ Binary $ F64BinaryOp Float.CopySign

    -- Test operators for 32-bit integers.
    0x45 -> return $ Test $ I32TestOp Int.Eqz

    -- Test operators for 64-bit integers.
    0x50 -> return $ Test $ I64TestOp Int.Eqz

    -- Comparison operators for 32-bit integers.
    0x46 -> return $ Compare $ I32CompareOp Int.Eq
    0x47 -> return $ Compare $ I32CompareOp Int.Ne
    0x48 -> return $ Compare $ I32CompareOp Int.LtS
    0x49 -> return $ Compare $ I32CompareOp Int.LtU
    0x4A -> return $ Compare $ I32CompareOp Int.GtS
    0x4B -> return $ Compare $ I32CompareOp Int.GtU
    0x4C -> return $ Compare $ I32CompareOp Int.LeS
    0x4D -> return $ Compare $ I32CompareOp Int.LeU
    0x4E -> return $ Compare $ I32CompareOp Int.GeS
    0x4F -> return $ Compare $ I32CompareOp Int.GeU

    -- Comparison operators for 64-bit integers.
    0x51 -> return $ Compare $ I64CompareOp Int.Eq
    0x52 -> return $ Compare $ I64CompareOp Int.Ne
    0x53 -> return $ Compare $ I64CompareOp Int.LtS
    0x54 -> return $ Compare $ I64CompareOp Int.LtU
    0x55 -> return $ Compare $ I64CompareOp Int.GtS
    0x56 -> return $ Compare $ I64CompareOp Int.GtU
    0x57 -> return $ Compare $ I64CompareOp Int.LeS
    0x58 -> return $ Compare $ I64CompareOp Int.LeU
    0x59 -> return $ Compare $ I64CompareOp Int.GeS
    0x5A -> return $ Compare $ I64CompareOp Int.GeU

    -- Comparison operators for 32-bit floats.
    0x5B -> return $ Compare $ F32CompareOp Float.Eq
    0x5C -> return $ Compare $ F32CompareOp Float.Ne
    0x5D -> return $ Compare $ F32CompareOp Float.Lt
    0x5E -> return $ Compare $ F32CompareOp Float.Gt
    0x5F -> return $ Compare $ F32CompareOp Float.Le
    0x60 -> return $ Compare $ F32CompareOp Float.Ge

    -- Comparison operators for 64-bit floats.
    0x61 -> return $ Compare $ F64CompareOp Float.Eq
    0x62 -> return $ Compare $ F64CompareOp Float.Ne
    0x63 -> return $ Compare $ F64CompareOp Float.Lt
    0x64 -> return $ Compare $ F64CompareOp Float.Gt
    0x65 -> return $ Compare $ F64CompareOp Float.Le
    0x66 -> return $ Compare $ F64CompareOp Float.Ge

    -- Conversion operators for 32-bit integers.
    0xA7 -> return $ Convert $ I32ConvertOp Int.WrapI64
    0xA8 -> return $ Convert $ I32ConvertOp Int.TruncSF32
    0xA9 -> return $ Convert $ I32ConvertOp Int.TruncUF32
    0xAA -> return $ Convert $ I32ConvertOp Int.TruncSF64
    0xAB -> return $ Convert $ I32ConvertOp Int.TruncUF64
    0xBC -> return $ Convert $ I32ConvertOp Int.ReinterpretFloat

    -- Conversion operators for 64-bit integers.
    0xAC -> return $ Convert $ I64ConvertOp Int.ExtendSI32
    0xAD -> return $ Convert $ I64ConvertOp Int.ExtendUI32
    0xAE -> return $ Convert $ I64ConvertOp Int.TruncSF32
    0xAF -> return $ Convert $ I64ConvertOp Int.TruncUF32
    0xB0 -> return $ Convert $ I64ConvertOp Int.TruncSF64
    0xB1 -> return $ Convert $ I64ConvertOp Int.TruncUF64
    0xBD -> return $ Convert $ I64ConvertOp Int.ReinterpretFloat

    -- Conversion operators for 32-bit floats.
    0xB2 -> return $ Convert $ F32ConvertOp Float.ConvertSI32
    0xB3 -> return $ Convert $ F32ConvertOp Float.ConvertUI32
    0xB4 -> return $ Convert $ F32ConvertOp Float.ConvertSI64
    0xB5 -> return $ Convert $ F32ConvertOp Float.ConvertUI64
    0xB6 -> return $ Convert $ F32ConvertOp Float.DemoteF64
    0xBE -> return $ Convert $ F32ConvertOp Float.ReinterpretInt

    -- Conversion operators for 64-bit floats.
    0xB7 -> return $ Convert $ F64ConvertOp Float.ConvertSI32
    0xB8 -> return $ Convert $ F64ConvertOp Float.ConvertUI32
    0xB9 -> return $ Convert $ F64ConvertOp Float.ConvertSI64
    0xBA -> return $ Convert $ F64ConvertOp Float.ConvertUI64
    0xBB -> return $ Convert $ F64ConvertOp Float.PromoteF32
    0xBF -> return $ Convert $ F64ConvertOp Float.ReinterpretInt

    0xFC -> getMathPrefix

    -- Sign extension operators
    0xC0 -> return $ Unary $ I32UnaryOp (Int.ExtendS Pack8)
    0xC1 -> return $ Unary $ I32UnaryOp (Int.ExtendS Pack16)
    0xC2 -> return $ Unary $ I64UnaryOp (Int.ExtendS Pack8)
    0xC3 -> return $ Unary $ I64UnaryOp (Int.ExtendS Pack16)
    0xC4 -> return $ Unary $ I64UnaryOp (Int.ExtendS Pack32)


    -- Unexpected instruction.
    _    -> fail $ printf "getInstr: unexpected instruction: 0x%02X" byte

{-# SPECIALIZE getInstr :: Get (Instr Identity) #-}
{-# SPECIALIZE getInstr :: Get (Instr Phrase) #-}

getInstrBlock :: Decodable phrase => Get [phrase (Instr phrase)]
getInstrBlock = go []
 where
  go accum = do
    byte <- lookAhead getWord8
    if byte == 0x0B || byte == 0x05
      then return $ List.reverse accum
      else do
        i <- liftGet getInstr
        go $ i : accum

{-# SPECIALIZE getInstrBlock :: Get [Identity (Instr Identity)] #-}
{-# SPECIALIZE getInstrBlock :: Get [Phrase (Instr Phrase)] #-}

getExpression :: Decodable phrase => Get (Expr phrase)
getExpression = do
  expr <- liftGet getInstrBlock
  byteGuard 0x0B
  return expr

{-# SPECIALIZE getExpression :: Get (Expr Identity) #-}
{-# SPECIALIZE getExpression :: Get (Expr Phrase) #-}

getAlternative :: Decodable phrase => Get [phrase (Instr phrase)]
getAlternative = do
  byteGuard 0x05
  getInstrBlock

{-# SPECIALIZE getAlternative :: Get [Identity (Instr Identity)] #-}
{-# SPECIALIZE getAlternative :: Get [Phrase (Instr Phrase)] #-}

getMagic :: Get Word32
getMagic = do
  magic <- getWord32be
  if magic == 0x0061736D
    then return magic
    else fail $ printf "getMagic: invalid magic: 0x%08X" magic

getVersion :: Get Word32
getVersion = do
  version <- getWord32be
  if version == 0x01000000
    then return version
    else fail $ printf "getVersion: invalid version: 0x%08X" version

getSection :: Word8 -> section -> Get section -> SectionT Get section
getSection code def parser = do
  done <- lift isEmpty
  if done
    then return def
    else do
      byte <- lift $ lookAhead $ getULEB128 7
      if byte == 0x00
        then do
          getCustom
          getSection code def parser
        else if
          | byte > 0x0B -> fail
          $ printf "getSection: malformed section code: 0x%02X" byte
          | byte > code -> return def
          | byte < code -> fail
          $ printf "getSection: unexpected section code: 0x%02X" byte
          | otherwise -> lift $ do
            byteGuard code
            size <- getULEB128 32
            sizeGuard size parser

getTypes :: Decodable phrase => SectionT Get [Type phrase]
getTypes = getSection 0x01 [] $ getList 32 $ liftGet $ getFuncType

{-# SPECIALIZE getTypes :: SectionT Get [Type Identity] #-}
{-# SPECIALIZE getTypes :: SectionT Get [Type Phrase] #-}

getImports :: Decodable phrase => SectionT Get [phrase (Import phrase)]
getImports = getSection 0x02 [] $ getList 32 $ liftGet getImport

{-# SPECIALIZE getImports :: SectionT Get [Identity (Import Identity)] #-}
{-# SPECIALIZE getImports :: SectionT Get [Phrase (Import Phrase)] #-}

getFuncTypes :: Decodable phrase => SectionT Get [Var phrase]
getFuncTypes = getSection 0x03 [] $ getList 32 getVar

{-# SPECIALIZE getFuncTypes :: SectionT Get [Var Identity] #-}
{-# SPECIALIZE getFuncTypes :: SectionT Get [Var Phrase] #-}

getTables :: Decodable phrase => SectionT Get [Table phrase]
getTables = getSection 0x04 [] $ getList 32 $ liftGet getTableType

{-# SPECIALIZE getTables :: SectionT Get [Table Identity] #-}
{-# SPECIALIZE getTables :: SectionT Get [Table Phrase] #-}

getMemories :: Decodable phrase => SectionT Get [Memory phrase]
getMemories = getSection 0x05 [] $ getList 32 $ liftGet getMemoryType

{-# SPECIALIZE getMemories :: SectionT Get [Memory Identity] #-}
{-# SPECIALIZE getMemories :: SectionT Get [Memory Phrase] #-}

getGlobals :: Decodable phrase => SectionT Get [phrase (Global phrase)]
getGlobals = getSection 0x06 [] $ getList 32 $ liftGet getGlobal

{-# SPECIALIZE getGlobals :: SectionT Get [Identity (Global Identity)] #-}
{-# SPECIALIZE getGlobals :: SectionT Get [Phrase (Global Phrase)] #-}

getExports :: Decodable phrase => SectionT Get [phrase (Export phrase)]
getExports = getSection 0x07 [] $ getList 32 $ liftGet getExport

{-# SPECIALIZE getExports :: SectionT Get [Identity (Export Identity)] #-}
{-# SPECIALIZE getExports :: SectionT Get [Phrase (Export Phrase)] #-}

getStart :: Decodable phrase => SectionT Get (Maybe (Var phrase))
getStart = getSection 0x08 Nothing $ Just <$> getVar

{-# SPECIALIZE getStart :: SectionT Get (Maybe (Var Identity)) #-}
{-# SPECIALIZE getStart :: SectionT Get (Maybe (Var Phrase)) #-}

getElems :: Decodable phrase => SectionT Get [phrase (TableSegment phrase)]
getElems = getSection 0x09 [] $ getList 32 $ liftGet getTableSegment

{-# SPECIALIZE getElems :: SectionT Get [Identity (TableSegment Identity)] #-}
{-# SPECIALIZE getElems :: SectionT Get [Phrase (TableSegment Phrase)] #-}

getCode :: Decodable phrase => Int -> SectionT Get [phrase (Func phrase)]
getCode target = getSection 0x0A [] $ check <=< getList 32 $ liftGet getFunc
 where
  check result = if List.length result == target
    then return result
    else fail "getCode: inconsistent result"

{-# SPECIALIZE getCode :: Int -> SectionT Get [Identity (Func Identity)] #-}
{-# SPECIALIZE getCode :: Int -> SectionT Get [Phrase (Func Phrase)] #-}

getDatas :: Decodable phrase => SectionT Get [phrase (MemorySegment phrase)]
getDatas = getSection 0x0B [] $ getList 32 $ liftGet getMemorySegment

{-# SPECIALIZE getDatas :: SectionT Get [Identity (MemorySegment Identity)] #-}
{-# SPECIALIZE getDatas :: SectionT Get [Phrase (MemorySegment Phrase)] #-}

getCustom :: SectionT Get ()
getCustom = modify <=< lift $ do
  byteGuard 0x00
  size <- getULEB128 32
  sizeGuard size $ do
    before      <- bytesRead
    _customName <- getText 32
    after       <- bytesRead
    let delta = fromIntegral $ after - before
    _customPayload <- getByteStream $ size - delta
    return $ (:) Custom {..}

getModule :: Decodable phrase => Get (Module phrase)
getModule = do
  void getMagic
  void getVersion
  fmap custom $ flip runStateT [] $ do
    types     <- V.fromList <$> getTypes
    imports   <- V.fromList <$> getImports
    funcTypes <- V.fromList <$> getFuncTypes
    tables    <- V.fromList <$> getTables
    memories  <- V.fromList <$> getMemories
    globals   <- V.fromList <$> getGlobals
    exports   <- V.fromList <$> getExports
    start     <- getStart
    elems     <- V.fromList <$> getElems
    code      <- V.fromList <$> (getCode $ fromIntegral $ V.length funcTypes)
    datas     <- V.fromList <$> getDatas
    getSection 0x00 () App.empty
    done <- lift isEmpty
    unless done $ fail "getModule: residual input"
    let funcs = V.zipWith zipper funcTypes code
    return $ Module types
                    globals
                    tables
                    memories
                    funcs
                    start
                    elems
                    datas
                    imports
                    exports
 where
  custom = uncurry $ \f -> f . V.reverse . V.fromList
  zipper _funcType = fmap $ \func -> func { _funcType }

{-# SPECIALIZE getModule :: Get (Module Identity) #-}
{-# SPECIALIZE getModule :: Get (Module Phrase) #-}
