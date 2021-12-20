{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Wasm.Binary.Encode where

import           Data.Binary.Put
import           Data.Bool
import           Data.ByteString.Lazy          as Byte
import           Data.Fix
import           Data.Functor.Classes
import           Data.Functor.Identity
import           Data.Int
import           Data.List                     as List
import qualified Data.Vector                   as V
import           Data.Maybe
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Data.Word

import           Wasm.Binary.LEB128
import           Wasm.Binary.Lift
import           Wasm.Syntax.AST
import           Wasm.Syntax.Ops
import           Wasm.Syntax.Ops.Int   as Int
import           Wasm.Syntax.Ops.Float as Float
import           Wasm.Syntax.Types
import           Wasm.Syntax.Values
import           Wasm.Util.Source

class (Put1 phrase, Show1 phrase, Traversable phrase) => Encodable phrase where

instance Encodable Identity

instance Encodable Phrase

putVector :: (a -> Put) -> V.Vector a -> Put
putVector putValue values = do
  putULEB128 $ V.length values
  mapM_ putValue values

putList :: (a -> Put) -> [a] -> Put
putList putValue = putVector putValue . V.fromList

putByteSlice :: ByteString -> Put
putByteSlice stream = do
  putULEB128 $ Byte.length stream
  putByteStream stream

putByteStream :: ByteString -> Put
putByteStream = putLazyByteString

putText :: Text -> Put
putText = putByteSlice . encodeUtf8

putValueType :: ValueType -> Put
putValueType = \case
  I32Type -> putWord8 0x7F
  I64Type -> putWord8 0x7E
  F32Type -> putWord8 0x7D
  F64Type -> putWord8 0x7C

putElemType :: ElemType -> Put
putElemType AnyFuncType = putWord8 0x70

putStackType :: StackType -> Put
putStackType = putList putValueType

putFuncType :: FuncType -> Put
putFuncType FuncType {..} = do
  putWord8 0x60
  putStackType _funcInput
  putStackType _funcOutput

putLimits :: Limits Int32 -> Put
putLimits Limits {..} = do
  putWord8 flag
  putULEB128 _limitLower
  if flag == 0x00 then mempty else putULEB128 $ fromJust _limitUpper
  where flag = bool 0x00 0x01 $ isJust _limitUpper

putMutability :: Mutability -> Put
putMutability = \case
  Immutable -> putWord8 0x00
  Mutable   -> putWord8 0x01

putTableType :: TableType -> Put
putTableType TableType {..} = do
  putElemType _tableElemType
  putLimits _tableLimits

putMemoryType :: MemoryType -> Put
putMemoryType = putLimits

putGlobalType :: GlobalType -> Put
putGlobalType GlobalType {..} = do
  putValueType _globalValueType
  putMutability _globalMutability

putVar :: Encodable phrase => Var phrase -> Put
putVar = liftPut putULEB128

{-# SPECIALIZE putVar :: Var Identity -> Put #-}
{-# SPECIALIZE putVar :: Var Phrase -> Put #-}

putBlockType :: Encodable phrase => BlockType phrase -> Put
putBlockType = \case
  VarBlockType x -> putVar x
  ValBlockType Nothing -> putWord8 0x40
  ValBlockType (Just t) -> putValueType t

putGlobal :: Encodable phrase => Global phrase -> Put
putGlobal Global {..} = do
  putGlobalType _globalType
  putExpression _globalValue

{-# SPECIALIZE putGlobal :: Global Identity -> Put #-}
{-# SPECIALIZE putGlobal :: Global Phrase -> Put #-}

putFunc :: Encodable phrase => Func phrase -> Put
putFunc Func {..} = do
  putULEB128 $ Byte.length stream
  putByteStream stream
 where
  stream = runPut $ do
    putLocals _funcLocals
    putInstrBlock _funcBody
    putWord8 0x0B

{-# SPECIALIZE putFunc :: Func Identity -> Put #-}
{-# SPECIALIZE putFunc :: Func Phrase -> Put #-}

putLocals :: [ValueType] -> Put
putLocals locals = do
  putULEB128 $ List.length groups
  mapM_ putLocal groups
 where
  groups = List.group $ sort locals
  putLocal list@(valueType : _) = do
    putULEB128 $ List.length list
    putValueType valueType
  putLocal _ = error "putLocals: impossible"

putTableSegment :: Encodable phrase => TableSegment phrase -> Put
putTableSegment Segment {..} = do
  putVar _segmentIndex
  putExpression _segmentOffset
  putList putVar _segmentInit

{-# SPECIALIZE putTableSegment :: TableSegment Identity -> Put #-}
{-# SPECIALIZE putTableSegment :: TableSegment Phrase -> Put #-}

putMemorySegment :: Encodable phrase => MemorySegment phrase -> Put
putMemorySegment Segment {..} = do
  putVar _segmentIndex
  putExpression _segmentOffset
  putByteSlice _segmentInit

{-# SPECIALIZE putMemorySegment :: MemorySegment Identity -> Put #-}
{-# SPECIALIZE putMemorySegment :: MemorySegment Phrase -> Put #-}

putImportDesc :: Encodable phrase => ImportDesc phrase -> Put
putImportDesc = \case
  FuncImport var -> do
    putWord8 0x00
    putVar var
  TableImport table -> do
    putWord8 0x01
    putTableType table
  MemoryImport memory -> do
    putWord8 0x02
    putMemoryType memory
  GlobalImport global -> do
    putWord8 0x03
    putGlobalType global

{-# SPECIALIZE putImportDesc :: ImportDesc Identity -> Put #-}
{-# SPECIALIZE putImportDesc :: ImportDesc Phrase -> Put #-}

putImport :: Encodable phrase => Import phrase -> Put
putImport Import {..} = do
  putText _importModule
  putText _importItem
  putImportDesc _importDesc

{-# SPECIALIZE putImport :: Import Identity -> Put #-}
{-# SPECIALIZE putImport :: Import Phrase -> Put #-}

putExportDesc :: Encodable phrase => ExportDesc phrase -> Put
putExportDesc = \case
  FuncExport var -> do
    putWord8 0x00
    putVar var
  TableExport var -> do
    putWord8 0x01
    putVar var
  MemoryExport var -> do
    putWord8 0x02
    putVar var
  GlobalExport var -> do
    putWord8 0x03
    putVar var

{-# SPECIALIZE putExportDesc :: ExportDesc Identity -> Put #-}
{-# SPECIALIZE putExportDesc :: ExportDesc Phrase -> Put #-}

putExport :: Encodable phrase => Export phrase -> Put
putExport Export {..} = do
  putText _exportName
  putExportDesc _exportDesc

{-# SPECIALIZE putExport :: Export Identity -> Put #-}
{-# SPECIALIZE putExport :: Export Phrase -> Put #-}

putMemoryOp :: MemoryOp size -> Put
putMemoryOp MemoryOp {..} = do
  putULEB128 _memoryAlignment
  putULEB128 _memoryOffset

putInstr :: Encodable phrase => Instr phrase -> Put
putInstr = flip (.) unFix $ \case

  -- Control flow operators.
  Unreachable       -> putWord8 0x00
  Nop               -> putWord8 0x01
  Block result expr -> do
    putWord8 0x02
    putBlockType result
    putInstrBlock expr
    putWord8 0x0B
  Loop result expr -> do
    putWord8 0x03
    putBlockType result
    putInstrBlock expr
    putWord8 0x0B
  If result consequent alternative -> do
    putWord8 0x04
    putBlockType result
    putInstrBlock consequent
    putAlternative alternative
    putWord8 0x0B
  Br var -> do
    putWord8 0x0C
    putVar var
  BrIf var -> do
    putWord8 0x0D
    putVar var
  BrTable table var -> do
    putWord8 0x0E
    putList putVar table
    putVar var
  Return   -> putWord8 0x0F

  -- Call operators.
  Call var -> do
    putWord8 0x10
    putVar var
  CallIndirect var -> do
    putWord8 0x11
    putVar var
    putWord8 0x00

  -- Parametric operators.
  Drop         -> putWord8 0x1A
  Select       -> putWord8 0x1B

  -- Variable access.
  GetLocal var -> do
    putWord8 0x20
    putVar var
  SetLocal var -> do
    putWord8 0x21
    putVar var
  TeeLocal var -> do
    putWord8 0x22
    putVar var
  GetGlobal var -> do
    putWord8 0x23
    putVar var
  SetGlobal var -> do
    putWord8 0x24
    putVar var

  -- Load operators.
  Load op@MemoryOp {..}
    | _memoryValueType == I32Type && isNothing _memorySize -> do
      putWord8 0x28
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I64Type && isNothing _memorySize -> do
      putWord8 0x29
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == F32Type && isNothing _memorySize -> do
      putWord8 0x2A
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == F64Type && isNothing _memorySize -> do
      putWord8 0x2B
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I32Type && _memorySize == Just (Pack8, SX) -> do
      putWord8 0x2C
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I32Type && _memorySize == Just (Pack8, ZX) -> do
      putWord8 0x2D
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I32Type && _memorySize == Just (Pack16, SX) -> do
      putWord8 0x2E
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I32Type && _memorySize == Just (Pack16, ZX) -> do
      putWord8 0x2F
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just (Pack8, SX) -> do
      putWord8 0x30
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just (Pack8, ZX) -> do
      putWord8 0x31
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just (Pack16, SX) -> do
      putWord8 0x32
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just (Pack16, ZX) -> do
      putWord8 0x33
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just (Pack32, SX) -> do
      putWord8 0x34
      putMemoryOp op
  Load op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just (Pack32, ZX) -> do
      putWord8 0x35
      putMemoryOp op

  -- Store operators.
  Store op@MemoryOp {..}
    | _memoryValueType == I32Type && isNothing _memorySize -> do
      putWord8 0x36
      putMemoryOp op
  Store op@MemoryOp {..}
    | _memoryValueType == I64Type && isNothing _memorySize -> do
      putWord8 0x37
      putMemoryOp op
  Store op@MemoryOp {..}
    | _memoryValueType == F32Type && isNothing _memorySize -> do
      putWord8 0x38
      putMemoryOp op
  Store op@MemoryOp {..}
    | _memoryValueType == F64Type && isNothing _memorySize -> do
      putWord8 0x39
      putMemoryOp op
  Store op@MemoryOp {..}
    | _memoryValueType == I32Type && _memorySize == Just Pack8 -> do
      putWord8 0x3A
      putMemoryOp op
  Store op@MemoryOp {..}
    | _memoryValueType == I32Type && _memorySize == Just Pack16 -> do
      putWord8 0x3B
      putMemoryOp op
  Store op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just Pack8 -> do
      putWord8 0x3C
      putMemoryOp op
  Store op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just Pack16 -> do
      putWord8 0x3D
      putMemoryOp op
  Store op@MemoryOp {..}
    | _memoryValueType == I64Type && _memorySize == Just Pack32 -> do
      putWord8 0x3E
      putMemoryOp op

  -- Memory operators.
  MemorySize -> do
    putWord8 0x3F
    putWord8 0x00
  MemoryGrow -> do
    putWord8 0x40
    putWord8 0x00
  MemoryFill -> do
    mapM_ putWord8 [0xFC, 0x0B, 0x00]
  MemoryCopy -> do
    mapM_ putWord8 [0xFC, 0x0A, 0x00, 0x00]

  -- Constants.
  Const (value -> I32 val) -> do
    putWord8 0x41
    putSLEB128 val
  Const (value -> I64 val) -> do
    putWord8 0x42
    putSLEB128 val
  Const (value -> F32 val) -> do
    putWord8 0x43
    putFloatle val
  Const (value -> F64 val) -> do
    putWord8 0x44
    putDoublele val

  -- Unary operators for 32-bit integers.
  Unary (I32UnaryOp Int.Clz) -> putWord8 0x67
  Unary (I32UnaryOp Int.Ctz) -> putWord8 0x68
  Unary (I32UnaryOp Int.Popcnt) -> putWord8 0x69

  -- Unary operators for 64-bit integers.
  Unary (I64UnaryOp Int.Clz) -> putWord8 0x79
  Unary (I64UnaryOp Int.Ctz) -> putWord8 0x7A
  Unary (I64UnaryOp Int.Popcnt) -> putWord8 0x7B

  -- Unary operators for 32-bit floats.
  Unary (F32UnaryOp Float.Abs) -> putWord8 0x8B
  Unary (F32UnaryOp Float.Neg) -> putWord8 0x8C
  Unary (F32UnaryOp Float.Ceil) -> putWord8 0x8D
  Unary (F32UnaryOp Float.Floor) -> putWord8 0x8E
  Unary (F32UnaryOp Float.Trunc) -> putWord8 0x8F
  Unary (F32UnaryOp Float.Nearest) -> putWord8 0x90
  Unary (F32UnaryOp Float.Sqrt) -> putWord8 0x91

  -- Unary operators for 64-bit floats.
  Unary (F64UnaryOp Float.Abs) -> putWord8 0x99
  Unary (F64UnaryOp Float.Neg) -> putWord8 0x9A
  Unary (F64UnaryOp Float.Ceil) -> putWord8 0x9B
  Unary (F64UnaryOp Float.Floor) -> putWord8 0x9C
  Unary (F64UnaryOp Float.Trunc) -> putWord8 0x9D
  Unary (F64UnaryOp Float.Nearest) -> putWord8 0x9E
  Unary (F64UnaryOp Float.Sqrt) -> putWord8 0x9F

  -- Binary operators for 32-bit integers.
  Binary (I32BinaryOp Int.Add) -> putWord8 0x6A
  Binary (I32BinaryOp Int.Sub) -> putWord8 0x6B
  Binary (I32BinaryOp Int.Mul) -> putWord8 0x6C
  Binary (I32BinaryOp Int.DivS) -> putWord8 0x6D
  Binary (I32BinaryOp Int.DivU) -> putWord8 0x6E
  Binary (I32BinaryOp Int.RemS) -> putWord8 0x6F
  Binary (I32BinaryOp Int.RemU) -> putWord8 0x70
  Binary (I32BinaryOp Int.And) -> putWord8 0x71
  Binary (I32BinaryOp Int.Or) -> putWord8 0x72
  Binary (I32BinaryOp Int.Xor) -> putWord8 0x73
  Binary (I32BinaryOp Int.Shl) -> putWord8 0x74
  Binary (I32BinaryOp Int.ShrS) -> putWord8 0x75
  Binary (I32BinaryOp Int.ShrU) -> putWord8 0x76
  Binary (I32BinaryOp Int.Rotl) -> putWord8 0x77
  Binary (I32BinaryOp Int.Rotr) -> putWord8 0x78

  -- Binary operators for 64-bit integers.
  Binary (I64BinaryOp Int.Add) -> putWord8 0x7C
  Binary (I64BinaryOp Int.Sub) -> putWord8 0x7D
  Binary (I64BinaryOp Int.Mul) -> putWord8 0x7E
  Binary (I64BinaryOp Int.DivS) -> putWord8 0x7F
  Binary (I64BinaryOp Int.DivU) -> putWord8 0x80
  Binary (I64BinaryOp Int.RemS) -> putWord8 0x81
  Binary (I64BinaryOp Int.RemU) -> putWord8 0x82
  Binary (I64BinaryOp Int.And) -> putWord8 0x83
  Binary (I64BinaryOp Int.Or) -> putWord8 0x84
  Binary (I64BinaryOp Int.Xor) -> putWord8 0x85
  Binary (I64BinaryOp Int.Shl) -> putWord8 0x86
  Binary (I64BinaryOp Int.ShrS) -> putWord8 0x87
  Binary (I64BinaryOp Int.ShrU) -> putWord8 0x88
  Binary (I64BinaryOp Int.Rotl) -> putWord8 0x89
  Binary (I64BinaryOp Int.Rotr) -> putWord8 0x8A

  -- Binary operators for 32-bit floats.
  Binary (F32BinaryOp Float.Add) -> putWord8 0x92
  Binary (F32BinaryOp Float.Sub) -> putWord8 0x93
  Binary (F32BinaryOp Float.Mul) -> putWord8 0x94
  Binary (F32BinaryOp Float.Div) -> putWord8 0x95
  Binary (F32BinaryOp Float.Min) -> putWord8 0x96
  Binary (F32BinaryOp Float.Max) -> putWord8 0x97
  Binary (F32BinaryOp Float.CopySign) -> putWord8 0x98

  -- Binary operators for 64-bit floats.
  Binary (F64BinaryOp Float.Add) -> putWord8 0xA0
  Binary (F64BinaryOp Float.Sub) -> putWord8 0xA1
  Binary (F64BinaryOp Float.Mul) -> putWord8 0xA2
  Binary (F64BinaryOp Float.Div) -> putWord8 0xA3
  Binary (F64BinaryOp Float.Min) -> putWord8 0xA4
  Binary (F64BinaryOp Float.Max) -> putWord8 0xA5
  Binary (F64BinaryOp Float.CopySign) -> putWord8 0xA6

  -- Test operators for 32-bit integers.
  Test (I32TestOp Int.Eqz) -> putWord8 0x45

  -- Test operators for 64-bit integers.
  Test (I64TestOp Int.Eqz) -> putWord8 0x50

  -- Comparison operators for 32-bit integers.
  Compare (I32CompareOp Int.Eq) -> putWord8 0x46
  Compare (I32CompareOp Int.Ne) -> putWord8 0x47
  Compare (I32CompareOp Int.LtS) -> putWord8 0x48
  Compare (I32CompareOp Int.LtU) -> putWord8 0x49
  Compare (I32CompareOp Int.GtS) -> putWord8 0x4A
  Compare (I32CompareOp Int.GtU) -> putWord8 0x4B
  Compare (I32CompareOp Int.LeS) -> putWord8 0x4C
  Compare (I32CompareOp Int.LeU) -> putWord8 0x4D
  Compare (I32CompareOp Int.GeS) -> putWord8 0x4E
  Compare (I32CompareOp Int.GeU) -> putWord8 0x4F

  -- Comparison operators for 64-bit integers.
  Compare (I64CompareOp Int.Eq) -> putWord8 0x51
  Compare (I64CompareOp Int.Ne) -> putWord8 0x52
  Compare (I64CompareOp Int.LtS) -> putWord8 0x53
  Compare (I64CompareOp Int.LtU) -> putWord8 0x54
  Compare (I64CompareOp Int.GtS) -> putWord8 0x55
  Compare (I64CompareOp Int.GtU) -> putWord8 0x56
  Compare (I64CompareOp Int.LeS) -> putWord8 0x57
  Compare (I64CompareOp Int.LeU) -> putWord8 0x58
  Compare (I64CompareOp Int.GeS) -> putWord8 0x59
  Compare (I64CompareOp Int.GeU) -> putWord8 0x5A

  -- Comparison operators for 32-bit floats.
  Compare (F32CompareOp Float.Eq) -> putWord8 0x5B
  Compare (F32CompareOp Float.Ne) -> putWord8 0x5C
  Compare (F32CompareOp Float.Lt) -> putWord8 0x5D
  Compare (F32CompareOp Float.Gt) -> putWord8 0x5E
  Compare (F32CompareOp Float.Le) -> putWord8 0x5F
  Compare (F32CompareOp Float.Ge) -> putWord8 0x60

  -- Comparison operators for 64-bit floats.
  Compare (F64CompareOp Float.Eq) -> putWord8 0x61
  Compare (F64CompareOp Float.Ne) -> putWord8 0x62
  Compare (F64CompareOp Float.Lt) -> putWord8 0x63
  Compare (F64CompareOp Float.Gt) -> putWord8 0x64
  Compare (F64CompareOp Float.Le) -> putWord8 0x65
  Compare (F64CompareOp Float.Ge) -> putWord8 0x66

  -- Conversion operators for 32-bit integers.
  Convert (I32ConvertOp Int.WrapI64) -> putWord8 0xA7
  Convert (I32ConvertOp Int.TruncSF32) -> putWord8 0xA8
  Convert (I32ConvertOp Int.TruncUF32) -> putWord8 0xA9
  Convert (I32ConvertOp Int.TruncSF64) -> putWord8 0xAA
  Convert (I32ConvertOp Int.TruncUF64) -> putWord8 0xAB
  Convert (I32ConvertOp Int.ReinterpretFloat) -> putWord8 0xBC

  -- Conversion operators for 64-bit integers.
  Convert (I64ConvertOp Int.ExtendSI32) -> putWord8 0xAC
  Convert (I64ConvertOp Int.ExtendUI32) -> putWord8 0xAD
  Convert (I64ConvertOp Int.TruncSF32) -> putWord8 0xAE
  Convert (I64ConvertOp Int.TruncUF32) -> putWord8 0xAF
  Convert (I64ConvertOp Int.TruncSF64) -> putWord8 0xB0
  Convert (I64ConvertOp Int.TruncUF64) -> putWord8 0xB1
  Convert (I64ConvertOp Int.ReinterpretFloat) -> putWord8 0xBD

  -- Conversion operators for 32-bit floats.
  Convert (F32ConvertOp Float.ConvertSI32) -> putWord8 0xB2
  Convert (F32ConvertOp Float.ConvertUI32) -> putWord8 0xB3
  Convert (F32ConvertOp Float.ConvertSI64) -> putWord8 0xB4
  Convert (F32ConvertOp Float.ConvertUI64) -> putWord8 0xB5
  Convert (F32ConvertOp Float.DemoteF64) -> putWord8 0xB6
  Convert (F32ConvertOp Float.ReinterpretInt) -> putWord8 0xBE

  -- Conversion operators for 64-bit floats.
  Convert (F64ConvertOp Float.ConvertSI32) -> putWord8 0xB7
  Convert (F64ConvertOp Float.ConvertUI32) -> putWord8 0xB8
  Convert (F64ConvertOp Float.ConvertSI64) -> putWord8 0xB9
  Convert (F64ConvertOp Float.ConvertUI64) -> putWord8 0xBA
  Convert (F64ConvertOp Float.PromoteF32) -> putWord8 0xBB
  Convert (F64ConvertOp Float.ReinterpretInt) -> putWord8 0xBF

  op -> error $ "putInstr: invalid operator: " ++ show op

{-# SPECIALIZE putInstr :: Instr Identity -> Put #-}
{-# SPECIALIZE putInstr :: Instr Phrase -> Put #-}

putInstrBlock :: Encodable phrase => [phrase (Instr phrase)] -> Put
putInstrBlock = mapM_ $ liftPut putInstr

{-# SPECIALIZE putInstrBlock :: [Identity (Instr Identity)] -> Put #-}
{-# SPECIALIZE putInstrBlock :: [Phrase (Instr Phrase)] -> Put #-}

putExpression :: Encodable phrase => Expr phrase -> Put
putExpression expr = do
  liftPut putInstrBlock expr
  putWord8 0x0B

{-# SPECIALIZE putExpression :: Expr Identity -> Put #-}
{-# SPECIALIZE putExpression :: Expr Phrase -> Put #-}

putAlternative :: Encodable phrase => [phrase (Instr phrase)] -> Put
putAlternative alternative = if List.null alternative
  then mempty
  else do
    putWord8 0x05
    putInstrBlock alternative

{-# SPECIALIZE putAlternative :: [Identity (Instr Identity)] -> Put #-}
{-# SPECIALIZE putAlternative :: [Phrase (Instr Phrase)] -> Put #-}

putMagic :: Put
putMagic = putWord32be 0x0061736D

putVersion :: Put
putVersion = putWord32be 0x01000000

putSection :: Word8 -> Put -> Put
putSection code writer = do
  putWord8 code
  putULEB128 $ Byte.length stream
  putByteStream stream
  where stream = runPut writer

putTypes :: Encodable phrase => V.Vector (Type phrase) -> Put
putTypes = putSection 0x01 . putVector (liftPut putFuncType)

{-# SPECIALIZE putTypes :: V.Vector (Type Identity) -> Put #-}
{-# SPECIALIZE putTypes :: V.Vector (Type Phrase) -> Put #-}

putImports :: Encodable phrase => V.Vector (phrase (Import phrase)) -> Put
putImports = putSection 0x02 . putVector (liftPut putImport)

{-# SPECIALIZE putImports :: V.Vector (Identity (Import Identity)) -> Put #-}
{-# SPECIALIZE putImports :: V.Vector (Phrase (Import Phrase)) -> Put #-}

putFuncTypes :: Encodable phrase => V.Vector (phrase (Func phrase)) -> Put
putFuncTypes = putSection 0x03 . putVector (liftPut (putVar . _funcType))

{-# SPECIALIZE putFuncTypes :: V.Vector (Identity (Func Identity)) -> Put #-}
{-# SPECIALIZE putFuncTypes :: V.Vector (Phrase (Func Phrase)) -> Put #-}

putTables :: Encodable phrase => V.Vector (Table phrase) -> Put
putTables = putSection 0x04 . putVector (liftPut putTableType)

{-# SPECIALIZE putTables :: V.Vector (Table Identity) -> Put #-}
{-# SPECIALIZE putTables :: V.Vector (Table Phrase) -> Put #-}

putMemories :: Encodable phrase => V.Vector (Memory phrase) -> Put
putMemories = putSection 0x05 . putVector (liftPut putMemoryType)

{-# SPECIALIZE putMemories :: V.Vector (Memory Identity) -> Put #-}
{-# SPECIALIZE putMemories :: V.Vector (Memory Phrase) -> Put #-}

putGlobals :: Encodable phrase => V.Vector (phrase (Global phrase)) -> Put
putGlobals = putSection 0x06 . putVector (liftPut putGlobal)

{-# SPECIALIZE putGlobals :: V.Vector (Identity (Global Identity)) -> Put #-}
{-# SPECIALIZE putGlobals :: V.Vector (Phrase (Global Phrase)) -> Put #-}

putExports :: Encodable phrase => V.Vector (phrase (Export phrase)) -> Put
putExports = putSection 0x07 . putVector (liftPut putExport)

{-# SPECIALIZE putExports :: V.Vector (Identity (Export Identity)) -> Put #-}
{-# SPECIALIZE putExports :: V.Vector (Phrase (Export Phrase)) -> Put #-}

putStart :: Encodable phrase => Maybe (Var phrase) -> Put
putStart = maybe mempty $ putSection 0x08 . putVar

{-# SPECIALIZE putStart :: Maybe (Var Identity) -> Put #-}
{-# SPECIALIZE putStart :: Maybe (Var Phrase) -> Put #-}

putElems :: Encodable phrase => V.Vector (phrase (TableSegment phrase)) -> Put
putElems = putSection 0x09 . putVector (liftPut putTableSegment)

{-# SPECIALIZE putElems :: V.Vector (Identity (TableSegment Identity)) -> Put #-}
{-# SPECIALIZE putElems :: V.Vector (Phrase (TableSegment Phrase)) -> Put #-}

putDatas :: Encodable phrase => V.Vector (phrase (MemorySegment phrase)) -> Put
putDatas = putSection 0x0B . putVector (liftPut putMemorySegment)

{-# SPECIALIZE putDatas :: V.Vector (Identity (MemorySegment Identity)) -> Put #-}
{-# SPECIALIZE putDatas :: V.Vector (Phrase (MemorySegment Phrase)) -> Put #-}

putCode :: Encodable phrase => V.Vector (phrase (Func phrase)) -> Put
putCode = putSection 0x0A . putVector (liftPut putFunc)

{-# SPECIALIZE putCode :: V.Vector (Identity (Func Identity)) -> Put #-}
{-# SPECIALIZE putCode :: V.Vector (Phrase (Func Phrase)) -> Put #-}

putCustom :: V.Vector (Custom) -> Put
putCustom = mapM_ $ \Custom {..} -> do
  putSection 0x00 $ do
    putText _customName
    putByteStream _customPayload

putModule :: Encodable phrase => Module phrase -> Put
putModule Module {..} = do
  putMagic
  putVersion
  putTypes _moduleTypes
  putImports _moduleImports
  putFuncTypes _moduleFuncs
  putTables _moduleTables
  putMemories _moduleMemories
  putGlobals _moduleGlobals
  putExports _moduleExports
  putStart _moduleStart
  putElems _moduleElems
  putCode _moduleFuncs
  putDatas _moduleData
  putCustom _moduleCustom

{-# SPECIALIZE putModule :: Module Identity -> Put #-}
{-# SPECIALIZE putModule :: Module Phrase -> Put #-}
