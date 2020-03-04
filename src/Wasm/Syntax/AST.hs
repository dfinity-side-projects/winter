{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Wasm.Syntax.AST where

import Control.DeepSeq
import Data.ByteString.Lazy
import Data.Default.Class
import Data.Fix
import Data.Functor.Classes
import Data.Functor.Identity
import Data.List as List
import Data.Text.Lazy as Text
import qualified Data.Vector as V
import GHC.Generics
import Lens.Micro.Platform as Lens

import Wasm.Syntax.Memory
import Wasm.Syntax.Ops
import Wasm.Syntax.Ops.Float as F
import Wasm.Syntax.Ops.Int as I
import Wasm.Syntax.Types
import Wasm.Syntax.Values
import Wasm.Util.NFData
import Wasm.Util.Show
import Wasm.Util.Source

type Var (phrase :: * -> *) = phrase Int

_Var :: Traversable phrase => Traversal' (Var phrase) Int
_Var = traverse

{-# SPECIALIZE _Var :: Traversal' (Var Identity) Int #-}
{-# SPECIALIZE _Var :: Traversal' (Var Phrase) Int #-}

type Literal (phrase :: * -> *) = phrase Value

_Literal :: Traversable phrase => Traversal' (Literal phrase) Value
_Literal = traverse

{-# SPECIALIZE _Literal :: Traversal' (Literal Identity) Value #-}
{-# SPECIALIZE _Literal :: Traversal' (Literal Phrase) Value #-}

type Type (phrase :: * -> *) = phrase FuncType

_Type :: Traversable phrase => Traversal' (Type phrase) FuncType
_Type = traverse

{-# SPECIALIZE _Type :: Traversal' (Type Identity) FuncType #-}
{-# SPECIALIZE _Type :: Traversal' (Type Phrase) FuncType #-}

data InstrF (phrase :: * -> *) fix
  = Unreachable
  | Nop
  | Drop
  | Select
  | Block StackType [phrase fix]
  | Loop StackType [phrase fix]
  | If StackType [phrase fix] [phrase fix]
  | Br (Var phrase)
  | BrIf (Var phrase)
  | BrTable [Var phrase] (Var phrase)
  | Return
  | Call (Var phrase)
  | CallIndirect (Var phrase)
  | GetLocal (Var phrase)
  | SetLocal (Var phrase)
  | TeeLocal (Var phrase)
  | GetGlobal (Var phrase)
  | SetGlobal (Var phrase)
  | Load LoadOp
  | Store StoreOp
  | MemorySize
  | MemoryGrow
  | Const (Literal phrase)
  | Test TestOp
  | Compare CompareOp
  | Unary UnaryOp
  | Binary BinaryOp
  | Convert ConvertOp
  deriving Generic

instance (NFData1 phrase, NFData fix) => NFData (InstrF phrase fix) where
  rnf = \case
    Unreachable -> ()
    Nop -> ()
    Drop -> ()
    Select -> ()
    Block result expr ->
      rnf result `seq`
      rnfLiftLift expr
    Loop result expr ->
      rnf result `seq`
      rnfLiftLift expr
    If condition consequent alternative ->
      rnf condition `seq`
      rnfLiftLift consequent `seq`
      rnfLiftLift alternative
    Br var -> rnfLift var
    BrIf var -> rnfLift var
    BrTable table var ->
      rnfLiftLift table `seq`
      rnfLift var
    Return -> ()
    Call var -> rnfLift var
    CallIndirect var -> rnfLift var
    GetLocal var -> rnfLift var
    SetLocal var -> rnfLift var
    TeeLocal var -> rnfLift var
    GetGlobal var -> rnfLift var
    SetGlobal var -> rnfLift var
    Load op -> rnf op
    Store op -> rnf op
    MemorySize -> ()
    MemoryGrow -> ()
    Const lit -> rnfLift lit
    Test op -> rnf op
    Compare op -> rnf op
    Unary op -> rnf op
    Binary op -> rnf op
    Convert op -> rnf op

showVecLiftPrec :: (Show a, Show1 f1) => Int -> V.Vector (f1 a) -> ShowS
showVecLiftPrec p v = showListLiftPrec p (V.toList v)

instance (Show1 phrase, Show fix) => Show (InstrF phrase fix) where
  showsPrec d = showParen (d > 10) . \case
    Unreachable ->
      showString "Unreachable"
    Nop ->
      showString "Nop"
    Drop ->
      showString "Drop"
    Select ->
      showString "Select"
    Block result expr ->
      showString "Block " .
      showPrec 11 result .
      showString " " .
      showListLiftPrec 11 expr
    Loop result expr ->
      showString "Loop " .
      showPrec 11 result .
      showString " " .
      showListLiftPrec 11 expr
    If condition consequent alternative ->
      showString "If " .
      showPrec 11 condition .
      showString " " .
      showListLiftPrec 11 consequent .
      showString " " .
      showListLiftPrec 11 alternative
    Br var ->
      showString "Br " .
      showLiftPrec 11 var
    BrIf var ->
      showString "BrIf " .
      showLiftPrec 11 var
    BrTable table var ->
      showString "BrTable " .
      showListLiftPrec 11 table .
      showString " " .
      showLiftPrec 11 var
    Return ->
      showString "Return"
    Call var ->
      showString "Call " .
      showLiftPrec 11 var
    CallIndirect var ->
      showString "CallIndirect " .
      showLiftPrec 11 var
    GetLocal var ->
      showString "GetLocal " .
      showLiftPrec 11 var
    SetLocal var ->
      showString "SetLocal " .
      showLiftPrec 11 var
    TeeLocal var ->
      showString "TeeLocal " .
      showLiftPrec 11 var
    GetGlobal var ->
      showString "GetGlobal " .
      showLiftPrec 11 var
    SetGlobal var ->
      showString "SetGlobal " .
      showLiftPrec 11 var
    Load op ->
      showString "Load " .
      showPrec 11 op
    Store op ->
      showString "Store " .
      showPrec 11 op
    MemorySize ->
      showString "MemorySize"
    MemoryGrow ->
      showString "MemoryGrow"
    Const lit ->
      showString "Const " .
      showLiftPrec 11 lit
    Test op ->
      showString "Test " .
      showPrec 11 op
    Compare op ->
      showString "Compare " .
      showPrec 11 op
    Unary op ->
      showString "Unary " .
      showPrec 11 op
    Binary op ->
      showString "Binary " .
      showPrec 11 op
    Convert op ->
      showString "Convert " .
      showPrec 11 op

type Instr phrase = Fix (InstrF phrase)

instance NFData1 phrase => NFData (Instr phrase)

type Expr (phrase :: * -> *) = phrase [phrase (Instr phrase)]

data Global phrase
  = Global
  { _globalType :: GlobalType
  , _globalValue :: Expr phrase
  } deriving Generic

makeLenses ''Global

instance Show1 phrase => Show (Global phrase) where
  showsPrec d Global {..} =
    showParen (d > 10) $
    showString "Global " .
    showPrec 11 _globalType .
    showString " " .
    showLiftListLiftPrec 11 _globalValue

instance NFData1 phrase => NFData (Global phrase) where
  rnf Global {..} =
    rnf _globalType `seq`
    rnfLiftLiftLift _globalValue

data Func phrase
  = Func
  { _funcType :: Var phrase
  , _funcLocals :: [ValueType]
  , _funcBody :: [phrase (Instr phrase)]
  } deriving Generic

makeLenses ''Func

showWasm :: (Regioned phrase, Show1 phrase)
         => Int -> [phrase (Instr phrase)] -> ShowS
showWasm _ [] = id
showWasm d ((value -> Fix instr):instrs) = go instr . showString "\n" . showWasm d instrs
 where
  go = \case
    Unreachable                                     -> showString "unreachable"
    Nop                                             -> showString "nop"
    Block l b                                       -> showString "block " . showsPrec d l . showString " $ do\n" . showWasm (succ d) b
    Loop l b                                        -> showString "loop " . showsPrec d l . showString " $ do\n" . showWasm (succ d) b
    If l x []                                       -> showString "if_ " . showsPrec d l . showString " $ do\n" . showWasm (succ d) x
    If l x y                                        -> showString "if_else_ " . showsPrec d l . showString " (do\n" . showWasm (succ d) x . showString ") (do\n" . showWasm (succ d) y . showString ")\n"
    Br (value -> n)                                  -> showString "br " . showsPrec d n
    BrIf (value -> n)                                -> showString "br_if " . showsPrec d n
    BrTable (List.map value -> ns) (value -> n)       -> showString "br_table " . showsPrec d ns . showString " " . showsPrec d n
    Return                                          -> showString "return_"
    Call (value -> n)                                -> showString "call " . showsPrec d n
    CallIndirect (value -> n)                        -> showString "call_indirect " . showsPrec d n
    Drop                                            -> showString "drop"
    Select                                          -> showString "select"
    GetLocal (value -> n)                            -> showString "get_local " . showsPrec d n
    SetLocal (value -> n)                            -> showString "set_local " . showsPrec d n
    TeeLocal (value -> n)                            -> showString "tee_local " . showsPrec d n
    GetGlobal (value -> n)                           -> showString "get_global " . showsPrec d n
    SetGlobal (value -> n)                           -> showString "set_global " . showsPrec d n
    Load (MemoryOp I32Type x y Nothing)             -> showString "i32.load " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I64Type x y Nothing)             -> showString "i64.load " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp F32Type x y Nothing)             -> showString "f32.load " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp F64Type x y Nothing)             -> showString "f64.load " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I32Type x y (Just (Pack8, SX)))  -> showString "i32.load8_s " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I32Type x y (Just (Pack8, ZX)))  -> showString "i32.load8_u " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I32Type x y (Just (Pack16, SX))) -> showString "i32.load16_s " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I32Type x y (Just (Pack16, ZX))) -> showString "i32.load16_u " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I64Type x y (Just (Pack8, SX)))  -> showString "i64.load8_s " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I64Type x y (Just (Pack8, ZX)))  -> showString "i64.load8_u " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I64Type x y (Just (Pack16, SX))) -> showString "i64.load16_s " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I64Type x y (Just (Pack16, ZX))) -> showString "i64.load16_u " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I64Type x y (Just (Pack32, SX))) -> showString "i64.load32_s " . showsPrec d x . showString " " . showsPrec d y
    Load (MemoryOp I64Type x y (Just (Pack32, ZX))) -> showString "i64.load32_u " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp I32Type x y Nothing)            -> showString "i32.store " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp I64Type x y Nothing)            -> showString "i64.store " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp F32Type x y Nothing)            -> showString "f32.store " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp F64Type x y Nothing)            -> showString "f64.store " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp I32Type x y (Just Pack8))       -> showString "i32.store8 " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp I32Type x y (Just Pack16))      -> showString "i32.store16 " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp I64Type x y (Just Pack8))       -> showString "i64.store8 " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp I64Type x y (Just Pack16))      -> showString "i64.store16 " . showsPrec d x . showString " " . showsPrec d y
    Store (MemoryOp I64Type x y (Just Pack32))      -> showString "i64.store32 " . showsPrec d x . showString " " . showsPrec d y
    MemorySize                                      -> showString "memory_size"
    MemoryGrow                                      -> showString "memory_grow"
    Const (value -> I32 x)                           -> showString "i32.const " . showsPrec d x
    Const (value -> I64 x)                           -> showString "i64.const " . showsPrec d x
    Const (value -> F32 x)                           -> showString "f32.const " . showsPrec d x
    Const (value -> F64 x)                           -> showString "f64.const " . showsPrec d x
    Unary (I32UnaryOp Clz)                          -> showString "i32.clz"
    Unary (I32UnaryOp Ctz)                          -> showString "i32.ctz"
    Unary (I32UnaryOp Popcnt)                       -> showString "i32.popcnt"
    Unary (I64UnaryOp Clz)                          -> showString "i64.clz"
    Unary (I64UnaryOp Ctz)                          -> showString "i64.ctz"
    Unary (I64UnaryOp Popcnt)                       -> showString "i64.popcnt"
    Unary (F32UnaryOp Abs)                          -> showString "f32.abs"
    Unary (F32UnaryOp Neg)                          -> showString "f32.neg"
    Unary (F32UnaryOp Ceil)                         -> showString "f32.ceil"
    Unary (F32UnaryOp Floor)                        -> showString "f32.floor"
    Unary (F32UnaryOp Trunc)                        -> showString "f32.trunc"
    Unary (F32UnaryOp Nearest)                      -> showString "f32.nearest"
    Unary (F32UnaryOp Sqrt)                         -> showString "f32.sqrt"
    Unary (F64UnaryOp Abs)                          -> showString "f64.abs"
    Unary (F64UnaryOp Neg)                          -> showString "f64.neg"
    Unary (F64UnaryOp Ceil)                         -> showString "f64.ceil"
    Unary (F64UnaryOp Floor)                        -> showString "f64.floor"
    Unary (F64UnaryOp Trunc)                        -> showString "f64.trunc"
    Unary (F64UnaryOp Nearest)                      -> showString "f64.nearest"
    Unary (F64UnaryOp Sqrt)                         -> showString "f64.sqrt"
    Binary (I32BinaryOp I.Add)                      -> showString "i32.add"
    Binary (I32BinaryOp I.Sub)                      -> showString "i32.sub"
    Binary (I32BinaryOp I.Mul)                      -> showString "i32.mul"
    Binary (I32BinaryOp DivS)                       -> showString "i32.div_s"
    Binary (I32BinaryOp DivU)                       -> showString "i32.div_u"
    Binary (I32BinaryOp RemS)                       -> showString "i32.rem_s"
    Binary (I32BinaryOp RemU)                       -> showString "i32.rem_u"
    Binary (I32BinaryOp And)                        -> showString "i32.and"
    Binary (I32BinaryOp Or)                         -> showString "i32.or"
    Binary (I32BinaryOp Xor)                        -> showString "i32.xor"
    Binary (I32BinaryOp Shl)                        -> showString "i32.shl"
    Binary (I32BinaryOp ShrS)                       -> showString "i32.shr_s"
    Binary (I32BinaryOp ShrU)                       -> showString "i32.shr_u"
    Binary (I32BinaryOp Rotl)                       -> showString "i32.rotl"
    Binary (I32BinaryOp Rotr)                       -> showString "i32.rotr"
    Binary (I64BinaryOp I.Add)                      -> showString "i64.add"
    Binary (I64BinaryOp I.Sub)                      -> showString "i64.sub"
    Binary (I64BinaryOp I.Mul)                      -> showString "i64.mul"
    Binary (I64BinaryOp DivS)                       -> showString "i64.div_s"
    Binary (I64BinaryOp DivU)                       -> showString "i64.div_u"
    Binary (I64BinaryOp RemS)                       -> showString "i64.rem_s"
    Binary (I64BinaryOp RemU)                       -> showString "i64.rem_u"
    Binary (I64BinaryOp And)                        -> showString "i64.and"
    Binary (I64BinaryOp Or)                         -> showString "i64.or"
    Binary (I64BinaryOp Xor)                        -> showString "i64.xor"
    Binary (I64BinaryOp Shl)                        -> showString "i64.shl"
    Binary (I64BinaryOp ShrS)                       -> showString "i64.shr_s"
    Binary (I64BinaryOp ShrU)                       -> showString "i64.shr_u"
    Binary (I64BinaryOp Rotl)                       -> showString "i64.rotl"
    Binary (I64BinaryOp Rotr)                       -> showString "i64.rotr"
    Binary (F32BinaryOp F.Add)                      -> showString "f32.add"
    Binary (F32BinaryOp F.Sub)                      -> showString "f32.sub"
    Binary (F32BinaryOp F.Mul)                      -> showString "f32.mul"
    Binary (F32BinaryOp Div)                        -> showString "f32.div"
    Binary (F32BinaryOp Min)                        -> showString "f32.min"
    Binary (F32BinaryOp Max)                        -> showString "f32.max"
    Binary (F32BinaryOp CopySign)                   -> showString "f32.copysign"
    Binary (F64BinaryOp F.Add)                      -> showString "f64.add"
    Binary (F64BinaryOp F.Sub)                      -> showString "f64.sub"
    Binary (F64BinaryOp F.Mul)                      -> showString "f64.mul"
    Binary (F64BinaryOp Div)                        -> showString "f64.div"
    Binary (F64BinaryOp Min)                        -> showString "f64.min"
    Binary (F64BinaryOp Max)                        -> showString "f64.max"
    Binary (F64BinaryOp CopySign)                   -> showString "f64.copysign"
    Test (I32TestOp Eqz)                            -> showString "i32.eqz"
    Test (I64TestOp Eqz)                            -> showString "i64.eqz"
    Compare (I32CompareOp I.Eq)                     -> showString "i32.eq"
    Compare (I32CompareOp I.Ne)                     -> showString "i32.ne"
    Compare (I32CompareOp LtS)                      -> showString "i32.lt_s"
    Compare (I32CompareOp LtU)                      -> showString "i32.lt_u"
    Compare (I32CompareOp GtS)                      -> showString "i32.gt_s"
    Compare (I32CompareOp GtU)                      -> showString "i32.gt_u"
    Compare (I32CompareOp LeS)                      -> showString "i32.le_s"
    Compare (I32CompareOp LeU)                      -> showString "i32.le_u"
    Compare (I32CompareOp GeS)                      -> showString "i32.ge_s"
    Compare (I32CompareOp GeU)                      -> showString "i32.ge_u"
    Compare (I64CompareOp I.Eq)                     -> showString "i64.eq"
    Compare (I64CompareOp I.Ne)                     -> showString "i64.ne"
    Compare (I64CompareOp LtS)                      -> showString "i64.lt_s"
    Compare (I64CompareOp LtU)                      -> showString "i64.lt_u"
    Compare (I64CompareOp GtS)                      -> showString "i64.gt_s"
    Compare (I64CompareOp GtU)                      -> showString "i64.gt_u"
    Compare (I64CompareOp LeS)                      -> showString "i64.le_s"
    Compare (I64CompareOp LeU)                      -> showString "i64.le_u"
    Compare (I64CompareOp GeS)                      -> showString "i64.ge_s"
    Compare (I64CompareOp GeU)                      -> showString "i64.ge_u"
    Compare (F32CompareOp F.Eq)                     -> showString "f32.eq"
    Compare (F32CompareOp F.Ne)                     -> showString "f32.ne"
    Compare (F32CompareOp Lt)                       -> showString "f32.lt"
    Compare (F32CompareOp Gt)                       -> showString "f32.gt"
    Compare (F32CompareOp Le)                       -> showString "f32.le"
    Compare (F32CompareOp Ge)                       -> showString "f32.ge"
    Compare (F64CompareOp F.Eq)                     -> showString "f64.eq"
    Compare (F64CompareOp F.Ne)                     -> showString "f64.ne"
    Compare (F64CompareOp Lt)                       -> showString "f64.lt"
    Compare (F64CompareOp Gt)                       -> showString "f64.gt"
    Compare (F64CompareOp Le)                       -> showString "f64.le"
    Compare (F64CompareOp Ge)                       -> showString "f64.ge"
    Convert (I32ConvertOp WrapI64)                  -> showString "i32.wrap_i64"
    Convert (I32ConvertOp TruncSF32)                -> showString "i32.trunc_s_f32"
    Convert (I32ConvertOp TruncUF32)                -> showString "i32.trunc_u_f32"
    Convert (I32ConvertOp TruncSF64)                -> showString "i32.trunc_s_f64"
    Convert (I32ConvertOp TruncUF64)                -> showString "i32.trunc_u_f64"
    Convert (I32ConvertOp ReinterpretFloat)         -> showString "i32.reinterpret_float"
    Convert (I64ConvertOp ExtendSI32)               -> showString "i64.extend_s_i32"
    Convert (I64ConvertOp ExtendUI32)               -> showString "i64.extend_u_i32"
    Convert (I64ConvertOp TruncSF32)                -> showString "i64.trunc_s_f32"
    Convert (I64ConvertOp TruncUF32)                -> showString "i64.trunc_u_f32"
    Convert (I64ConvertOp TruncSF64)                -> showString "i64.trunc_s_f64"
    Convert (I64ConvertOp TruncUF64)                -> showString "i64.trunc_u_f64"
    Convert (I64ConvertOp ReinterpretFloat)         -> showString "i64.reinterpret_float"
    Convert (F32ConvertOp ConvertSI32)              -> showString "f32.convert_s_i32"
    Convert (F32ConvertOp ConvertUI32)              -> showString "f32.convert_u_i32"
    Convert (F32ConvertOp ConvertSI64)              -> showString "f32.convert_s_i64"
    Convert (F32ConvertOp ConvertUI64)              -> showString "f32.convert_u_i64"
    Convert (F32ConvertOp DemoteF64)                -> showString "f32.demote_f64"
    Convert (F32ConvertOp ReinterpretInt)           -> showString "f32.reinterpret_int"
    Convert (F64ConvertOp ConvertSI32)              -> showString "f64.convert_s_i32"
    Convert (F64ConvertOp ConvertUI32)              -> showString "f64.convert_u_i32"
    Convert (F64ConvertOp ConvertSI64)              -> showString "f64.convert_s_i64"
    Convert (F64ConvertOp ConvertUI64)              -> showString "f64.convert_u_i64"
    Convert (F64ConvertOp PromoteF32)               -> showString "f64.promote_f32"
    Convert (F64ConvertOp ReinterpretInt)           -> showString "f64.reinterpret_int"
    x                                               -> showsPrec d x

instance (Regioned phrase, Show1 phrase) => Show (Func phrase) where
  showsPrec d Func {..} =
    showParen (d > 10) $
    showString "Func " .
    showLiftPrec 11 _funcType .
    showString " " .
    showPrec 11 _funcLocals .
    showString " $ do\n" .
    showWasm 11 _funcBody

instance NFData1 phrase => NFData (Func phrase) where
  rnf Func {..} =
    rnfLift _funcType `seq`
    rnf _funcLocals `seq`
    rnfLiftLift _funcBody

type Table (phrase :: * -> *) = phrase TableType

_Table :: Traversable phrase => Traversal' (Table phrase) TableType
_Table = traverse

type Memory (phrase :: * -> *) = phrase MemoryType

_Memory :: Traversable phrase => Traversal' (Memory phrase) MemoryType
_Memory = traverse

data Segment phrase a
  = Segment
  { _segmentIndex :: Var phrase
  , _segmentOffset :: Expr phrase
  , _segmentInit :: a
  } deriving (Generic, Generic1)

makeLenses ''Segment

instance (NFData1 phrase, NFData a) => NFData (Segment phrase a) where
  rnf Segment {..} =
    rnfLift _segmentIndex `seq`
    rnfLiftLiftLift _segmentOffset `seq`
    rnf _segmentInit

instance NFData1 phrase => NFData1 (Segment phrase) where
  liftRnf rnfRec Segment {..} =
    rnfLift _segmentIndex `seq`
    rnfLiftLiftLift _segmentOffset `seq`
    rnfRec _segmentInit

instance (Show1 phrase, Show a) => Show (Segment phrase a) where
  showsPrec d Segment {..} =
    showParen (d > 10) $
    showString "Segment " .
    showLiftPrec 11 _segmentIndex .
    showString " " .
    showLiftListLiftPrec 11 _segmentOffset .
    showString " " .
    showPrec 11 _segmentInit

instance Show1 phrase => Show1 (Segment phrase) where
  liftShowsPrec showRec _ d Segment {..} =
    showParen (d > 10) $
    showString "Segment " .
    showLiftPrec 11 _segmentIndex .
    showString " " .
    showLiftListLiftPrec 11 _segmentOffset .
    showString " " .
    showRec 11 _segmentInit

type TableSegment phrase = Segment phrase [Var phrase]

type MemorySegment phrase = Segment phrase ByteString

data ExportDesc phrase
  = FuncExport (Var phrase)
  | TableExport (Var phrase)
  | MemoryExport (Var phrase)
  | GlobalExport (Var phrase)
  deriving Generic

_FuncExport :: Traversal' (ExportDesc phrase) (Var phrase)
_FuncExport f (FuncExport x) = FuncExport <$> f x
_FuncExport _ x              = pure x

{-# SPECIALIZE _FuncExport :: Traversal' (ExportDesc Identity) (Var Identity) #-}
{-# SPECIALIZE _FuncExport :: Traversal' (ExportDesc Phrase) (Var Phrase) #-}

_TableExport :: Traversal' (ExportDesc phrase) (Var phrase)
_TableExport f (TableExport x) = TableExport <$> f x
_TableExport _ x               = pure x

{-# SPECIALIZE _TableExport :: Traversal' (ExportDesc Identity) (Var Identity) #-}
{-# SPECIALIZE _TableExport :: Traversal' (ExportDesc Phrase) (Var Phrase) #-}

_MemoryExport :: Traversal' (ExportDesc phrase) (Var phrase)
_MemoryExport f (MemoryExport x) = MemoryExport <$> f x
_MemoryExport _ x                = pure x

{-# SPECIALIZE _MemoryExport :: Traversal' (ExportDesc Identity) (Var Identity) #-}
{-# SPECIALIZE _MemoryExport :: Traversal' (ExportDesc Phrase) (Var Phrase) #-}

_GlobalExport :: Traversal' (ExportDesc phrase) (Var phrase)
_GlobalExport f (GlobalExport x) = GlobalExport <$> f x
_GlobalExport _ x                = pure x

{-# SPECIALIZE _GlobalExport :: Traversal' (ExportDesc Identity) (Var Identity) #-}
{-# SPECIALIZE _GlobalExport :: Traversal' (ExportDesc Phrase) (Var Phrase) #-}

_ExportDescVar :: Traversal' (ExportDesc phrase) (Var phrase)
_ExportDescVar f (FuncExport   x) = FuncExport <$> f x
_ExportDescVar f (TableExport  x) = TableExport <$> f x
_ExportDescVar f (MemoryExport x) = MemoryExport <$> f x
_ExportDescVar f (GlobalExport x) = GlobalExport <$> f x

{-# SPECIALIZE _ExportDescVar :: Traversal' (ExportDesc Identity) (Var Identity) #-}
{-# SPECIALIZE _ExportDescVar :: Traversal' (ExportDesc Phrase) (Var Phrase) #-}

instance NFData1 phrase => NFData (ExportDesc phrase) where
  rnf = rnfLift . (^?! _ExportDescVar)

instance Show1 phrase => Show (ExportDesc phrase) where
  showsPrec d = showParen (d > 10) . \case
    FuncExport var ->
      showString "FuncExport " .
      showLiftPrec 11 var
    TableExport var ->
      showString "TableExport " .
      showLiftPrec 11 var
    MemoryExport var ->
      showString "MemoryExport " .
      showLiftPrec 11 var
    GlobalExport var ->
      showString "GlobalExport " .
      showLiftPrec 11 var

data Export phrase
  = Export
  { _exportName :: Text
  , _exportDesc :: ExportDesc phrase
  } deriving (Generic, NFData, Show)

makeLenses ''Export

data ImportDesc phrase
  = FuncImport (Var phrase)
  | TableImport TableType
  | MemoryImport MemoryType
  | GlobalImport GlobalType
  deriving Generic

_FuncImport :: Traversal' (ImportDesc phrase) (Var phrase)
_FuncImport f (FuncImport x) = FuncImport <$> f x
_FuncImport _ x              = pure x

{-# SPECIALIZE _FuncImport :: Traversal' (ImportDesc Identity) (Var Identity) #-}
{-# SPECIALIZE _FuncImport :: Traversal' (ImportDesc Phrase) (Var Phrase) #-}

_TableImport :: Traversal' (ImportDesc phrase) TableType
_TableImport f (TableImport x) = TableImport <$> f x
_TableImport _ x               = pure x

_MemoryImport :: Traversal' (ImportDesc phrase) MemoryType
_MemoryImport f (MemoryImport x) = MemoryImport <$> f x
_MemoryImport _ x                = pure x

_GlobalImport :: Traversal' (ImportDesc phrase) GlobalType
_GlobalImport f (GlobalImport x) = GlobalImport <$> f x
_GlobalImport _ x                = pure x

instance NFData1 phrase => NFData (ImportDesc phrase) where
  rnf = \case
    FuncImport var -> rnfLift var
    TableImport table -> rnf table
    MemoryImport memory -> rnf memory
    GlobalImport global -> rnf global

instance Show1 phrase => Show (ImportDesc phrase) where
  showsPrec d = showParen (d > 10) . \case
    FuncImport var ->
      showString "FuncImport " .
      showLiftPrec 11 var
    TableImport table ->
      showString "TableImport " .
      showPrec 11 table
    MemoryImport memory ->
      showString "MemoryImport " .
      showPrec 11 memory
    GlobalImport global ->
      showString "GlobalImport " .
      showPrec 11 global

data Import phrase
  = Import
  { _importModule :: Text
  , _importItem :: Text
  , _importDesc :: ImportDesc phrase
  } deriving (Generic, NFData, Show)

makeLenses ''Import

data Custom
  = Custom
  { _customName :: Text
  , _customPayload :: ByteString
  } deriving (Generic, NFData, Show)

makeLenses ''Custom

data Module phrase
  = Module
  { _moduleTypes    :: V.Vector (Type phrase)
  , _moduleGlobals  :: V.Vector (phrase (Global phrase))
  , _moduleTables   :: V.Vector (Table phrase)
  , _moduleMemories :: V.Vector (Memory phrase)
  , _moduleFuncs    :: V.Vector (phrase (Func phrase))
  , _moduleStart    :: Maybe (Var phrase)
  , _moduleElems    :: V.Vector (phrase (TableSegment phrase))
  , _moduleData     :: V.Vector (phrase (MemorySegment phrase))
  , _moduleImports  :: V.Vector (phrase (Import phrase))
  , _moduleExports  :: V.Vector (phrase (Export phrase))
  , _moduleCustom   :: V.Vector Custom
  } deriving Generic

makeLenses ''Module

instance Default (Module phrase) where
  def = Module V.empty V.empty V.empty V.empty V.empty Nothing V.empty V.empty V.empty V.empty V.empty

instance NFData1 phrase => NFData (Module phrase) where
  rnf Module {..} =
    rnfLiftLift _moduleTypes `seq`
    rnfLiftLift _moduleGlobals `seq`
    rnfLiftLift _moduleTables `seq`
    rnfLiftLift _moduleMemories `seq`
    rnfLiftLift _moduleFuncs `seq`
    rnfLiftLift _moduleStart `seq`
    rnfLiftLiftLiftLiftLift _moduleElems `seq`
    rnfLiftLift _moduleData `seq`
    rnfLiftLift _moduleImports `seq`
    rnfLiftLift _moduleExports `seq`
    rnf _moduleCustom

instance (Regioned phrase, Show1 phrase) => Show (Module phrase) where
  showsPrec d Module {..} =
    showParen (d > 10) $
    showString "Module {" .
    showString "\n  _moduleTypes   = " .
    showVecLiftPrec 11 _moduleTypes .
    showString "\n  _moduleGlobals  = " .
    showVecLiftPrec 11 _moduleGlobals .
    showString "\n  _moduleTables  = " .
    showVecLiftPrec 11 _moduleTables .
    showString "\n  _moduleMemories = " .
    showVecLiftPrec 11 _moduleMemories .
    showString "\n  _moduleFuncs   = [" .
    List.foldr (\x -> ((showString "\n    " . showLiftPrec 11 x) .)) id _moduleFuncs .
    showString "\n  ]" .
    showString "\n  _moduleStart   = " .
    showLiftLiftPrec 11 _moduleStart .
    showString "\n  _moduleElems   = " .
    showListLiftLiftListLiftPrec 11 (V.toList _moduleElems) .
    showString "\n  _moduleData    = " .
    showVecLiftPrec 11 _moduleData .
    showString "\n  _moduleImports  = " .
    showVecLiftPrec 11 _moduleImports .
    showString "\n  _moduleExports  = " .
    showVecLiftPrec 11 _moduleExports .
    showString "\n  _moduleCustom  = " .
    showPrec 11 _moduleCustom .
    showString "\n}"

funcTypeFor :: Traversable phrase => Module phrase -> Var phrase -> FuncType
funcTypeFor ast (need _Var -> var) = ast ^?! moduleTypes . ix var . _Type

importTypeFor
  :: Traversable phrase => Module phrase -> Import phrase -> ExternType
importTypeFor ast = flip (.) _importDesc $ \case
  FuncImport   var    -> ExternFuncType $ funcTypeFor ast var
  TableImport  table  -> ExternTableType table
  MemoryImport memory -> ExternMemoryType memory
  GlobalImport global -> ExternGlobalType global

exportTypeFor
  :: Traversable phrase => Module phrase -> Export phrase -> ExternType
exportTypeFor ast = flip (.) _exportDesc $ \case
  FuncExport   (need _Var -> var) -> ExternFuncType $ funcs !! var
  TableExport  (need _Var -> var) -> ExternTableType $ tables !! var
  MemoryExport (need _Var -> var) -> ExternMemoryType $ memories !! var
  GlobalExport (need _Var -> var) -> ExternGlobalType $ globals !! var
 where

  imports :: V.Vector ExternType
  imports = V.map (importTypeFor ast . value) (ast ^. moduleImports)

  funcs :: [FuncType]
  funcs =
    imports
      ^.. traverse
      .   _ExternFuncType
      ++  ast
      ^.. moduleFuncs
      .   traverse
      .   traverse
      .   funcType
      .   Lens.to (funcTypeFor ast)

  tables :: [TableType]
  tables =
    imports
      ^.. traverse
      .   _ExternTableType
      ++  ast
      ^.. moduleTables
      .   traverse
      .   _Table

  memories :: [MemoryType]
  memories =
    imports
      ^.. traverse
      .   _ExternMemoryType
      ++  ast
      ^.. moduleMemories
      .   traverse
      .   _Memory

  globals :: [GlobalType]
  globals =
    imports
      ^.. traverse
      .   _ExternGlobalType
      ++  ast
      ^.. moduleGlobals
      .   traverse
      .   traverse
      .   globalType
