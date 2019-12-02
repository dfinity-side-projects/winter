{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-|
An interpreter for wasm function.

Note that although these functions are polymorphic in any 'PrimMonad',
performance is best if the monad is plain 'IO' or 'ST s', and the 'f' paramter
is 'Identity' or 'Phrase', due to judicious use of specialization.
-}
module Wasm.Exec.Eval
  ( initialize
  , invokeByName
  , getByName
  , createHostFunc
  , createHostFuncEff
  , elem
  , invoke
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Reader hiding (local)
import           Control.Monad.Trans.State
import           Control.Monad.Identity
import           Control.Monad.Primitive
import           Control.Monad.ST (ST)
import qualified Data.ByteString.Lazy as B
import           Data.Default.Class (Default(..))
import           Data.Fix
import           Data.Functor.Classes
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Primitive.MutVar
import           Data.List hiding (lookup, elem)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text.Lazy (Text, unpack)
import qualified Data.Vector as V
import           Lens.Micro.Platform
import           Prelude hiding (lookup, elem)
import           Text.Show (showListWith)

import           Wasm.Exec.EvalNumeric
import qualified Wasm.Runtime.Func as Func
import qualified Wasm.Runtime.Global as Global
import           Wasm.Runtime.Instance
import qualified Wasm.Runtime.Memory as Memory
import           Wasm.Runtime.Table as Table
import           Wasm.Syntax.AST
import           Wasm.Syntax.Ops
import           Wasm.Syntax.Types
import           Wasm.Syntax.Values as Values
import           Wasm.Util.Source

-- import           Debug.Trace

{- Errors -}

data EvalError
  = EvalLinkError Region String
  | EvalTrapError Region String
  | EvalCrashError Region String
  | EvalMemoryError Region Memory.MemoryError
  | EvalGlobalError Region Global.GlobalError
  | EvalTableError Region Table.TableError
  | EvalExhaustionError Region String
  | EvalNumericError Region NumericError
  deriving (Show, Eq)

instance Exception EvalError

memoryErrorString :: Memory.MemoryError -> String
memoryErrorString = \case
  Memory.MemoryBoundsError  -> "out of bounds memory access"
  Memory.MemorySizeOverflow -> "memory size overflow"
  Memory.MemorySizeLimit    -> "memory size limit reached"
  Memory.MemoryTypeError    -> "type mismatch at memory access"
  Memory.MemoryOutOfMemory  -> "out of memory"

{-
numericError at = \case
  NumericError.IntegerOverflow -> "integer overflow"
  NumericError.IntegerDivideByZero -> "integer divide by zero"
  NumericError.InvalidConversionToInteger -> "invalid conversion to integer"
  EvalNumeric.TypeError (i, v, t) ->
    Crash.error at
      ("type error, expected " ^ Types.string_of_value_type t ^ " as operand " ^
       string_of_int i ^ ", got " ^ Types.string_of_value_type (type_of v))
  exn -> raise exn
-}

{- Administrative Expressions & Configurations -}

type Stack a = [a]

data Frame f m = Frame
  { _frameInst :: !(ModuleInst f m)
  , _frameLocals :: ![MutVar (PrimState m) Value]
  }

instance Show (Frame f m) where
  showsPrec d Frame {..}
    = showString "Frame (with "
    . showsPrec d (length _frameLocals)
    . showString " locals)"

makeLenses ''Frame

{-
In a change from the reference interpreter, where the small-step relation works
on just a value and instruction set, we introduce a control stack as well. This
gives a significant performance boost.

The correspondence between their and our 'Code' type is the following:

    Wasm:   (vs1, Label n les (vs2, es2) :: es1)
    Winter: Code cfg1 (Label n les les (Code … cfg2 vs1 es1)) vs2 es2

The point is that now es2 (the instruction to execute next) is nicely exposed
for O(1) analsys, instead of hidden deep in the Code data structure. On
can also think of this as a zipper.

We also keep track of the current 'Config' (esp 'configFrame') here.
-}

data Control f m
  = EmptyCS
  | Label !Int !(DList (f (AdminInstr f m))) (Code f m)
  | Framed !Int (Code f m)
    -- ^ the frame is the _previous_ frame, to restore when popping this control

data Code f m = Code
  { _codeControl  :: !(Control f m)
  , _codeConfig  :: !(Config f m)
  , _codeStack  :: !(Stack Value)
  , _codeInstrs :: ![f (AdminInstr f m)]
  }

instance (Regioned f, Show1 f) => Show (Code f m) where
  showsPrec d Code {..} =
    showParen (d > 10)
      $ showString "Code "
      . showsPrec 11 _codeControl
      -- . showString " "
      -- . showsPrec 11 _codeConfig
      . showString " "
      . showsPrec 11 _codeStack
      . showString " "
      . showListWith (showsPrec1 11) _codeInstrs

type DList a = [a] -> [a]
data AdminInstr f m
  = Plain !(Instr f)
  | Invoke !(ModuleFunc f m)
  | Trapping !String
  | Returning !(Stack Value)
  | Breaking !Int !(Stack Value)

instance (Regioned f, Show1 f) => Show (Control f m) where
  showsPrec d = showParen (d > 10) . \case
    EmptyCS -> showString "EmptyCS"
    Label i l c -> showString "Label "     . showsPrec 11 i
                                           . showString " "
                                           . showListWith (showsPrec1 11) (l [])
                                           . showString " "
                                           . showsPrec 11 c
    Framed i c -> showString "Framed "   . showsPrec 11 i
                                         . showString " "
                                         . showsPrec 11 c


instance (Regioned f, Show1 f) => Show (AdminInstr f m) where
  showsPrec d = showParen (d > 10) . \case
    Plain p      -> showString "Plain "     . showsPrec 11 p
    Invoke i     -> showString "Invoke "    . showsPrec1 11 i
    Trapping t   -> showString "Trapping "  . showsPrec1 11 t
    Returning r  -> showString "Returning " . showsPrec1 11 r
    Breaking i s -> showString "Breaking "  . showsPrec 11 i
                                           . showString " "
                                           . showsPrec1 11 s

data Config f m = Config
  { _configModules :: !(IntMap (ModuleInst f m))
  , _configFrame   :: !(Frame f m)
  , _configBudget  :: !Int                {- to model stack overflow -}
  }

makeLenses ''Config

type EvalT = ExceptT EvalError
type CEvalT f m = ReaderT (Config f m) (EvalT m)

getInst :: Monad m => ModuleRef -> CEvalT f m (ModuleInst f m)
getInst ref = do
  mres <- view (configModules.at ref)
  case mres of
    Nothing -> throwError $
      EvalCrashError def $ "Reference to unknown module #" ++ show ref
    Just x  -> return x

getFrameInst :: Monad m => CEvalT f m (ModuleInst f m)
getFrameInst = view (configFrame.frameInst)

newConfig :: IntMap (ModuleInst f m) -> ModuleInst f m -> Config f m
newConfig mods inst = Config
  { _configModules = mods
  , _configFrame   = Frame inst []
  , _configBudget  = 300
  }

plain :: Regioned f => f (Instr f) -> f (AdminInstr f m)
plain e = Plain (value e) @@ region e
{-# INLINE plain #-}

lookup :: (Regioned f, Monad m)
       => String -> s -> Lens' s [a] -> Var f -> EvalT m a
lookup category inst l x@(value -> x') =
  if fromIntegral x' < length (inst^.l)
  then pure $ inst^?!l.ix (fromIntegral x')
  else throwError $
    EvalCrashError (region x) ("undefined " <> category <> " " <> show x')

type_ :: (Regioned f, Monad m)
      => ModuleInst f m -> Var f -> EvalT m FuncType
type_ inst = fmap value . lookup "type" inst (miModule.moduleTypes)

func :: (Regioned f, Monad m)
     => ModuleInst f m -> Var f -> EvalT m (ModuleFunc f m)
func inst = lookup "function" inst miFuncs

table :: (Regioned f, Monad m)
      => ModuleInst f m -> Var f -> EvalT m (TableInst m (ModuleFunc f m))
table inst = lookup "table" inst miTables

memory :: (Regioned f, Monad m)
       => ModuleInst f m -> Var f -> EvalT m (Memory.MemoryInst m)
memory inst = lookup "memory" inst miMemories

global :: (Regioned f, Monad m)
       => ModuleInst f m -> Var f -> EvalT m (Global.GlobalInst m)
global inst = lookup "global" inst miGlobals

local :: (Regioned f, Monad m)
      => Frame f m -> Var f -> EvalT m (MutVar (PrimState m) Value)
local frame = lookup "local" frame frameLocals

elem :: (Regioned f, PrimMonad m)
     => ModuleInst f m -> Var f -> Table.Index -> Region
     -> EvalT m (ModuleFunc f m)
elem inst x i at' = do
  t <- table inst x
  x <- lift $ Table.load t i
  case x of
    Nothing -> throwError $
      EvalTrapError at' ("uninitialized element " ++ show i)
    Just f -> pure f

funcElem :: (Regioned f, PrimMonad m)
         => ModuleInst f m -> Var f -> Table.Index -> Region
         -> EvalT m (ModuleFunc f m)
funcElem = elem
{-# INLINE funcElem #-}

takeFrom :: Monad m
         => Int -> Stack a -> Region -> EvalT m (Stack a)
takeFrom n vs at' =
  if n > length vs
  then throwError $ EvalCrashError at' "stack underflow"
  else pure $ take n vs

partialZip :: [a] -> [b] -> [Either a (Either b (a, b))]
partialZip [] [] = []
partialZip xs [] = map Left xs
partialZip [] ys = map (Right . Left) ys
partialZip (x:xs) (y:ys) = Right (Right (x, y)) : partialZip xs ys

checkTypes :: Monad m
           => Region -> [ValueType] -> [Value] -> EvalT m ()
checkTypes at ts xs = forM_ (partialZip ts xs) $ \case
  Left t ->
    throwError $ EvalCrashError at $ "missing argument of type " ++ show t
  Right (Left x) ->
    throwError $ EvalCrashError at $ "unexpected argument " ++ show x
  Right (Right (t, x)) | Values.typeOf x /= t ->
    throwError $ EvalCrashError at $ "expected type " ++ show t
      ++ " got " ++ show x
    | otherwise -> return ()


{- Evaluation -}

{-
 * Conventions:
 *   e  : instr
 *   v  : value
 *   es : instr list
 *   vs : value stack
 *   c : config
 -}

step :: (Regioned f, PrimMonad m)
     => Code f m -> EvalT m (Code f m)
step (Code cs _ vs []) = case cs of
    EmptyCS -> error "Cannot step without instructions"
    Label _ _ code' -> {-# SCC step_Label1 #-}
        return $ code' { _codeStack = vs ++ _codeStack code' }
    Framed _ code' -> {-# SCC step_Framed1 #-}
        return $ code' { _codeStack = vs ++ _codeStack code' }
step(Code cs cfg vs (e:es)) = (`runReaderT` cfg) $ do
    let at = region e
        -- short form for stepping with the current control stack
        k vs es = return $ Code cs cfg vs es
    case (cs, value e) of
      (_, Plain e') -> {-# SCC step_Plain #-} case (unFix e', vs) of
        (Unreachable, vs)              -> {-# SCC step_Unreachable #-}
          k vs (Trapping "unreachable executed" @@ at : es)
        (Nop, vs)                      -> {-# SCC step_Nop #-}
          k vs es
        (Block ts es', vs)             -> {-# SCC step_Block #-}
          return $ Code
            (Label (length ts) id (Code cs cfg vs es))
            cfg [] (map plain es')
        (Loop _ es', vs)               -> {-# SCC step_Loop #-}
          return $ Code
            (Label 0 (e :) (Code cs cfg vs es))
            cfg [] (map plain es')
        (If ts _ es2, I32 0 : vs')     -> {-# SCC step_If1 #-}
          k vs' (Plain (Fix (Block ts es2)) @@ at : es)
        (If ts es1 _, I32 _ : vs')     -> {-# SCC step_If2 #-}
          k vs' (Plain (Fix (Block ts es1)) @@ at : es)
        (Br x, vs)                     -> {-# SCC step_Br #-}
          k [] (Breaking (value x) vs @@ at : es)
        (BrIf _, I32 0 : vs')          -> {-# SCC step_BrIf1 #-}
          k vs' es
        (BrIf x, I32 _ : vs')          -> {-# SCC step_BrIf2 #-}
          k vs' (Plain (Fix (Br x)) @@ at : es)
        (BrTable xs x, I32 i : vs')
          | i < 0 || fromIntegral i >= length xs -> {-# SCC step_BrTable1 #-}
            k vs' (Plain (Fix (Br x)) @@ at : es)
          | otherwise -> {-# SCC step_BrTable2 #-}
            k vs' (Plain (Fix (Br (xs !! fromIntegral i))) @@ at : es)
        (Return, vs)                   -> {-# SCC step_Return #-}
          k [] (Returning vs @@ at : es)

        (Call x, vs) -> {-# SCC step_Call #-} do
          inst <- getFrameInst
          -- traceM $ "Call " ++ show (value x)
          f <- lift $ func inst x
          k vs (Invoke f @@ at : es)

        (CallIndirect x, I32 i : vs) -> {-# SCC step_CallIndirect #-} do
          inst <- getFrameInst
          func <- lift $ funcElem inst (0 @@ at) i at
          t <- lift $ type_ inst x
          k vs $
            if t /= Func.typeOf func
            then Trapping "indirect call type mismatch" @@ at : es
            else Invoke func @@ at :  es

        (Drop, _ : vs') -> {-# SCC step_Drop #-}
          k vs' es

        (Select, I32 0 : v2 : _ : vs') -> {-# SCC step_Select1 #-}
          k (v2 : vs') es
        (Select, I32 _ : _ : v1 : vs') -> {-# SCC step_Select2 #-}
          k (v1 : vs') es

        (GetLocal x, vs) -> {-# SCC step_GetLocal #-} do
          frame <- view configFrame
          mut <- lift $ local frame x
          l <- readMutVar mut
          k (l : vs) es

        (SetLocal x, v : vs') -> {-# SCC step_SetLocal #-} do
          frame <- view configFrame
          mut <- lift $ local frame x
          writeMutVar mut v
          k vs' es

        (TeeLocal x, v : vs') -> {-# SCC step_TeeLocal #-} do
          frame <- view configFrame
          mut <- lift $ local frame x
          writeMutVar mut v
          k (v : vs') es

        (GetGlobal x, vs) -> {-# SCC step_GetGlobal #-} do
          inst <- getFrameInst
          g <- lift . lift . Global.load =<< lift (global inst x)
          -- traceM $ "GetGlobal " ++ show (value x) ++ " = " ++ show g
          k (g : vs) es

        (SetGlobal x, v : vs') -> {-# SCC step_SetGlobal #-} do
          inst <- getFrameInst
          g <- lift $ global inst x
          eres <- lift $ lift $ runExceptT $ Global.store g v
          case eres of
            Right () -> k vs' es
            Left err -> throwError $ EvalCrashError at $ case err of
              Global.GlobalNotMutable -> "write to immutable global"
              Global.GlobalTypeError  -> "type mismatch at global write"

        (Load op, I32 i : vs') -> {-# SCC step_Load #-} do
          inst <- getFrameInst
          mem <- lift $ memory inst (0 @@ at)
          let addr = fromIntegral $ i64_extend_u_i32 (fromIntegral i)
          let off = fromIntegral (op^.memoryOffset)
          let ty = op^.memoryValueType
          eres <- lift $ lift $ runExceptT $ case op^.memorySize of
                Nothing        -> Memory.loadValue mem addr off ty
                Just (sz, ext) -> Memory.loadPacked sz ext mem addr off ty
          case eres of
            Right v' -> k (v' : vs') es
            Left exn -> k vs' (Trapping (memoryErrorString exn) @@ at : es)

        (Store op, v : I32 i : vs') -> {-# SCC step_Store #-} do
          inst <- getFrameInst
          mem <- lift $ memory inst (0 @@ at)
          let addr = fromIntegral $ i64_extend_u_i32 (fromIntegral i)
          let off = fromIntegral (op^.memoryOffset)
          eres <- lift $ lift $ runExceptT $ case op^.memorySize of
                Nothing -> Memory.storeValue mem addr off v
                Just sz -> Memory.storePacked sz mem addr off v
          case eres of
            Right () -> k vs' es
            Left exn -> k vs' (Trapping (memoryErrorString exn) @@ at : es)

        (MemorySize, vs) -> {-# SCC step_MemorySize #-} do
          inst <- getFrameInst
          mem  <- lift $ memory inst (0 @@ at)
          sz   <- lift $ lift $ Memory.size mem
          k (I32 sz : vs) es

        (MemoryGrow, I32 delta : vs') -> {-# SCC step_MemoryGrow #-} do
          inst    <- getFrameInst
          mem     <- lift $ memory inst (0 @@ at)
          oldSize <- lift $ lift $ Memory.size mem
          eres    <- lift $ lift $ runExceptT $ Memory.grow mem delta
          let result = case eres of
                  Left _   -> -1
                  Right () -> oldSize
          k (I32 result : vs') es

        (Const v, vs) -> {-# SCC step_Const #-}
          k (value v : vs) es

        (Test testop, v : vs') -> {-# SCC step_Test #-} do
          let eres = case testop of
                I32TestOp o -> testOp @Int32 intTestOp o v
                I64TestOp o -> testOp @Int64 intTestOp o v
          case eres of
            Left err -> k vs' (Trapping (show err) @@ at : es)
            Right v' -> k (v' : vs') es

        (Compare relop, v2 : v1 : vs') -> {-# SCC step_Compare #-} do
          let eres = case relop of
                I32CompareOp o -> compareOp @Int32 intRelOp o v1 v2
                I64CompareOp o -> compareOp @Int64 intRelOp o v1 v2
                F32CompareOp o -> compareOp @Float floatRelOp o v1 v2
                F64CompareOp o -> compareOp @Double floatRelOp o v1 v2
          case eres of
            Left err -> k vs' (Trapping (show err) @@ at : es)
            Right v' -> k (v' : vs') es

        (Unary unop, v : vs') -> {-# SCC step_Unary #-} do
          let eres = case unop of
                I32UnaryOp o -> unaryOp @Int32 intUnOp o v
                I64UnaryOp o -> unaryOp @Int64 intUnOp o v
                F32UnaryOp o -> unaryOp @Float floatUnOp o v
                F64UnaryOp o -> unaryOp @Double floatUnOp o v
          case eres of
            Left err -> k vs' (Trapping (show err) @@ at : es)
            Right v' -> k (v' : vs') es

        (Binary binop, v2 : v1 : vs') -> {-# SCC step_Binary #-} do
          let eres = case binop of
                I32BinaryOp o -> binaryOp @Int32 intBinOp o v1 v2
                I64BinaryOp o -> binaryOp @Int64 intBinOp o v1 v2
                F32BinaryOp o -> binaryOp @Float floatBinOp o v1 v2
                F64BinaryOp o -> binaryOp @Double floatBinOp o v1 v2
          case eres of
            Left err -> k vs' (Trapping (show err) @@ at : es)
            Right v' -> k (v' : vs') es

        (Convert cvtop, v : vs') -> {-# SCC step_Convert #-} do
          let eres = case cvtop of
                I32ConvertOp o -> intCvtOp @Int32 o v
                I64ConvertOp o -> intCvtOp @Int64 o v
                F32ConvertOp o -> floatCvtOp @Float o v
                F64ConvertOp o -> floatCvtOp @Double o v
          case eres of
            Left err -> k vs' (Trapping (show err) @@ at : es)
            Right v' -> k (v' : vs') es

        _ ->  {-# SCC step_fallthrough_ #-} do
          let s1 = show (reverse vs)
              s2 = show (map Values.typeOf (reverse vs))
          throwError $ EvalCrashError at
            ("missing or ill-typed operand on stack (" ++ s1 ++ " : " ++ s2 ++ ")")

      (EmptyCS, Trapping msg) -> {-# SCC step_Trapping #-}
        throwError $ EvalTrapError at msg
      (EmptyCS, Returning _)  -> {-# SCC step_Returning #-}
        throwError $ EvalCrashError at "undefined frame"
      (EmptyCS, Breaking _ _) -> {-# SCC step_Breaking #-}
        throwError $ EvalCrashError at "undefined label"

      (Label _ _ code', Trapping msg) -> {-# SCC step_Label_Trap #-}
        return $ code' { _codeInstrs = Trapping msg @@ at : _codeInstrs code' }
      (Label _ _ code', Returning vs0) -> {-# SCC step_Label_Return #-}
        return $ code' { _codeInstrs = Returning vs0 @@ at : _codeInstrs code' }
      (Label n es0 code', Breaking 0 vs0) -> {-# SCC step_Label_Break1 #-} do
        vs0' <- lift $ takeFrom n vs0 at
        return $ code'
            { _codeStack = vs0' ++ _codeStack code'
            , _codeInstrs = es0 $ _codeInstrs code'
            }
      (Label _ _ code', Breaking bk vs0) -> {-# SCC step_Label_Break2 #-}
        return $ code' { _codeInstrs = Breaking (bk - 1) vs0 @@ at : _codeInstrs code' }

      (Framed _ code', Trapping msg) -> {-# SCC step_Framed_Trap #-}
        return $ code' { _codeInstrs = Trapping msg @@ at : _codeInstrs code' }
      (Framed n code', Returning vs0) -> {-# SCC step_Framed_Return #-} do
        vs0' <- lift $ takeFrom n vs0 at
        return $ code' { _codeStack = vs0' ++ _codeStack code' }
      (Framed _ _, Breaking _ _) -> {-# SCC step_Framed_Break #-}
        throwError $ EvalCrashError at "undefined label"

      (_, Invoke func) -> {-# SCC step_Invoke #-} do
        budget <- view configBudget
        when (budget == 0) $
          throwError $ EvalExhaustionError at "call stack exhausted"

        let FuncType ins outs = Func.typeOf func
            n = length ins

        (reverse -> args, vs') <-
          if n > length vs
          then throwError $ EvalCrashError at "stack underflow"
          else pure $ splitAt n vs

        -- traceM $ "Invoke: ins  = " ++ show ins
        -- traceM $ "Invoke: args = " ++ show args
        -- traceM $ "Invoke: outs = " ++ show outs
        -- traceM $ "Invoke: vs'  = " ++ show vs'

        lift $ checkTypes at ins args

        case func of
          Func.AstFunc _ ref f -> do
            inst' <- getInst ref
            locals' <- traverse newMutVar $
              args ++ map defaultValue (value f^.funcLocals)
            return $ Code
                (Framed (length outs) (Code cs cfg vs' es))
                (cfg & configFrame .~ Frame inst' locals' & configBudget %~ pred)
                []
                [Plain (Fix (Block outs (value f^.funcBody))) @@ region f]

          Func.HostFunc _ f -> do
            -- jww (2018-11-01): Need an exception handler here, so we can
            -- report host errors.
            let res = reverse (f args)
            lift $ checkTypes at outs res
            k (res ++ vs') es
            -- try (reverse (f args) ++ vs', [])
            -- with Crash (_, msg) -> EvalCrashError at msg)

          Func.HostFuncEff _ f -> do
            -- jww (2018-11-01): Need an exception handler here, so we can
            -- report host errors.
            res' <- lift $ lift $ f args
            case res' of
              Left err -> throwError $ EvalTrapError at err
              Right (reverse -> res) -> do
                lift $ checkTypes at outs res
                k (res ++ vs') es
                -- try (reverse (f args) ++ vs', [])
                -- with Crash (_, msg) -> EvalCrashError at msg)

{-# SPECIALIZE step :: Code Identity IO -> EvalT IO (Code Identity IO) #-}
{-# SPECIALIZE step :: Code Identity (ST s) -> EvalT (ST s) (Code Identity (ST s)) #-}
{-# SPECIALIZE step :: Code Phrase IO -> EvalT IO (Code Phrase IO) #-}
{-# SPECIALIZE step :: Code Phrase (ST s) -> EvalT (ST s) (Code Phrase (ST s)) #-}

eval :: (Regioned f, PrimMonad m, Show1 f)
     => Code f m -> EvalT m (Stack Value)
eval (Code EmptyCS _ vs []) = return vs
eval c = step c >>= eval

{-# SPECIALIZE eval :: Code Identity IO -> EvalT IO (Stack Value) #-}
{-# SPECIALIZE eval :: Code Identity (ST s) -> EvalT (ST s) (Stack Value) #-}
{-# SPECIALIZE eval :: Code Phrase IO -> EvalT IO (Stack Value) #-}
{-# SPECIALIZE eval :: Code Phrase (ST s) -> EvalT (ST s) (Stack Value) #-}

{- Functions & Constants -}

invoke :: (Regioned f, PrimMonad m, Show1 f)
       => IntMap (ModuleInst f m)
       -> ModuleInst f m
       -> ModuleFunc f m
       -> [Value]
       -> EvalT m [Value]
invoke mods inst func vs = do
  let (at, inst') = case func of
        Func.AstFunc _ i f -> (region f, mods^?!ix i)
        _ -> (def, inst)
  let cfg = newConfig mods inst'
  reverse <$> eval (Code EmptyCS cfg (reverse vs) [Invoke func @@ at])
  -- jww (2018-11-01): How do we detect stack overflow?
  -- reverse (eval c) with Stack_overflow ->
  --   Exhaustion.error at "call stack exhausted"

{-# SPECIALIZE invoke
      :: IntMap (ModuleInst Identity IO)
      -> ModuleInst Identity IO
      -> ModuleFunc Identity IO
      -> [Value]
      -> EvalT IO [Value] #-}

{-# SPECIALIZE invoke
      :: IntMap (ModuleInst Identity (ST s))
      -> ModuleInst Identity (ST s)
      -> ModuleFunc Identity (ST s)
      -> [Value]
      -> EvalT (ST s) [Value] #-}

{-# SPECIALIZE invoke
      :: IntMap (ModuleInst Phrase IO)
      -> ModuleInst Phrase IO
      -> ModuleFunc Phrase IO
      -> [Value]
      -> EvalT IO [Value] #-}

{-# SPECIALIZE invoke
      :: IntMap (ModuleInst Phrase (ST s))
      -> ModuleInst Phrase (ST s)
      -> ModuleFunc Phrase (ST s)
      -> [Value]
      -> EvalT (ST s) [Value] #-}

invokeByName :: (Regioned f, PrimMonad m, Show1 f)
             => IntMap (ModuleInst f m) -> ModuleInst f m -> Text -> [Value]
             -> EvalT m [Value]
invokeByName mods inst name vs = do
  -- traceM $ "invokeByName " ++ unpack name
  case inst ^. miExports.at name of
    Just (ExternFunc f) -> invoke mods inst f vs
    e -> throwError $ EvalCrashError def $
      "Cannot invoke export " ++ unpack name ++ ": " ++ show e

{-# SPECIALIZE invokeByName
      :: IntMap (ModuleInst Identity IO)
      -> ModuleInst Identity IO -> Text -> [Value] -> EvalT IO [Value] #-}

{-# SPECIALIZE invokeByName
      :: IntMap (ModuleInst Identity (ST s))
      -> ModuleInst Identity (ST s) -> Text -> [Value] -> EvalT (ST s) [Value] #-}

{-# SPECIALIZE invokeByName
      :: IntMap (ModuleInst Phrase IO)
      -> ModuleInst Phrase IO -> Text -> [Value] -> EvalT IO [Value] #-}

{-# SPECIALIZE invokeByName
      :: IntMap (ModuleInst Phrase (ST s))
      -> ModuleInst Phrase (ST s) -> Text -> [Value] -> EvalT (ST s) [Value] #-}

getByName :: (Regioned f, Show1 f, PrimMonad m)
          => ModuleInst f m -> Text -> EvalT m Value
getByName inst name = case inst ^. miExports.at name of
  Just (ExternGlobal g) -> readMutVar (g^.Global.giContent)
  e -> throwError $ EvalCrashError def $
    "Cannot get exported global " ++ unpack name ++ ": " ++ show e

evalConst :: (Regioned f, PrimMonad m, Show1 f)
          => IntMap (ModuleInst f m)
          -> ModuleInst f m -> Expr f -> EvalT m Value
evalConst mods inst expr = do
  let cfg = newConfig mods inst
  xs <- eval (Code EmptyCS cfg [] (map plain (value expr)))
  case xs of
    [v] -> pure v
    _ -> throwError $
      EvalCrashError (region expr) "wrong number of results on stack"

i32 :: Monad m => Value -> Region -> EvalT m Int32
i32 v at = case v of
  I32 i -> pure i
  _ -> throwError $ EvalCrashError at "type error: i32 value expected"

{- Modules -}

createFunc :: (Regioned f, Monad m)
           => ModuleInst f m -> ModuleRef -> f (Func f)
           -> EvalT m (ModuleFunc f m)
createFunc inst ref f = do
  ty <- type_ inst (value f^.funcType)
  pure $ Func.alloc ty ref f

createHostFunc :: FuncType -> ([Value] -> [Value]) -> ModuleFunc f m
createHostFunc = Func.allocHost

createHostFuncEff :: FuncType -> ([Value] -> m (Either String [Value])) -> ModuleFunc f m
createHostFuncEff = Func.allocHostEff

createTable :: (Regioned f, PrimMonad m)
            => Table f -> EvalT m (TableInst m (ModuleFunc f m))
createTable tab = do
  eres <- lift $ runExceptT $ Table.alloc (value tab)
  case eres of
    Left err -> throwError $ EvalTableError (region tab) err
    Right g  -> pure g

liftMem :: Monad m
        => Region -> ExceptT Memory.MemoryError m a -> EvalT m a
liftMem at act = do
  eres <- lift $ runExceptT act
  case eres of
    Left err -> throwError $ EvalMemoryError at err
    Right x  -> pure x

createMemory :: (Regioned f, PrimMonad m)
             => Memory f -> EvalT m (Memory.MemoryInst m)
createMemory mem = liftMem (region mem) $ Memory.alloc (value mem)

createGlobal :: (Regioned f, PrimMonad m, Show1 f)
             => IntMap (ModuleInst f m) -> ModuleInst f m -> f (Global f)
             -> EvalT m (Global.GlobalInst m)
createGlobal mods inst x@(value -> glob) = do
  v <- evalConst mods inst (glob^.globalValue)
  eres <- lift $ runExceptT $ Global.alloc (glob^.globalType) v
  case eres of
    Left err -> throwError $ EvalGlobalError (region x) err
    Right g  -> pure g

createExport :: (Regioned f, Monad m)
             => ModuleInst f m -> f (Export f) -> EvalT m (ExportInst f m)
createExport inst (value -> ex) = do
  ext <- case ex^.exportDesc of
    FuncExport   x -> ExternFunc   <$> func inst x
    TableExport  x -> ExternTable  <$> table inst x
    MemoryExport x -> ExternMemory <$> memory inst x
    GlobalExport x -> ExternGlobal <$> global inst x
  pure $ M.singleton (ex^.exportName) ext

initTable :: (Regioned f, Show1 f, PrimMonad m)
          => IntMap (ModuleInst f m) -> ModuleInst f m -> f (TableSegment f)
          -> EvalT m ()
initTable mods inst s@(value -> seg) = do
  tab <- table inst (seg^.segmentIndex)
  c <- evalConst mods inst (seg^.segmentOffset)
  offset <- i32 c (region (seg^.segmentOffset))
  let end_ = offset + fromIntegral (length (seg^.segmentInit))
  bound <- lift $ Table.size tab
  when (bound < end_ || end_ < offset) $
    throwError $ EvalLinkError (region s) "elements segment does not fit table"
  fs <- traverse (func inst) (seg^.segmentInit)
  lift $ Table.blit tab offset (V.fromList fs)

initMemory :: (Regioned f, Show1 f, PrimMonad m)
           => IntMap (ModuleInst f m) -> ModuleInst f m -> f (MemorySegment f)
           -> EvalT m ()
initMemory mods inst s@(value -> seg) = do
  mem <- memory inst (seg^.segmentIndex)
  c <- evalConst mods inst (seg^.segmentOffset)
  offset' <- i32 c (region (seg^.segmentOffset))
  let offset = i64_extend_u_i32 (fromIntegral offset')
  let end_ = offset + fromIntegral (B.length (seg^.segmentInit))
  bound <- lift $ Memory.bound mem
  when (fromIntegral bound < end_ || end_ < fromIntegral offset) $
    throwError $ EvalLinkError (region s) "data segment does not fit memory"
  liftMem (region s) $
    Memory.storeBytes mem (fromIntegral offset)
                      (V.fromList (B.unpack (seg^.segmentInit)))

addImport :: (Regioned f, PrimMonad m)
          => ModuleInst f m
          -> Extern f m
          -> f (Import f)
          -> EvalT m (ModuleInst f m)
addImport inst ext im = do
  typ <- lift $ externTypeOf ext
  if not (matchExternType typ (importTypeFor (inst^.miModule) (value im)))
    then throwError $ EvalLinkError (region im) "incompatible import type"
    else pure $ case ext of
      ExternFunc func   -> inst & miFuncs    %~ (func :)
      ExternTable tab   -> inst & miTables   %~ (tab  :)
      ExternMemory mem  -> inst & miMemories %~ (mem  :)
      ExternGlobal glob -> inst & miGlobals  %~ (glob :)

resolveImports :: (Regioned f, Show1 f, PrimMonad m)
               => Map Text ModuleRef
               -> IntMap (ModuleInst f m)
               -> ModuleInst f m
               -> EvalT m (ModuleInst f m)
resolveImports names mods inst = flip execStateT inst $
  forM_ (reverse (inst^.miModule.moduleImports)) $ \im -> do
    let im' = value im
    case M.lookup (im'^.importModule) names of
      Nothing -> throwError $ EvalLinkError (region im) $
        "Missing module for import: " ++ show (value im)
      Just ref -> case IM.lookup ref mods of
        Nothing -> throwError $ EvalLinkError (region im) $
          "Missing module for import: " ++ show (value im)
        Just src ->
          case M.lookup (im'^.importItem) (src^.miExports) of
            Nothing -> throwError $ EvalLinkError (region im) $
              "Missing extern for import: " ++ show (value im)
            Just ext -> do
              m <- get
              m' <- lift $ addImport m ext im
              put m'

initialize :: (Regioned f, Show1 f, PrimMonad m)
           => f (Module f)
           -> Map Text ModuleRef
           -> IntMap (ModuleInst f m)
           -> EvalT m (ModuleRef, ModuleInst f m)
initialize (value -> mod) names mods = do
  inst <- resolveImports names mods (emptyModuleInst mod)
  let ref = nextKey mods
  inst' <- flip execStateT inst $ do
    ts <- lift $ traverse createTable (mod^.moduleTables)
    fs <- lift $ traverse (createFunc inst ref) (mod^.moduleFuncs)
    ms <- lift $ traverse createMemory (mod^.moduleMemories)
    gs <- lift $ traverse (createGlobal mods inst) (mod^.moduleGlobals)

    miFuncs    %= (<> (fs & traverse.Func._AstFunc._2 .~ ref))
    miTables   %= (<> ts)
    miMemories %= (<> ms)
    miGlobals  %= (<> gs)

    inst1 <- get
    let mods1 = IM.insert ref inst1 mods
    forM_ (mod^.moduleElems) $ lift . initTable mods1 inst1
    forM_ (mod^.moduleData)  $ lift . initMemory mods1 inst1

    inst2 <- get
    es <- lift $ traverse (createExport inst2) (mod^.moduleExports)
    miExports .= mconcat es

    inst3 <- get
    forM_ (mod^.moduleStart) $ \start -> do
      f <- lift $ func inst3 start
      lift $ invoke (IM.insert ref inst3 mods) inst3 f []

  pure (ref, inst')

{-# SPECIALIZE initialize
           :: Identity (Module Identity)
           -> Map Text ModuleRef
           -> IntMap (ModuleInst Identity IO)
           -> EvalT IO (ModuleRef, ModuleInst Identity IO) #-}

{-# SPECIALIZE initialize
           :: Identity (Module Identity)
           -> Map Text ModuleRef
           -> IntMap (ModuleInst Identity (ST s))
           -> EvalT (ST s) (ModuleRef, ModuleInst Identity (ST s)) #-}

{-# SPECIALIZE initialize
           :: Phrase (Module Phrase)
           -> Map Text ModuleRef
           -> IntMap (ModuleInst Phrase IO)
           -> EvalT IO (ModuleRef, ModuleInst Phrase IO) #-}

{-# SPECIALIZE initialize
           :: Phrase (Module Phrase)
           -> Map Text ModuleRef
           -> IntMap (ModuleInst Phrase (ST s))
           -> EvalT (ST s) (ModuleRef, ModuleInst Phrase (ST s)) #-}

nextKey :: IntMap a -> IM.Key
nextKey m = go (max 1 (IM.size m))
 where
  go k | IM.member k m = go (succ k)
       | otherwise = k
