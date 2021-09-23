{-# LANGUAGE OverloadedStrings #-}

module SpecTest (spectest) where

import           Control.Monad.Except
import           Data.Default.Class
import qualified Data.Map as M
import qualified Data.Vector as V
import           Lens.Micro.Platform
import           Text.Printf

import qualified Wasm.Exec.Eval as Eval
import qualified Wasm.Runtime.Func as Func
import qualified Wasm.Runtime.Global as Global
import           Wasm.Runtime.Instance
import qualified Wasm.Runtime.Memory as Memory
import qualified Wasm.Runtime.Table as Table
import           Wasm.Syntax.Types
import qualified Wasm.Syntax.Values as Values
import           Wasm.Util.Source (Phrase)

spectest :: IO (ModuleInst Phrase IO)
spectest = do
  table0  <- ret <$> runExceptT table
  memory0 <- ret <$> runExceptT memory
  global0 <- ret <$> runExceptT (global (GlobalType I32Type Immutable))
  global1 <- ret <$> runExceptT (global (GlobalType F32Type Immutable))
  global2 <- ret <$> runExceptT (global (GlobalType F64Type Immutable))
  let print0 = pr []                     -- print
      print1 = pr [I32Type]              -- print_i32
      print2 = pr [I32Type, F32Type]     -- print_i32_f32
      print3 = pr [F64Type, F64Type]     -- print_f64_f64
      print4 = pr [F32Type]              -- print_f32
      print5 = pr [F64Type]              -- print_f64
  pure $ emptyModuleInst def
    & miGlobals .~
      (V.fromList
        [ global0
        , global1
        , global2
        ])
    & miTables .~ V.singleton table0
    & miMemories .~ V.singleton memory0
    & miFuncs .~
      (V.fromList
      [ print0
      , print1
      , print2
      , print3
      , print4
      , print5
      , Eval.getInstructionCount
      ])
    & miExports .~ M.fromList
      [ ("print",         ExternFunc print0)
      , ("print_i32",     ExternFunc print1)
      , ("print_i32_f32", ExternFunc print2)
      , ("print_f64_f64", ExternFunc print3)
      , ("print_f32",     ExternFunc print4)
      , ("print_f64",     ExternFunc print5)
          -- To use this, one would include the following in their Wast file:
          --   (import "spectest" "instr_count" (func $instr_count (result i64)))
          --   (call $instr_count)
          -- The result of this call is the current instruction count thus
          -- far, including the call to $instr_count itself.
      , ("instr_count",   ExternFunc Eval.getInstructionCount)
      , ("global_i32",    ExternGlobal global0)
      , ("global_f32",    ExternGlobal global1)
      , ("global_f64",    ExternGlobal global2)
      , ("table",         ExternTable table0)
      , ("memory",        ExternMemory memory0)
      ]
 where
  ret :: Show e => Either e a -> a
  ret = either (error.show) id

  pr ins = Func.allocHostEff (FuncType ins []) (const print_)

  print_ :: [Values.Value] -> IO (Either String [Values.Value])
  print_ vs = Right [] <$ mapM_ print_value vs

  print_value v = printf "%s : %s\n" (show v) (show (Values.typeOf v))

  global gt@(GlobalType t _) = Global.alloc gt $ case t of
    I32Type -> Values.I32 666
    I64Type -> Values.I64 666
    F32Type -> Values.F32 666.6
    F64Type -> Values.F64 666.6

  table  = Table.alloc (TableType AnyFuncType (Limits 10 (Just 20)))
  memory = Memory.alloc (Limits 1 (Just 2))

