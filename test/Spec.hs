{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec (tests) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCaseSteps, assertFailure)

import           Wasm.Text.Wast (parseWastFile)
import           Wasm.Text.Winter (Winter)
import           Wasm.Util.Source (Phrase)
import           Wasm.Syntax.Values (Value (..))

import           SpecTest (spectest)
import           Wat2Wasm (wat2Wasm)

tests :: [FilePath] -> TestTree
tests files = testGroup "spec" $ map prep files
 where
  prep file = testCaseSteps file $ \step -> do
    input <- Prelude.readFile file
    inst  <- spectest
    parseWastFile @(Winter Phrase) @IO file input
      (M.singleton "spectest" 1)
      (IM.singleton 1 inst)
      wat2Wasm step valListEq assertFailure

valListEq :: [Value] -> [Value] -> Bool
valListEq vs1 vs2 =
    length vs1 == length vs2 &&
    all (uncurry valEq) (zip vs1 vs2)

valEq :: Value -> Value -> Bool
valEq v1 v2 = case (v1, v2) of
  (I32 i1, I32 i2) -> i1 == i2
  (I64 i1, I64 i2) -> i1 == i2
  (F32 f1, F32 f2) -> isNaN f1 && isNaN f2 || f1 == f2
  (F64 f1, F64 f2) -> isNaN f1 && isNaN f2 || f1 == f2
  _ -> False
