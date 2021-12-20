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
import           Wasm.Util.Float (floatToBits, doubleToBits)

import           SpecTest (spectest)
import           Wat2Wasm (wat2Wasm)

tests :: [String] -> String -> [FilePath] -> TestTree
tests languageFlags name files = testGroup name $ map prep files
 where
  prep file = testCaseSteps file $ \step -> do
    input <- Prelude.readFile file
    inst  <- spectest
    parseWastFile @(Winter Phrase) @IO file input
      (M.singleton "spectest" 1)
      (IM.singleton 1 inst)
      (wat2Wasm languageFlags) step valListEq assertFailure

valListEq :: [Value] -> [Value] -> Bool
valListEq vs1 vs2 =
    length vs1 == length vs2 &&
    all (uncurry valEq) (zip vs1 vs2)

-- Note: we compare float values bitwise. This is more strict than necessary:
-- when a test expects an arithmetic NaN we can accept any of the arithmetic
-- NaNs for the type, but this is actually easier to implement and is more
-- deterministic too.
valEq :: Value -> Value -> Bool
valEq v1 v2 = case (v1, v2) of
  (I32 i1, I32 i2) -> i1 == i2
  (I64 i1, I64 i2) -> i1 == i2
  (F32 f1, F32 f2) -> floatToBits f1 == floatToBits f2
  (F64 f1, F64 f2) -> doubleToBits f1 == doubleToBits f2
  _ -> False
