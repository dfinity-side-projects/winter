{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec (tests) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCaseSteps, assertEqual, assertFailure)

import           Wasm.Text.Wast (parseWastFile)
import           Wasm.Text.Winter (Winter)
import           Wasm.Util.Source (Phrase)

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
      wat2Wasm step assertEqual assertFailure
