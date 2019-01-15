{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec (tests) where

import           Control.Monad (void)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, assertEqual, assertFailure)

import           Wasm.Text.SpecTest (spectest)
import           Wasm.Text.Wast (parseWastFile)
import           Wasm.Text.Wat2Wasm (wat2Wasm)
import           Wasm.Text.Winter (Winter)
import           Wasm.Util.Source (Phrase)

tests :: [FilePath] -> TestTree
tests files = testGroup "spec" $ map prep files
 where
  prep file = testCase file $ do
    input <- Prelude.readFile file
    inst  <- spectest
    void $ parseWastFile @(Winter Phrase) @IO file input
      (M.singleton "spectest" 1)
      (IM.singleton 1 inst)
      wat2Wasm assertEqual assertFailure
