module Main where

import Data.List
import System.Directory
import System.Environment
import Test.Tasty (defaultMain, testGroup)

import Property as Property (tests)
import Unit as Unit (tests)
import Spec as Spec (tests)

main :: IO ()
main = do
  mwasmPath <- lookupEnv "WASM_SPEC_TESTS"
  testDir <- case mwasmPath of
      Nothing -> error "Please define WASM_SPEC_TESTS to point to .../WebAssebly/spec/test/core"
      Just path -> return path
  putStrLn $ "Using wasm spec test directory: " ++ testDir
  files <- listDirectory testDir
  let wastFiles = flip concatMap files $ \file ->
        [ testDir ++ "/" ++ file
        | ".wast" `isSuffixOf` file
            && "inline-module.wast" /= file
              -- jww (2018-11-02): We aren't going to bother fully supporting
              -- Unicode function names in the reference interpreter yet.
            && "names.wast" /= file
              -- jww (2018-11-03): We need more accurate floating-point support.
            && "f32_bitwise.wast" /= file
            && "f64_bitwise.wast" /= file
            && "float_literals.wast" /= file
            && "float_exprs.wast" /= file
            && "conversions.wast" /= file
            && "address.wast" /= file
              -- jww (2018-11-03): Strange behavior from shl
            && "i32.wast" /= file
            && "i64.wast" /= file
        ]

  defaultMain $ testGroup "main"
    [ Property.tests
    , Unit.tests
    , Spec.tests wastFiles
    ]
