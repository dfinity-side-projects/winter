module Wat2Wasm (wat2Wasm) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           System.Directory
import           System.Exit
import           System.IO.Temp
import           System.Process

wat2Wasm :: String -> IO ByteString
wat2Wasm contents = do
  wat  <- emptyTempFile "." "test.wat"
  wasm <- emptyTempFile "." "test.wasm"
  writeFile wat contents
  (exit, _out, err) <-
    readProcessWithExitCode "wat2wasm" [wat, "-o", wasm] ""
  case exit of
    ExitSuccess   -> do
      res <- BL.readFile wasm
      removeFile wat
      removeFile wasm
      return res
    ExitFailure _ ->
      fail err
