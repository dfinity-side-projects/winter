{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import Control.Applicative           ((<|>))
import Control.Monad                 (void)
import Control.Monad.Except          (runExceptT)
import Data.Binary.Get               (runGet)
import Data.Default.Class            (Default (..))
import Data.Functor.Identity         (Identity (..))
import Data.Map                      (singleton, fromList)
import Data.Text.Lazy                (pack)
import System.Console.CmdArgs        (Data, cmdArgs)
import Text.ParserCombinators.Parsec (Parser)

import qualified Data.ByteString.Lazy          as Lazy
import qualified Data.IntMap                   as IntMap
import qualified Text.ParserCombinators.Parsec as Parsec

import Wasm.Binary.Decode    (getModule)
import Wasm.Exec.Eval        (initialize, invokeByName)
import Wasm.Runtime.Func     (allocHostEff)
import Wasm.Runtime.Instance (Extern (..), ModuleInst(..), emptyModuleInst)
import Wasm.Syntax.AST       ()
import Wasm.Syntax.Types     (FuncType (..), ValueType (..))
import Wasm.Syntax.Values    (Value (..))

data Options
  = Options
  { func :: String
  , args :: String
  , wasm :: FilePath
  } deriving Data

instance Default Options where
  def = Options "sum" "i32.1,i32.1" "sum.wasm"

parseValues :: Parser [Value]
parseValues = do
  value <- parseValue
  Parsec.option [value] . Parsec.try $ do
    void $ Parsec.char ','
    (:) value <$> parseValues

parseValue :: Parser Value
parseValue =
  Parsec.try parseI32
    <|> Parsec.try parseI64
    <|> Parsec.try parseF32
    <|> Parsec.try parseF64

parseI32 :: Parser Value
parseI32 = do
  void $ Parsec.string "i32."
  I32 <$> parseInt

parseI64 :: Parser Value
parseI64 = do
  void $ Parsec.string "i64."
  I64 <$> parseInt

parseF32 :: Parser Value
parseF32 = do
  void $ Parsec.string "f32."
  F32 <$> parseFloat

parseF64 :: Parser Value
parseF64 = do
  void $ Parsec.string "f64."
  F64 <$> parseFloat

parseSign :: Num a => Parser (a -> a)
parseSign = Parsec.option id . Parsec.try $ do
  void $ Parsec.char '-'
  return negate

parseInt :: Integral a => Parser a
parseInt = do
  sign <- parseSign
  sign <$> do
    digits <- Parsec.many1 Parsec.digit
    let number = foldl step 0 digits
    return number
 where
  step acc x = (cast x - cast '0') + acc * 10
  cast = fromIntegral . fromEnum

parseFloat :: Floating a => Parser a
parseFloat = do
  sign <- parseSign
  sign <$> do
    prefix <- Parsec.many1 Parsec.digit
    let whole = foldl stepl 0 prefix
    Parsec.option whole . Parsec.try $ do
      void $ Parsec.char '.'
      suffix <- Parsec.many1 Parsec.digit
      let part   = foldr stepr 0 suffix
      let number = whole + part
      return number
 where
  stepl acc x = (cast x - cast '0') + acc * 10
  stepr x acc = (cast x - cast '0' + acc) / 10
  cast = realToFrac . fromEnum

printI32 :: [Value] -> IO [Value]
printI32 [value] = [] <$ print value
printI32 _ = fail "printI32: invalid argument"

app :: ModuleInst Identity IO
app =
  let printI32' = allocHostEff (FuncType [I32Type] []) printI32
  in (emptyModuleInst def)
    { _miGlobals  = [ ]
    , _miTables   = [ ]
    , _miMemories = [ ]
    , _miFuncs    = [ printI32' ]
    , _miExports  = fromList [ ("printI32", ExternFunc printI32') ]
    }

main :: IO ()
main = do
  Options {..} <- cmdArgs def
  case Parsec.parse parseValues "args" args of
    Left  err    -> fail $ show err
    Right values -> do
      bytes <- Lazy.readFile wasm
      let ast = Identity $ runGet getModule bytes
      result <- runExceptT $ do
        let names = singleton "Main" 1
            mods  = IntMap.singleton 1 app
        (ref, inst) <- initialize ast names mods
        let name = pack func
        invokeByName (IntMap.insert ref inst mods) inst name values
      case result of
        Left  err     -> fail $ show err
        Right outputs -> print outputs
