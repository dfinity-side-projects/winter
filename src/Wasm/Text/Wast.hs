{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a parser for Wast scripts, used by the WebAssembly specification.

module Wasm.Text.Wast
  ( Script
  , Cmd(..)
  , Assertion(..)
  , Action(..)
  , CheckState(..)
  , WasmEngine(..)
  , _Constant
  , script
  , parseWastFile
  ) where

import           Control.Applicative
import           Control.Exception.Lifted hiding (try)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.State
import           Data.Kind (Type)
import           Data.Bifunctor
import           Data.Bits ((.|.), (.&.), shiftL, complement)
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Char8 as Byte (pack)
import           Data.Char
import           Data.Functor.Identity
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List (isInfixOf)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Numbers.FloatingHex
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy as Text (pack)
import           Lens.Micro.Platform
import           Text.Parsec hiding ((<|>), many, optional,
                                     digit, hexDigit, octDigit)
import           Text.Parsec.Language (haskellDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token as P

import           Wasm.Util.Float

{-
import           Wasm.Binary.Decode
import           Wasm.Exec.Eval hiding (Invoke, invoke, elem)
import           Wasm.Runtime.Instance
import           Wasm.Runtime.Mutable
import qualified Wasm.Syntax.AST as AST
import           Wasm.Syntax.Values
import           Wasm.Util.Source
-}

-- import           Debug.Trace

class Show (Value w) => WasmEngine w m where
  type Value w :: Type
  type Module w :: Type
  type ModuleInst w m :: Type

  const_i32 :: Int32 -> Value w
  const_i64 :: Int64 -> Value w
  const_f32 :: Float -> Value w
  const_f64 :: Double -> Value w

  decodeModule :: ByteString -> Either String (Module w)

  initializeModule
    :: Module w -> Map Text ModuleRef -> IntMap (ModuleInst w m)
    -> m (Either String (ModuleRef, ModuleInst w m, Maybe String))

  invokeByName
    :: IntMap (ModuleInst w m) -> ModuleInst w m -> Text
    -> [Value w] -> m (Either String ([Value w], ModuleInst w m))
  getByName
    :: ModuleInst w m -> Text
    -> m (Either String (Value w, ModuleInst w m))

type LineNumber = Int
type Script w = [(LineNumber, Cmd w)]
type Name = Text

data Cmd w
  = CmdModule ModuleDecl
  | CmdRegister String (Maybe Name)
  | CmdAction (Action w)
  | CmdAssertion (Assertion w)
  | CmdMeta (Meta w)

deriving instance Show (Value w) => Show (Cmd w)

data Tree = Leaf String | Node [Tree]

instance Show Tree where
  showsPrec _ (Leaf x) = showString x
  showsPrec _ (Node xs) = showString "(" . showl xs . showString ")"
   where
    showl [] = id
    showl (y:ys) = shows y . showl ys

tree :: Parser Tree
tree = node <|> leaf
  where
    node = Node <$> between (char '(') (char ')') (many tree)
    leaf = Leaf <$> many1 (noneOf "()")

data ModuleDecl
  = ModuleDecl (Maybe Name) String
  | ModuleBinary (Maybe Name) ByteString
  | ModuleQuote (Maybe Name) String
  deriving Show

data Action w
  = ActionInvoke (Maybe Name) String [Expr w]
  | ActionGet    (Maybe Name) String

deriving instance Show (Value w) => Show (Action w)

type Failure = String

data Assertion w
  = AssertReturn (Action w) [Expr w]
  | AssertReturnCanonicalNan (Action w)
  | AssertReturnArithmeticNan (Action w)
  | AssertTrap (Action w) Failure
  | AssertMalformed ModuleDecl Failure
  | AssertInvalid ModuleDecl Failure
  | AssertUnlinkable ModuleDecl Failure
  | AssertTrapModule ModuleDecl Failure
  | AssertExhaustion (Action w) Failure

deriving instance Show (Value w) => Show (Assertion w)

data Meta w
  = MetaScript (Maybe Name) (Script w)
  | MetaInput (Maybe Name) String
  | MetaOutput (Maybe Name) (Maybe String)

deriving instance Show (Value w) => Show (Meta w)

data Expr w
  = Constant (Value w)
  | Invoke String [Expr w]

deriving instance Show (Value w) => Show (Expr w)

_Constant :: Traversal' (Expr w) (Value w)
_Constant f (Constant x) = Constant <$> f x
_Constant _ x = pure x

_Invoke :: Traversal' (Expr w) (String, [Expr w])
_Invoke f (Invoke x y) = (\(x',y') -> Invoke x' y') <$> f (x, y)
_Invoke _ x = pure x

keyword :: String -> Parser ()
keyword k = string k *> whiteSpace

lang :: P.GenLanguageDef String u Identity
lang = haskellDef
  { P.commentStart = "(;"
  , P.commentEnd   = ";)"
  , P.commentLine  = ";"
  }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser lang

withLine :: Parser a -> Parser (LineNumber, a)
withLine p =
 (,) <$> (sourceLine <$> getPosition) <*> p

script :: forall w m. WasmEngine w m => Parser (Script w)
script = some (whiteSpace *> withLine (cmd @_ @m)) <* whiteSpace <* eof

name :: Parser Name
name = fmap Text.pack . (:)
  <$> char '$' <*> some (satisfy (\c -> isAlphaNum c || c == '_'))
  <* whiteSpace

expr :: forall w m. WasmEngine w m => Parser (Expr w)
expr = do
  _ <- char '(' *> whiteSpace
  x <-   try constant
    <|> invoke
  _ <- whiteSpace *> char ')' *> whiteSpace
  return x
 where
  constant =  go "i32.const" (const_i32 @w @m) (fromIntegral <$> negOr_ int)
          <|> go "f32.const" (const_f32 @w @m) (negOr_ float32)
          <|> go "i64.const" (const_i64 @w @m) (fromIntegral <$> negOr_ int)
          <|> go "f64.const" (const_f64 @w @m) (negOr_ float64)
   where
    int :: Parser Integer
    int         = do{ f <- P.lexeme lexer sign
                    ; n <- nat
                    ; return (f n)
                    }

    sign        =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

    nat :: Parser Integer
    nat         = zeroNumber <|> decimal

    zeroNumber  = do{ _ <- char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""

    digit    = satisfy (\c -> isDigit c || c == '_')    <?> "digit"
    hexDigit = satisfy (\c -> isHexDigit c || c == '_') <?> "hexadecimal digit"
    octDigit = satisfy (\c -> isOctDigit c || c == '_') <?> "octal digit"

    decimal     = number 10 digit
    hexadecimal = do{ _ <- oneOf "xX"; number 16 hexDigit }
    octal       = do{ _ <- oneOf "oO"; number 8 octDigit  }

    number base baseDigit
        = do{ digits <- filter (/= '_') <$> many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }

    negOr_ :: Num a => Parser a -> Parser a
    negOr_ p = do
      neg <- optional (char '-' *> whiteSpace)
      x <- p
      return $ case neg of Just _ -> negate x; Nothing -> x

    float32 :: Parser Float
    float32 = float setFloatPayload f32CanonicalNaN

    float64 :: Parser Double
    float64 = float setDoublePayload f64CanonicalNaN

    float :: FloatingHexReader a => (a -> Integer -> a) -> a -> Parser a
    float set_payload canonical_nan =  try (fromRational . toRational <$> P.float lexer)
         <|> try (parse_nan set_payload canonical_nan)
         <|> try (1 / 0 <$ keyword "inf")
         <|> try floatingHex
         <|> (fromIntegral <$> int)
     where
      floatingHex :: FloatingHexReader a => Parser a
      floatingHex = do
        x <- some (satisfy (\c -> isHexDigit c
                            || c == 'x' || c == 'X'
                            || c == 'p' || c == 'e'
                            || c == 'P' || c == 'E'
                            || c == '.' || c == '+' || c == '-'))
        let mres = readHFloat x <|> readHFloat (x ++ "p0")
        maybe mzero pure mres

    -- Both canonical and arithmetic NaNs are parsed to canonical NaN. This is
    -- more strict than necessary: if a test result is expected to be an
    -- arithmetic NaN we can accept any of the possible arithmetic NaN values,
    -- but this is easier to implement, and makes the programs more (according
    -- to wasmtime, entirely) deterministic.
    --
    -- I think ideally we'd return a sum here with alternatives:
    --
    -- - AnyNaN -- expect a NaN value with the given sign, payload doesn't matter (`nan` in wast)
    -- - CanonicalNaN -- expect a canonical NaN (`nan:canonical` in wast)
    -- - ArithmeticNaN -- accept any arithmetic NaN (`nan:arithmetic` in wast)
    -- - SpecificNaN -- accept only the given NaN (`nan:0x...` in wast)
    --
    -- and then decide how to compare a float result with the expected NaN
    -- value, but I think that will require significant amount of refactoring,
    -- so this will do for now.
    parse_nan :: (a -> Integer -> a) -> a -> Parser a
    parse_nan set_payload canonical_nan = do
        keyword "nan"
        optional (char ':') >>= \case
          Nothing ->
            pure canonical_nan
          Just _ ->
            ((string "canonical" <|> string "arithmetic") >> pure canonical_nan) <|>
            (string "0x" >> hex_payload >>= pure . set_payload canonical_nan)

    hex_payload :: Parser Integer
    hex_payload = number 16 hexDigit

    go k f p = try $ keyword k *> (Constant . f <$> p)

  invoke = keyword "invoke" *> (Invoke <$> string_ <*> many (expr @_ @m))

setFloatPayload :: Float -> Integer -> Float
setFloatPayload f p =
    assert (p >= 1 && p <= ((1 `shiftL` 23) - 1)) $
    floatFromBits ((floatToBits f .&. complement ((1 `shiftL` 23) - 1)) .|. fromIntegral p)

setDoublePayload :: Double -> Integer -> Double
setDoublePayload d p =
    assert (p >= 1 && p <= ((1 `shiftL` 52) - 1)) $
    doubleFromBits ((doubleToBits d .&. complement ((1 `shiftL` 52) - 1)) .|. fromIntegral p)

cmd :: forall w m. WasmEngine w m => Parser (Cmd w)
cmd = do
  x <-   CmdModule    <$> try module_
    <|> uncurry
        CmdRegister  <$> try register
    <|> CmdAction    <$> try (action @_ @m)
    <|> CmdAssertion <$> try (assertion @_ @m)
    <|> CmdMeta      <$> try (meta @_ @m)
  return x
 where
  register = do
    _ <- char '(' *> whiteSpace
    _ <- keyword "register"
    whiteSpace
    res <- (,) <$> string_ <*> optional name
    _ <- whiteSpace *> char ')' *> whiteSpace
    return res

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer

literal :: Parser String
literal = do
  _ <- char '"'
  str <- many char'
  _ <- char '"'
  return str
 where
  char' :: Parser Char
  char'
    =  try (satisfy $ \c ->
               c /= '"' &&
               c /= '\\' &&
               not ('\x00' <= c && c <= '\x1f') &&
               not ('\x7f' <= c && c <= '\xff'))
   <|> try (read <$> utf8enc)   -- jww (2018-11-02): I don't think this is correct
   <|> try (do _ <- char '\\'
               c <- oneOf "nrt\\'\""
               case c of
                 'n'  -> pure '\n'
                 'r'  -> pure '\r'
                 't'  -> pure '\t'
                 '\\' -> pure '\\'
                 '\'' -> pure '\''
                 '"'  -> pure '"'
                 _    -> mzero)
   <|> try (do _ <- char '\\'
               h <- hexdigit
               l <- hexdigit
               let n = read ['0', 'x', h, l] :: Int
               pure $ chr n)
   <|> try (do _ <- char '\\'
               _ <- char 'u'
               _ <- char '{'
               x <- hexnum
               _ <- char '}'
               pure x)

  hexnum = do
    h <- hexdigit
    s <- many (optional (char '_') *> hexdigit)
    let n = read ('0':'x':h:s) :: Int
    pure $ chr n

  utf8cont :: Parser Char
  utf8cont = inRange '\x80' '\xbf'

  utf8enc :: Parser String
  utf8enc
     =  (\x y -> [x, y]) <$> inRange '\xc2' '\xdf' <*> utf8cont
    <|> (\x y z -> [x, y, z]) <$> char '\xe0' <*> inRange '\xa0' '\xbf' <*> utf8cont
    <|> (\x y z -> [x, y, z]) <$> char '\xed' <*> inRange '\x80' '\x9f' <*> utf8cont
    <|> (\x y z -> [x, y, z])
          <$> (inRange '\xe1' '\xec' <|> inRange '\xee' '\xef')
          <*> utf8cont <*> utf8cont
    <|> (\x y z w -> [x, y, z, w])
          <$> char '\xf0' <*> inRange '\x90' '\xbf' <*> utf8cont <*> utf8cont
    <|> (\x y z w -> [x, y, z, w])
          <$> char '\xf4' <*> inRange '\x80' '\x8f' <*> utf8cont <*> utf8cont
    <|> (\x y z w -> [x, y, z, w])
          <$> inRange '\xf1' '\xf3' <*> utf8cont <*> utf8cont <*> utf8cont


inRange :: Char -> Char -> Parser Char
inRange x y = satisfy (\c -> x <= c && c <= y)

hexdigit :: Parser Char
hexdigit = inRange '0' '9' <|> inRange 'a' 'f' <|> inRange 'A' 'F'

module_ :: Parser ModuleDecl
module_ = do
  x <-   try (do nm <- char '(' *> whiteSpace *> keyword "module" *> optional name
                 keyword "binary" *>
                   (ModuleBinary nm . Byte.pack . concat
                     <$> many (literal <* whiteSpace) <* char ')'))
    <|> try (do nm <- char '(' *> whiteSpace *> keyword "module" *> optional name
                keyword "quote" *>
                  (ModuleQuote nm . concat
                    <$> many string_ <* char ')'))
    <|> (do nm <- lookAhead $ char '(' *> whiteSpace *> keyword "module" *> optional name
            ModuleDecl nm <$> show <$> tree)
  -- traceM $ "x = " ++ show x
  whiteSpace
  return x

action :: forall w m. WasmEngine w m => Parser (Action w)
action = do
  _ <- char '(' *> whiteSpace
  x <-   go "invoke" (ActionInvoke <$> optional name <*> string_ <*> many (expr @_ @m))
    <|> go "get"    (ActionGet    <$> optional name <*> string_)
  _ <- whiteSpace *> char ')' *> whiteSpace
  return x
 where
  go k f = try $ keyword k *> f

failure :: Parser Failure
-- failure = P.stringLiteral lexer <* whiteSpace
failure = literal <* whiteSpace

string_ :: Parser String
-- string_ = P.stringLiteral lexer <* whiteSpace
string_ = literal <* whiteSpace

assertion :: forall w m. WasmEngine w m => Parser (Assertion w)
assertion = do
  _ <- char '(' *> whiteSpace
  x <-  go "return"                (AssertReturn <$> action @_ @m <*> many (expr @_ @m))
    <|> go "return_canonical_nan"  (AssertReturnCanonicalNan <$> action @_ @m)
    <|> go "return_arithmetic_nan" (AssertReturnArithmeticNan <$> action @_ @m)
    <|> go "trap"                  (AssertTrap <$> action @_ @m <*> failure)
    <|> go "malformed"             (AssertMalformed <$> module_ <*> failure)
    <|> go "invalid"               (AssertInvalid <$> module_ <*> failure)
    <|> go "unlinkable"            (AssertUnlinkable <$> module_ <*> failure)
    <|> go "trap"                  (AssertTrapModule <$> module_ <*> failure)
    <|> go "exhaustion"            (AssertExhaustion <$> action @_ @m <*> failure)
  _ <- whiteSpace *> char ')' *> whiteSpace
  return x
 where
  go k f = try $ keyword ("assert_" ++ k) *> f

meta :: forall w m. WasmEngine w m => Parser (Meta w)
meta = do
  _ <- char '(' *> whiteSpace
  x <-   go "script" (MetaScript <$> optional name <*> script @_ @m)
    <|> go "input"  (MetaInput  <$> optional name <*> string_)
    <|> go "output" (MetaOutput <$> optional name <*> optional string_)
  _ <- whiteSpace *> char ')' *> whiteSpace
  return x
 where
  go k f = try $ keyword k *> f

type ModuleRef = Int

data CheckState w m = CheckState
    { _checkStateRef     :: ModuleRef
    , _checkStateNames   :: Map Text ModuleRef
    , _checkStateModules :: IntMap (ModuleInst w m)
    }

makeLenses ''CheckState

newCheckState :: Map Text ModuleRef -> IntMap (ModuleInst w m) -> CheckState w m
newCheckState names mods =
  assert (M.size names == IM.size mods) $
    CheckState
      { _checkStateRef     = M.size names
      , _checkStateNames   = names
      , _checkStateModules = mods
      }

prettyAction :: WasmEngine w m => Action w -> String
prettyAction (ActionInvoke _ nm args) =
  let args' = args^..traverse._Constant in
  nm ++ " " ++ show args'
prettyAction (ActionGet _ nm) = nm

invokeAction
  :: forall w m. (Control.Monad.Fail.MonadFail m, MonadBaseControl IO m, WasmEngine w m)
  => Action w
  -> (Either String ([Value w], ModuleInst w m) -> StateT (CheckState w m) m ())
  -> StateT (CheckState w m) m ()
invokeAction (ActionInvoke mname nm args) k = do
  CheckState ref names mods <- get
  let args' = args^..traverse._Constant
      ref'  = case mname of
                Nothing -> ref
                Just n -> names^?!ix n
  mres <- use (checkStateModules.at ref')
  case mres of
    Nothing ->
      fail $ "Failed to look up module: " ++ show ref'
    Just inst ->
      catch (do eres <- lift $ invokeByName @w @m mods inst (Text.pack nm) args'
                k eres) $ \(exc :: SomeException) ->
        unless ("wasm function signature contains illegal type" `isInfixOf` show exc) $
          throwIO exc
invokeAction (ActionGet mname nm) k = do
  CheckState ref names mods <- get
  let ref'  = case mname of
                Nothing -> ref
                Just n -> names^?!ix n
  case IM.lookup ref' mods of
    Nothing ->
      fail $ "Failed to look up module: " ++ show ref'
    Just inst -> do
      eres <- lift $ getByName @w @m inst (Text.pack nm)
      k (first (:[]) <$> eres)

invokeModule
  :: forall w m. (MonadFail m, WasmEngine w m)
  => (String -> m ByteString)
     -- ^ Convert module into Wasm binary
  -> ModuleDecl
  -> (Either String (Maybe String) -> StateT (CheckState w m) m ())
     -- ^ Continuation. Argument is one of these:
     --
     -- - Left err:         Instantiation failed with "err". In this case the module
     --                     won't be available.
     --
     -- - Right Nothing:    Instantiation successful, start function did not trap.
     --
     -- - Right (Just err): Instantiation successful, but start function
     --                     trapped. Trapping in start function doesn't make the
     --                     module unavailable.
  -> StateT (CheckState w m) m ()
invokeModule readModule decl k = do
  (mname, wasm) <- case decl of
    ModuleDecl mname sexp -> (mname,) <$> lift (readModule sexp)
    ModuleBinary mname wasm -> return (mname, wasm)
    ModuleQuote _ _ -> fail "unexpected ModuleQuote"
  case decodeModule @w @m wasm of
    Left err ->
      k $ Left $  "Error decoding binary wasm: " ++ err
    Right (m :: Module w) -> do
      CheckState _ names mods <- get
      eres <- lift $ initializeModule @w @m m names mods
      case eres of
        Left err ->
          k $ Left $ "Error initializing module " ++ err
        Right (ref, inst, start_err) -> do
          checkStateRef .= ref
          checkStateModules.at ref ?= inst
          forM_ mname $ \nm -> checkStateNames.at nm ?= ref
          k (Right start_err)

parseWastFile
  :: forall w m. (MonadFail m, MonadBaseControl IO m, WasmEngine w m)
  => FilePath
  -> String
  -> Map Text ModuleRef
  -> IntMap (ModuleInst w m)
  -> (String -> m ByteString)                       -- convert module into Wasm binary
  -> (String -> m ())                               -- names a step
  -> ([Value w] -> [Value w] -> Bool)
  -> (String -> m ())                               -- a negative assertion
  -> m ()
parseWastFile path input preNames preMods readModule step valEq assertFailure =
  case runP (script @w @m) () path input of
    Left err -> fail $ show err
    Right wast -> flip evalStateT (newCheckState preNames preMods) $
      forM_ wast $ \(l,c) -> lift (step ("line " ++ show l)) >> case c of
        CmdModule moddecl ->
          invokeModule readModule moddecl $ \case
              Left e -> lift $ assertFailure e
              Right _ -> return ()

        CmdAssertion e -> case e of
          AssertReturn a exps -> do
            let exps' = exps^..traverse._Constant
            invokeAction a $ lift . \case
              Left err -> assertFailure $ "assert_return failed: " ++ err
              Right (vals, _mod) ->
                unless (valEq vals exps') $ assertFailure $
                  prettyAction @w @m a ++ " == " ++ show exps' ++ "\n" ++
                  "expected: " ++ show exps' ++ "\n" ++
                  " but got: " ++ show vals

          AssertTrap a msg ->
            invokeAction a $ \case
              Left _ -> return ()
              Right _ -> lift $ assertFailure $ "Did not trap, expected " ++ msg

          AssertTrapModule moddecl msg ->
            invokeModule readModule moddecl $ \case
              Left _ -> return ()
              Right (Just _err) -> return ()
              Right Nothing -> lift $ assertFailure $ "Did not trap, expected " ++ msg

          AssertReturnCanonicalNan _act  -> return ()
          AssertReturnArithmeticNan _act -> return ()
          AssertMalformed _mod' _exp     -> return ()
          AssertInvalid _mod' _exp       -> return ()
          AssertUnlinkable _mod' _exp    -> return ()
          AssertExhaustion _act _exp     -> return ()

        CmdAction (ActionInvoke mname nm args) -> do
          CheckState ref names mods <- get
          let ref' = case mname of
                       Nothing -> ref
                       Just n -> names^?!ix n
          mres <- use (checkStateModules.at ref')
          case mres of
            Nothing ->
              fail $ "Failed to look up module: " ++ show ref'
            Just inst -> do
              eres <- lift $ invokeByName @w @m mods inst
                (Text.pack nm) (args^..traverse._Constant)
              case eres of
                Left err ->
                  fail $ "Error invoking: "
                      ++ nm ++ " " ++ show args ++ ": " ++ show err
                Right _ -> pure ()

        -- Register takes a module, and creates Externs for each of its exported
        -- members under the given name.
        CmdRegister str mname -> do
          CheckState ref names _mods <- get
          let ref' = case mname of
                       Nothing -> ref
                       Just n -> names^?!ix n
          checkStateNames.at (Text.pack str) ?= ref'

        e -> fail $ "unexpected: " ++ show e
