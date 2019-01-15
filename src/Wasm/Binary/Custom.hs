{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasm.Binary.Custom where

import           Data.Binary.Get
import           Data.List                     as List
import           Data.Text.Lazy

import           Wasm.Binary.Decode
import           Wasm.Binary.LEB128
import           Wasm.Syntax.AST

getNameSection :: Module phrase -> Maybe Custom
getNameSection = getCustomSection "name"

getCustomSection :: Text -> Module phrase -> Maybe Custom
getCustomSection key ast = case List.filter matches $ _moduleCustom ast of
  [c] -> Just c
  _ -> Nothing
  where
    matches = ((==) key) . _customName

getModuleName :: Module phrase -> Maybe Text
getModuleName ast = do
  case getNameSection ast of
    Just custom -> do
      runGet getModuleName' (_customPayload custom)
    Nothing -> Nothing
  where
    getModuleName' = do
      done <- isEmpty
      if done then
        return Nothing
      else do
        byte <- getWord8
        case byte of
          0x00 -> do
            _size :: Int <- getULEB128 32
            name <- getText 32
            return $ Just name
          _ -> do
            size <- getULEB128 32
            skip size
            getModuleName'
