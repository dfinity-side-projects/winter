{-# LANGUAGE RecordWildCards #-}

module Wasm.Binary.Lift where

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Functor.Identity

import           Wasm.Util.Source

class Get1 phrase where
  liftGet :: Get a -> Get (phrase a)

instance Get1 Identity where
  liftGet = fmap Identity

instance Get1 Phrase where
  liftGet = getPhrase

class Put1 phrase where
  liftPut :: (a -> Put) -> phrase a -> Put

instance Put1 Identity where
  liftPut putValue = putValue . runIdentity

instance Put1 Phrase where
  liftPut = putPhrase

getPosition :: Get Position
getPosition = do
  column <- bytesRead
  return $ Position "region" (-1) column

getPhrase :: Get a -> Get (Phrase a)
getPhrase getValue = do
  left  <- getPosition
  val   <- getValue
  right <- getPosition
  return $ val @@ Region left right

putPhrase :: (a -> Put) -> Phrase a -> Put
putPhrase putValue Phrase {..} = putValue _phraseIt
