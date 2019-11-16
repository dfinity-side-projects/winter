{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Util.Source where

import           Control.DeepSeq
import           Data.Default.Class
import           Data.Functor.Classes
import           Data.Functor.Identity
import           Data.Int
import           Data.Maybe
import           GHC.Generics
import           Lens.Micro.Platform
import           Text.Printf

infixl 6 @@

class Traversable phrase => Regioned phrase where
  region :: phrase a -> Region
  (@@) :: a -> Region -> phrase a

instance Regioned Identity where
  region = def
  {-# INLINE region #-}
  (@@) x = const $ Identity x
  {-# INLINE (@@) #-}

data Position
  = Position
  { _posFile   :: !FilePath
  , _posLine   :: !Int64
  , _posColumn :: !Int64
  } deriving (Eq, Generic, NFData)

instance Default Position where
  def = Position def def def
  {-# INLINE def #-}

instance Show Position where
  show Position {..} =
    if _posLine == -1
    then printf "0x%x" _posColumn
    else printf "%d.%d" _posLine $ succ _posColumn

data Region
  = Region
  { _regionLeft  :: !Position
  , _regionRight :: !Position
  } deriving (Eq, Generic, NFData)

instance Default Region where
  def = Region def def
  {-# INLINE def #-}

instance Show Region where
  show Region {..} =
    printf "%s:%s" file area
    where file = _posFile _regionLeft
          area = show _regionLeft ++
            if _regionLeft == _regionRight
            then "" else "-" ++ show _regionRight

data Phrase a
  = Phrase
  { _phraseAt :: !Region
  , _phraseIt :: a
  } deriving (Eq, Functor, Generic, Generic1, NFData, NFData1)

instance Default a => Default (Phrase a) where
  def = Phrase def def
  {-# INLINE def #-}

instance Foldable Phrase where
  foldMap f Phrase {..} = f _phraseIt
  {-# INLINE foldMap #-}

instance Applicative Phrase where
  pure x = x @@ def
  {-# INLINE pure #-}
  (Phrase ff f) <*> (Phrase _ x) = f x @@ ff
  {-# INLINE (<*>) #-}

instance Regioned Phrase where
  region = _phraseAt
  {-# INLINE region #-}
  (@@) = flip Phrase
  {-# INLINE (@@) #-}

instance Show a => Show (Phrase a) where
  showsPrec _ Phrase {..} =
    showsPrec 11 _phraseIt .
    showString " @@ " .
    showsPrec 11 _phraseAt

instance Show1 Phrase where
  liftShowsPrec showIt _ _ Phrase {..} =
    showIt 11 _phraseIt .
    showString " @@ " .
    showsPrec 11 _phraseAt

instance Traversable Phrase where
  sequenceA Phrase {..} = Phrase _phraseAt <$> _phraseIt
  {-# INLINE sequenceA #-}

makeLenses ''Position
makeLenses ''Region
makeLenses ''Phrase

move :: Position -> Char -> Position
move pos = \case
  '\t' -> over posColumn tab pos
  '\n' -> over posLine succ $ set posColumn 1 pos
  _    -> over posColumn succ pos
  where tab n = 8 * div (n + 7) 8 + 1

need :: Traversal' record field -> record -> field
need field = fromMaybe err . preview field where err = error "need: Nothing"

{-# INLINE need #-}

value :: Traversable phrase => phrase a -> a
value = need traverse

{-# INLINE value #-}

{-# SPECIALIZE value :: Phrase a -> a #-}
{-# SPECIALIZE value :: Identity a -> a #-}
