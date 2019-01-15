module Wasm.Util.Show where

import           Data.Functor.Classes
import           Text.Show

showPrec :: Show a => Int -> a -> ShowS
showPrec = showsPrec

showListPrec :: Show a => Int -> [a] -> ShowS
showListPrec = showListWith . showPrec

showListListPrec :: Show a => Int -> [[a]] -> ShowS
showListListPrec = showListWith . showListPrec

showLiftPrec :: (Show a, Show1 f1) => Int -> f1 a -> ShowS
showLiftPrec = showsPrec1

showListLiftPrec :: (Show a, Show1 f1) => Int -> [f1 a] -> ShowS
showListLiftPrec = showListWith . showLiftPrec

showListListLiftPrec :: (Show a, Show1 f1) => Int -> [[f1 a]] -> ShowS
showListListLiftPrec = showListWith . showListLiftPrec

showLiftLiftPrec :: (Show a, Show1 f1, Show1 f2) => Int -> f1 (f2 a) -> ShowS
showLiftLiftPrec d = liftShowsPrec showLiftPrec (showListLiftPrec d) d

showLiftListLiftPrec
  :: (Show a, Show1 f1, Show1 f2) => Int -> f1 [f2 a] -> ShowS
showLiftListLiftPrec d =
  liftShowsPrec showLiftLiftPrec (showListListLiftPrec d) d

showListLiftListLiftPrec
  :: (Show a, Show1 f1, Show1 f2) => Int -> [f1 [f2 a]] -> ShowS
showListLiftListLiftPrec = showListWith . showLiftListLiftPrec

showLiftLiftListLiftPrec
  :: (Show a, Show1 f1, Show1 f2, Show1 f3) => Int -> f1 (f2 [f3 a]) -> ShowS
showLiftLiftListLiftPrec d =
  liftShowsPrec showLiftListLiftPrec (showListLiftListLiftPrec d) d

showListLiftLiftListLiftPrec
  :: (Show a, Show1 f1, Show1 f2, Show1 f3) => Int -> [f1 (f2 [f3 a])] -> ShowS
showListLiftLiftListLiftPrec = showListWith . showLiftLiftListLiftPrec
