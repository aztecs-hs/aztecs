{-# LANGUAGE DeriveFunctor #-}

module Aztecs.ECS.System.Dynamic.Reader
  ( -- * Dynamic Systems
    DynamicReaderSystem (..),
    ArrowDynamicReaderSystem (..),
    raceDyn,
  )
where

import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Aztecs.ECS.World (World (..))
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Parallel (par)

newtype DynamicReaderSystem i o = DynamicReaderSystem
  { -- | Run a dynamic system producing some output
    runReaderSystemDyn :: World -> i -> o
  }
  deriving (Functor)

instance Category DynamicReaderSystem where
  id = DynamicReaderSystem $ \_ i -> i
  DynamicReaderSystem f . DynamicReaderSystem g = DynamicReaderSystem $ \w i -> let b = g w i in f w b

instance Arrow DynamicReaderSystem where
  arr f = DynamicReaderSystem $ \_ i -> f i
  first (DynamicReaderSystem f) = DynamicReaderSystem $ \w (i, x) -> let a = f w i in (a, x)

instance ArrowDynamicReaderSystem DynamicReaderSystem where
  runArrowReaderSystemDyn = DynamicReaderSystem

raceDyn :: DynamicReaderSystem i a -> DynamicReaderSystem i b -> DynamicReaderSystem i (a, b)
raceDyn (DynamicReaderSystem f) (DynamicReaderSystem g) = DynamicReaderSystem $ \w -> \i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      a = fa
      b = gbPar
   in (a, b)
