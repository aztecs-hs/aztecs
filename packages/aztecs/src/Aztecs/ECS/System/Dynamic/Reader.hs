{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Dynamic.Reader
  ( -- * Dynamic Systems
    DynamicReaderSystem (..),
    ArrowDynamicReaderSystem (..),
    raceDyn,
  )
where

import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader)
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import Aztecs.ECS.World.Archetypes (Node)
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Parallel (par)
import Data.Set (Set)

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

instance ArrowDynamicReaderSystem DynamicQueryReader DynamicReaderSystem where
  allDyn cIds q = DynamicReaderSystem $ allDyn' cIds q
  filterDyn cIds q f = DynamicReaderSystem $ filterDyn' cIds q f

raceDyn :: DynamicReaderSystem i a -> DynamicReaderSystem i b -> DynamicReaderSystem i (a, b)
raceDyn (DynamicReaderSystem f) (DynamicReaderSystem g) = DynamicReaderSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      a = fa
      b = gbPar
   in (a, b)

allDyn' :: Set ComponentID -> DynamicQueryReader i o -> World -> i -> [o]
allDyn' cIds q w = let !v = V.view cIds $ archetypes w in \i -> V.readAllDyn i q v

filterDyn' ::
  Set ComponentID ->
  DynamicQueryReader i o ->
  (Node -> Bool) ->
  World ->
  i ->
  [o]
filterDyn' cIds q f w = let !v = V.filterView cIds f $ archetypes w in \i -> V.readAllDyn i q v
