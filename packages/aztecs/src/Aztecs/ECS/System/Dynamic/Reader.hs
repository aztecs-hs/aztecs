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

import Aztecs.ECS.Access (Access)
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
    runReaderSystemDyn :: World -> i -> (o, Access ())
  }
  deriving (Functor)

instance Category DynamicReaderSystem where
  id = DynamicReaderSystem $ \_ i -> (i, pure ())
  DynamicReaderSystem f . DynamicReaderSystem g = DynamicReaderSystem $ \w i ->
    let (b, gAccess) = g w i
        (c, fAccess) = f w b
     in (c, gAccess >> fAccess)

instance Arrow DynamicReaderSystem where
  arr f = DynamicReaderSystem $ \_ i -> (f i, pure ())
  first (DynamicReaderSystem f) = DynamicReaderSystem $ \w (i, x) -> let (a, access) = f w i in ((a, x), access)

instance ArrowDynamicReaderSystem DynamicQueryReader DynamicReaderSystem where
  allDyn cIds q = DynamicReaderSystem $ \w i ->
    let !v = V.view cIds $ archetypes w in (V.readAllDyn i q v, pure ())
  filterDyn cIds q f = DynamicReaderSystem $ \w i ->
    let !v = V.filterView cIds f $ archetypes w in (V.readAllDyn i q v, pure ())

raceDyn :: DynamicReaderSystem i a -> DynamicReaderSystem i b -> DynamicReaderSystem i (a, b)
raceDyn (DynamicReaderSystem f) (DynamicReaderSystem g) = DynamicReaderSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, fAccess) = fa
      (b, gAccess) = gbPar
   in ((a, b), fAccess >> gAccess)
