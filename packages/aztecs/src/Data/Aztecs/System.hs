{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.System
  ( -- * Systems
    System (..),
    ArrowReaderSystem (..),
    ArrowSystem (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.Query (ReadsWrites (..))
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.System.Class (ArrowSystem (..), filterMap, map, mapSingle, map_, queue)
import Data.Aztecs.System.Dynamic (DynamicSystem (..), raceDyn)
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (runArrowReaderSystemDyn))
import Data.Aztecs.System.Reader.Class (ArrowReaderSystem (..), all, filter, single)
import Data.Aztecs.World.Components (Components)
import Prelude hiding (all, filter, map, (.))
import qualified Prelude hiding (filter, map)

-- | System to process entities.
newtype System i o = SystemT
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runSystem :: Components -> (DynamicSystem i o, ReadsWrites, Components)
  }
  deriving (Functor)

instance Category System where
  id = SystemT $ \cs -> (DynamicSystem $ \_ i -> (i, mempty, pure ()), mempty, cs)
  SystemT f . SystemT g = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow System where
  arr f = SystemT $ \cs -> (DynamicSystem $ \_ i -> (f i, mempty, pure ()), mempty, cs)
  first (SystemT f) = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs in (first f', rwsF, cs')
  f &&& g = SystemT $ \cs ->
    let (dynF, rwsA, cs') = runSystem f cs
        (dynG, rwsB, cs'') = runSystem g cs'
     in ( if Q.disjoint rwsA rwsB then dynF &&& dynG else raceDyn dynF dynG,
          rwsA <> rwsB,
          cs''
        )

instance ArrowReaderSystem System where
  runArrowReaderSystem f = SystemT $ \cs ->
    let (g, rs, cs') = f cs in (runArrowReaderSystemDyn g, ReadsWrites rs mempty, cs')

instance ArrowSystem System where
  runArrowSystem f = SystemT $ \cs ->
    let (g, rs, cs') = f cs in (DynamicSystem g, rs, cs')
