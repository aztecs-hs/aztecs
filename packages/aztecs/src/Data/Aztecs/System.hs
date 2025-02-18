{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System
  ( -- * Systems
    System(..),
    queue,

    -- ** Queries

    -- *** Reading
    all,
    filter,
    single,

    -- *** Writing
    map,
    map_,
    filterMap,
    mapSingle,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.Query (ReadsWrites (..))
import Data.Aztecs.System.Class (ArrowSystem (..), filterMap, map, mapSingle, map_, queue)
import Data.Aztecs.System.Dynamic (DynamicSystemT (..))
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicSystemReader (runArrowSystemReaderDyn))
import Data.Aztecs.System.Reader.Class (ArrowSystemReader (..), all, filter, single)
import Data.Aztecs.World.Components (Components)
import Prelude hiding (all, filter, map, (.))
import qualified Prelude hiding (filter, map)

-- | System to process entities.
newtype System i o = SystemT
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runSystem :: Components -> (DynamicSystemT i o, ReadsWrites, Components)
  }
  deriving (Functor)

instance Category System where
  id = SystemT $ \cs -> (DynamicSystemT $ \_ -> \i -> (i, mempty, pure ()), mempty, cs)
  SystemT f . SystemT g = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow System where
  arr f = SystemT $ \cs -> (DynamicSystemT $ \_ -> \i -> (f i, mempty, pure ()), mempty, cs)
  first (SystemT f) = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
     in (first f', rwsF, cs')

{-TODO
a &&& b = SystemT $ \cs ->
  let (dynA, rwsA, cs') = runSystemT a cs
      (dynB, rwsB, cs'') = runSystemT b cs'
   in ( if Q.disjoint rwsA rwsB
          then dynA &&& dynB
          else raceDyn dynA dynB,
        rwsA <> rwsB,
        cs''
      )
-}

instance ArrowSystemReader System where
  runArrowSystemReader f = SystemT $ \cs ->
    let (g, rs, cs') = f cs in (runArrowSystemReaderDyn g, ReadsWrites rs mempty, cs')

instance ArrowSystem System where
  runArrowSystem f = SystemT $ \cs ->
    let (g, rs, cs') = f cs in (DynamicSystemT g, rs, cs')
