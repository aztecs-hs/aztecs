{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System
  ( -- * Systems
    System,
    SystemT (..),
    queue,
    task,

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
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.System.Class (ArrowSystem (..), filterMap, map, mapSingle, map_, queue)
import Data.Aztecs.System.Dynamic (DynamicSystemT (..), raceDyn)
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicSystemReader (runArrowSystemReaderDyn))
import Data.Aztecs.System.Reader.Class (ArrowSystemReader (..), all, filter, single)
import Data.Aztecs.World.Components (Components)
import Prelude hiding (all, filter, map, (.))
import qualified Prelude hiding (filter, map)

type System i o = SystemT IO i o

-- | System to process entities.
newtype SystemT m i o = SystemT
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runSystemT :: Components -> (DynamicSystemT m i o, ReadsWrites, Components)
  }
  deriving (Functor)

instance (Monad m) => Category (SystemT m) where
  id = SystemT $ \cs -> (DynamicSystemT $ \_ -> \i -> return (i, mempty, pure ()), mempty, cs)
  SystemT f . SystemT g = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow (SystemT IO) where
  arr f = SystemT $ \cs -> (DynamicSystemT $ \_ -> \i -> return (f i, mempty, pure ()), mempty, cs)
  first (SystemT f) = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
     in (first f', rwsF, cs')
  a &&& b = SystemT $ \cs ->
    let (dynA, rwsA, cs') = runSystemT a cs
        (dynB, rwsB, cs'') = runSystemT b cs'
     in ( if Q.disjoint rwsA rwsB
            then dynA &&& dynB
            else raceDyn dynA dynB,
          rwsA <> rwsB,
          cs''
        )

instance ArrowSystemReader IO (SystemT IO) where
  runArrowSystemReader f = SystemT $ \cs ->
    let (g, rs, cs') = f cs in (runArrowSystemReaderDyn g, ReadsWrites rs mempty, cs')

instance ArrowSystem IO (SystemT IO) where
  runArrowSystem f = SystemT $ \cs ->
    let (g, rs, cs') = f cs in (DynamicSystemT g, rs, cs')

-- | Run a monadic task from some input.
task :: (Monad m) => (i -> m o) -> SystemT m i o
task f = SystemT $ \cs ->
  ( DynamicSystemT $ \_ -> \i -> do
      o <- f i
      return (o, mempty, pure ()),
    mempty,
    cs
  )
