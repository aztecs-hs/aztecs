{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Aztecs.ECS.System
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.System.Dynamic
  ( -- * Dynamic systems
    DynamicSystemT (..),
    runDynamicSystemT,

    -- ** Queries
    all,
    filter,
    map,
    mapSingleMaybe,
    filterMap,
    filterMapM,
  )
where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic (DynamicQuery)
import qualified Aztecs.ECS.Query.Dynamic as DQ
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Entities (Entities)
import Data.Set (Set)
import Data.Vector (Vector)
import Prelude hiding (all, filter, map, mapM)

-- | Query operation.
data Op m a where
  -- | All entities matching a dynamic query.
  QAll :: DynamicQuery m a -> Op m (Vector a)
  -- | Filtered entities matching a dynamic query.
  QFilter :: DynamicQuery m a -> (Node m -> Bool) -> Op m (Vector a)
  -- | Map entities matching a dynamic query.
  QMap :: DynamicQuery m a -> Op m (Vector a)
  -- | Map single entity matching a dynamic query.
  QMapSingleMaybe :: DynamicQuery m a -> Op m (Maybe a)
  -- | Filter and map entities matching a dynamic query.
  QFilterMap :: (Node m -> Bool) -> DynamicQuery m a -> Op m (Vector a)

-- | Run a query operation on entities.
runOp :: (Monad m) => Set ComponentID -> Op m a -> Entities m -> m (a, Entities m, Access m ())
runOp cIds (QAll q) es = do
  as <- DQ.readQueryDyn cIds q es
  return (as, es, return ())
runOp cIds (QFilter q flt) es = do
  as <- DQ.readQueryFilteredDyn cIds flt q es
  return (as, es, return ())
runOp cIds (QMap q) es = DQ.queryDyn cIds q es
runOp cIds (QMapSingleMaybe q) es = DQ.querySingleMaybeDyn cIds q es
runOp cIds (QFilterMap flt q) es = DQ.queryFilteredDyn cIds flt q es
{-# INLINE runOp #-}

-- | Dynamic system.
data DynamicSystemT m a where
  -- | Pure value.
  Pure :: a -> DynamicSystemT m a
  -- | Functor map.
  Map :: (b -> a) -> DynamicSystemT m b -> DynamicSystemT m a
  -- | Applicative apply.
  Ap :: DynamicSystemT m (b -> a) -> DynamicSystemT m b -> DynamicSystemT m a
  -- | Query operation.
  Op :: Set ComponentID -> Op m a -> DynamicSystemT m a

instance Functor (DynamicSystemT m) where
  fmap f (Pure a) = Pure (f a)
  fmap f s = Map f s
  {-# INLINE fmap #-}

instance Applicative (DynamicSystemT m) where
  pure = Pure
  {-# INLINE pure #-}

  Pure f <*> s = fmap f s
  f <*> Pure a = fmap ($ a) f
  f <*> s = Ap f s
  {-# INLINE (<*>) #-}

-- | Run a dynamic system on entities, returning results, updated entities, and hooks to run.
runDynamicSystemT :: (Monad m) => DynamicSystemT m a -> Entities m -> m (a, Entities m, Access m ())
runDynamicSystemT (Pure a) es = return (a, es, return ())
runDynamicSystemT (Map f s) es = do
  (b, es', hook) <- runDynamicSystemT s es
  return (f b, es', hook)
runDynamicSystemT (Ap sf sa) es = do
  (f, es', hook1) <- runDynamicSystemT sf es
  (a, es'', hook2) <- runDynamicSystemT sa es'
  return (f a, es'', hook1 >> hook2)
runDynamicSystemT (Op cIds op) es = runOp cIds op es
{-# INLINE runDynamicSystemT #-}

all :: Set ComponentID -> DynamicQuery m a -> DynamicSystemT m (Vector a)
all cIds q = Op cIds (QAll q)
{-# INLINE all #-}

filter :: Set ComponentID -> DynamicQuery m a -> (Node m -> Bool) -> DynamicSystemT m (Vector a)
filter cIds q flt = Op cIds (QFilter q flt)
{-# INLINE filter #-}

map :: Set ComponentID -> DynamicQuery m a -> DynamicSystemT m (Vector a)
map cIds q = Op cIds (QMap q)
{-# INLINE map #-}

mapSingleMaybe :: Set ComponentID -> DynamicQuery m a -> DynamicSystemT m (Maybe a)
mapSingleMaybe cIds q = Op cIds (QMapSingleMaybe q)
{-# INLINE mapSingleMaybe #-}

filterMap :: Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> DynamicSystemT m (Vector a)
filterMap cIds flt q = Op cIds (QFilterMap flt q)
{-# INLINE filterMap #-}

-- | Alias for 'filterMap'.
filterMapM :: Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> DynamicSystemT m (Vector a)
filterMapM = filterMap
{-# INLINE filterMapM #-}
