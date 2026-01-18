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
    allM,
    filter,
    filterM,
    map,
    mapM,
    mapSingleMaybe,
    mapSingleMaybeM,
    filterMap,
    filterMapM,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic (DynamicQuery, DynamicQueryT)
import qualified Aztecs.ECS.Query.Dynamic as DQ
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Entities (Entities)
import Control.Monad.Identity
import Data.Set (Set)
import Data.Vector (Vector)
import Prelude hiding (all, filter, map, mapM)

-- | Query operation.
data Op m a where
  -- | All entities matching a dynamic query.
  QAll :: DynamicQuery a -> Op Identity (Vector a)
  -- | All entities matching a dynamic query.
  QAllM :: DynamicQueryT m a -> Op m (Vector a)
  -- | Filtered entities matching a dynamic query.
  QFilter :: DynamicQuery a -> (Node Identity -> Bool) -> Op Identity (Vector a)
  -- | Filtered entities matching a dynamic query.
  QFilterM :: DynamicQueryT m a -> (Node m -> Bool) -> Op m (Vector a)
  -- | Map entities matching a dynamic query.
  QMap :: DynamicQuery a -> Op Identity (Vector a)
  -- | Map entities matching a dynamic query.
  QMapM :: DynamicQueryT m a -> Op m (Vector a)
  -- | Map single entity matching a dynamic query.
  QMapSingleMaybe :: DynamicQuery a -> Op Identity (Maybe a)
  -- | Map single entity matching a dynamic query.
  QMapSingleMaybeM :: DynamicQueryT m a -> Op m (Maybe a)
  -- | Filter and map entities matching a dynamic query.
  QFilterMap :: (Node Identity -> Bool) -> DynamicQuery a -> Op Identity (Vector a)
  -- | Filter and map entities matching a dynamic query.
  QFilterMapM :: (Node m -> Bool) -> DynamicQueryT m a -> Op m (Vector a)

-- | Run a query operation on entities.
runOp :: (Monad m) => Set ComponentID -> Op m a -> Entities m -> m (a, Entities m)
runOp cIds (QAll q) es = return (DQ.readQueryDyn cIds q es, es)
runOp cIds (QAllM q) es = do
  as <- DQ.readQueryDynM cIds q es
  return (as, es)
runOp cIds (QFilter q flt) es = return (DQ.readQueryFilteredDyn cIds q flt es, es)
runOp cIds (QFilterM q flt) es = do
  as <- DQ.readQueryFilteredDynM cIds flt q es
  return (as, es)
runOp cIds (QMap q) es = return (DQ.queryDyn cIds q es)
runOp cIds (QMapM q) es = DQ.queryDynM cIds q es
runOp cIds (QMapSingleMaybe q) es = return (DQ.querySingleMaybeDyn cIds q es)
runOp cIds (QMapSingleMaybeM q) es = DQ.querySingleMaybeDynM cIds q es
runOp cIds (QFilterMap flt q) es = return (DQ.queryFilteredDyn cIds flt q es)
runOp cIds (QFilterMapM flt q) es = DQ.queryFilteredDynM cIds flt q es
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

-- | Run a dynamic system on entities, returning results and updated entities.
runDynamicSystemT :: (Monad m) => DynamicSystemT m a -> Entities m -> m (a, Entities m)
runDynamicSystemT (Pure a) es = return (a, es)
runDynamicSystemT (Map f s) es = do
  (b, es') <- runDynamicSystemT s es
  return (f b, es')
runDynamicSystemT (Ap sf sa) es = do
  (f, es') <- runDynamicSystemT sf es
  (a, es'') <- runDynamicSystemT sa es'
  return (f a, es'')
runDynamicSystemT (Op cIds op) es = runOp cIds op es
{-# INLINE runDynamicSystemT #-}

all :: Set ComponentID -> DynamicQuery a -> DynamicSystemT Identity (Vector a)
all cIds q = Op cIds (QAll q)
{-# INLINE all #-}

allM :: Set ComponentID -> DynamicQueryT m a -> DynamicSystemT m (Vector a)
allM cIds q = Op cIds (QAllM q)
{-# INLINE allM #-}

filter :: Set ComponentID -> DynamicQuery a -> (Node Identity -> Bool) -> DynamicSystemT Identity (Vector a)
filter cIds q flt = Op cIds (QFilter q flt)
{-# INLINE filter #-}

filterM :: Set ComponentID -> DynamicQueryT m a -> (Node m -> Bool) -> DynamicSystemT m (Vector a)
filterM cIds q flt = Op cIds (QFilterM q flt)
{-# INLINE filterM #-}

map :: Set ComponentID -> DynamicQuery a -> DynamicSystemT Identity (Vector a)
map cIds q = Op cIds (QMap q)
{-# INLINE map #-}

mapM :: Set ComponentID -> DynamicQueryT m a -> DynamicSystemT m (Vector a)
mapM cIds q = Op cIds (QMapM q)
{-# INLINE mapM #-}

mapSingleMaybe :: Set ComponentID -> DynamicQuery a -> DynamicSystemT Identity (Maybe a)
mapSingleMaybe cIds q = Op cIds (QMapSingleMaybe q)
{-# INLINE mapSingleMaybe #-}

mapSingleMaybeM :: Set ComponentID -> DynamicQueryT m a -> DynamicSystemT m (Maybe a)
mapSingleMaybeM cIds q = Op cIds (QMapSingleMaybeM q)
{-# INLINE mapSingleMaybeM #-}

filterMap :: Set ComponentID -> (Node Identity -> Bool) -> DynamicQuery a -> DynamicSystemT Identity (Vector a)
filterMap cIds flt q = Op cIds (QFilterMap flt q)
{-# INLINE filterMap #-}

filterMapM :: Set ComponentID -> (Node m -> Bool) -> DynamicQueryT m a -> DynamicSystemT m (Vector a)
filterMapM cIds flt q = Op cIds (QFilterMapM flt q)
{-# INLINE filterMapM #-}
