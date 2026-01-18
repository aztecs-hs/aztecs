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
module Aztecs.ECS.System
  ( -- * Dynamic systems
    DynamicSystemT (..),
    runDynamicSystemT,

    -- * Systems
    System,
    SystemT (..),

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

    -- ** Dynamic queries
    allDyn,
    allDynM,
    filterDyn,
    filterDynM,
    mapDyn,
    mapDynM,
    mapSingleMaybeDyn,
    mapSingleMaybeDynM,
    filterMapDyn,
    filterMapDynM,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query (QueryFilter (..), QueryT (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQuery, DynamicQueryFilter (..), DynamicQueryT)
import qualified Aztecs.ECS.Query.Dynamic as DQ
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Components (Components)
import Aztecs.ECS.World.Entities (Entities)
import Control.Monad.Identity
import qualified Data.Foldable as F
import Data.Set (Set)
import Data.Vector (Vector)
import Prelude hiding (all, filter, map, mapM)

-- | Query operation.
data Op m a where
  -- | All entities matching a dynamic query.
  AllOp :: DynamicQuery a -> Op m (Vector a)
  -- | All entities matching a dynamic query.
  AllOpM :: DynamicQueryT m a -> Op m (Vector a)
  -- | Filtered entities matching a dynamic query.
  FilterOp :: DynamicQuery a -> (Node -> Bool) -> Op m (Vector a)
  -- | Filtered entities matching a dynamic query.
  FilterOpM :: DynamicQueryT m a -> (Node -> Bool) -> Op m (Vector a)
  -- | Map entities matching a dynamic query.
  MapOp :: DynamicQuery a -> Op m (Vector a)
  -- | Map entities matching a dynamic query.
  MapOpM :: DynamicQueryT m a -> Op m (Vector a)
  -- | Map single entity matching a dynamic query.
  MapSingleMaybeOp :: DynamicQuery a -> Op m (Maybe a)
  -- | Map single entity matching a dynamic query.
  MapSingleMaybeOpM :: DynamicQueryT m a -> Op m (Maybe a)
  -- | Filter and map entities matching a dynamic query.
  FilterMapOp :: (Node -> Bool) -> DynamicQuery a -> Op m (Vector a)
  -- | Filter and map entities matching a dynamic query.
  FilterMapOpM :: (Node -> Bool) -> DynamicQueryT m a -> Op m (Vector a)

-- | Run a query operation on entities.
runOp :: (Monad m) => Set ComponentID -> Op m a -> Entities -> m (a, Entities)
runOp cIds (AllOp q) es = return (DQ.allDyn cIds q es, es)
runOp cIds (AllOpM q) es = do
  as <- DQ.allDynM cIds q es
  return (as, es)
runOp cIds (FilterOp q flt) es = return (DQ.filterDyn cIds q flt es, es)
runOp cIds (FilterOpM q flt) es = do
  as <- DQ.filterDynM cIds flt q es
  return (as, es)
runOp cIds (MapOp q) es = return (DQ.mapDyn cIds q es)
runOp cIds (MapOpM q) es = DQ.mapDynM cIds q es
runOp cIds (MapSingleMaybeOp q) es = return (DQ.mapSingleMaybeDyn cIds q es)
runOp cIds (MapSingleMaybeOpM q) es = DQ.mapSingleMaybeDynM cIds q es
runOp cIds (FilterMapOp flt q) es = return (DQ.filterMapDyn cIds flt q es)
runOp cIds (FilterMapOpM flt q) es = DQ.filterMapDynM cIds flt q es
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

type System = SystemT Identity

-- | System for querying entities.
newtype SystemT m a = SystemT {runSystemT :: Components -> (Components, DynamicSystemT m a)}

instance Functor (SystemT m) where
  fmap f (SystemT g) = SystemT $ \cs ->
    let !(cs', dynS) = g cs in (cs', fmap f dynS)
  {-# INLINE fmap #-}

instance Applicative (SystemT m) where
  pure a = SystemT (,Pure a)
  {-# INLINE pure #-}

  (SystemT f) <*> (SystemT g) = SystemT $ \cs ->
    let !(cs', dynF) = f cs
        !(cs'', dynG) = g cs'
     in (cs'', dynF <*> dynG)
  {-# INLINE (<*>) #-}

-- | Run a dynamic system on entities, returning results and updated entities.
runDynamicSystemT :: (Monad m) => DynamicSystemT m a -> Entities -> m (a, Entities)
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

-- | Match all entities.
all :: QueryT Identity a -> SystemT Identity (Vector a)
all q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', Op (Q.reads rws <> Q.writes rws) (AllOp dynQ))

-- | Match all entities (monadic).
allM :: (Monad m) => QueryT m a -> SystemT m (Vector a)
allM q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', Op (Q.reads rws <> Q.writes rws) (AllOpM dynQ))

-- | Match all entities with a filter.
filter :: QueryT Identity a -> QueryFilter -> SystemT Identity (Vector a)
filter q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', Op (Q.reads rws <> Q.writes rws) (FilterOp dynQ flt))

-- | Match all entities with a filter (monadic).
filterM :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m (Vector a)
filterM q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', Op (Q.reads rws <> Q.writes rws) (FilterOpM dynQ flt))

-- | Map all matching entities.
map :: QueryT Identity a -> SystemT Identity (Vector a)
map q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', Op (Q.reads rws <> Q.writes rws) (MapOp dynQ))

-- | Map all matching entities (monadic).
mapM :: (Monad m) => QueryT m a -> SystemT m (Vector a)
mapM q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', Op (Q.reads rws <> Q.writes rws) (MapOpM dynQ))

-- | Map a single matching entity, or @Nothing@.
mapSingleMaybe :: QueryT Identity a -> SystemT Identity (Maybe a)
mapSingleMaybe q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', Op (Q.reads rws <> Q.writes rws) (MapSingleMaybeOp dynQ))

-- | Map a single matching entity, or @Nothing@ (monadic).
mapSingleMaybeM :: (Monad m) => QueryT m a -> SystemT m (Maybe a)
mapSingleMaybeM q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', Op (Q.reads rws <> Q.writes rws) (MapSingleMaybeOpM dynQ))

-- | Filter and map all matching entities.
filterMap :: QueryT Identity a -> QueryFilter -> SystemT Identity (Vector a)
filterMap q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', Op (Q.reads rws <> Q.writes rws) (FilterMapOp flt dynQ))

-- | Filter and map all matching entities (monadic).
filterMapM :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m (Vector a)
filterMapM q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', Op (Q.reads rws <> Q.writes rws) (FilterMapOpM flt dynQ))

-- | Match all entities with a dynamic query.
allDyn :: Set ComponentID -> DynamicQuery a -> SystemT Identity (Vector a)
allDyn cIds q = SystemT (,Op cIds (AllOp q))

-- | Match all entities with a dynamic query.
allDynM :: Set ComponentID -> DynamicQueryT m a -> SystemT m (Vector a)
allDynM cIds q = SystemT (,Op cIds (AllOpM q))

-- | Match all entities with a dynamic query and filter.
filterDyn :: Set ComponentID -> DynamicQuery a -> (Node -> Bool) -> SystemT Identity (Vector a)
filterDyn cIds q f = SystemT (,Op cIds (FilterOp q f))

-- | Match all entities with a dynamic query and filter.
filterDynM :: Set ComponentID -> DynamicQueryT m a -> (Node -> Bool) -> SystemT m (Vector a)
filterDynM cIds q f = SystemT (,Op cIds (FilterOpM q f))

-- | Map all entities with a dynamic query.
mapDyn :: Set ComponentID -> DynamicQuery a -> SystemT Identity (Vector a)
mapDyn cIds q = SystemT (,Op cIds (MapOp q))

-- | Map all entities with a dynamic query.
mapDynM :: Set ComponentID -> DynamicQueryT m a -> SystemT m (Vector a)
mapDynM cIds q = SystemT (,Op cIds (MapOpM q))

-- | Map a single entity with a dynamic query.
mapSingleMaybeDyn :: Set ComponentID -> DynamicQuery a -> SystemT Identity (Maybe a)
mapSingleMaybeDyn cIds q = SystemT (,Op cIds (MapSingleMaybeOp q))

-- | Map a single entity with a dynamic query.
mapSingleMaybeDynM :: Set ComponentID -> DynamicQueryT m a -> SystemT m (Maybe a)
mapSingleMaybeDynM cIds q = SystemT (,Op cIds (MapSingleMaybeOpM q))

-- | Filter and map all entities with a dynamic query.
filterMapDyn :: Set ComponentID -> (Node -> Bool) -> DynamicQuery a -> SystemT Identity (Vector a)
filterMapDyn cIds f q = SystemT (,Op cIds (FilterMapOp f q))

-- | Filter and map all entities with a dynamic query.
filterMapDynM :: Set ComponentID -> (Node -> Bool) -> DynamicQueryT m a -> SystemT m (Vector a)
filterMapDynM cIds f q = SystemT (,Op cIds (FilterMapOpM f q))
