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
import Aztecs.ECS.System.Dynamic (DynamicSystemT (..), runDynamicSystemT)
import qualified Aztecs.ECS.System.Dynamic as DS
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Components (Components)
import Control.Monad.Identity
import qualified Data.Foldable as F
import Data.Set (Set)
import Data.Vector (Vector)
import Prelude hiding (all, filter, map, mapM)

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

-- | Match all entities.
all :: QueryT Identity a -> SystemT Identity (Vector a)
all q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.all (Q.reads rws <> Q.writes rws) dynQ)

-- | Match all entities (monadic).
allM :: (Monad m) => QueryT m a -> SystemT m (Vector a)
allM q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.allM (Q.reads rws <> Q.writes rws) dynQ)

-- | Match all entities with a filter.
filter :: QueryT Identity a -> QueryFilter -> SystemT Identity (Vector a)
filter q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', DS.filter (Q.reads rws <> Q.writes rws) dynQ flt)

-- | Match all entities with a filter (monadic).
filterM :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m (Vector a)
filterM q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', DS.filterM (Q.reads rws <> Q.writes rws) dynQ flt)

-- | Map all matching entities.
map :: QueryT Identity a -> SystemT Identity (Vector a)
map q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.map (Q.reads rws <> Q.writes rws) dynQ)

-- | Map all matching entities (monadic).
mapM :: (Monad m) => QueryT m a -> SystemT m (Vector a)
mapM q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.mapM (Q.reads rws <> Q.writes rws) dynQ)

-- | Map a single matching entity, or @Nothing@.
mapSingleMaybe :: QueryT Identity a -> SystemT Identity (Maybe a)
mapSingleMaybe q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.mapSingleMaybe (Q.reads rws <> Q.writes rws) dynQ)

-- | Map a single matching entity, or @Nothing@ (monadic).
mapSingleMaybeM :: (Monad m) => QueryT m a -> SystemT m (Maybe a)
mapSingleMaybeM q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.mapSingleMaybeM (Q.reads rws <> Q.writes rws) dynQ)

-- | Filter and map all matching entities.
filterMap :: QueryT Identity a -> QueryFilter -> SystemT Identity (Vector a)
filterMap q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', DS.filterMap (Q.reads rws <> Q.writes rws) flt dynQ)

-- | Filter and map all matching entities (monadic).
filterMapM :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m (Vector a)
filterMapM q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', DS.filterMapM (Q.reads rws <> Q.writes rws) flt dynQ)

-- | Match all entities with a dynamic query.
allDyn :: Set ComponentID -> DynamicQuery a -> SystemT Identity (Vector a)
allDyn cIds q = SystemT (,DS.all cIds q)

-- | Match all entities with a dynamic query.
allDynM :: Set ComponentID -> DynamicQueryT m a -> SystemT m (Vector a)
allDynM cIds q = SystemT (,DS.allM cIds q)

-- | Match all entities with a dynamic query and filter.
filterDyn :: Set ComponentID -> DynamicQuery a -> (Node -> Bool) -> SystemT Identity (Vector a)
filterDyn cIds q f = SystemT (,DS.filter cIds q f)

-- | Match all entities with a dynamic query and filter.
filterDynM :: Set ComponentID -> DynamicQueryT m a -> (Node -> Bool) -> SystemT m (Vector a)
filterDynM cIds q f = SystemT (,DS.filterM cIds q f)

-- | Map all entities with a dynamic query.
mapDyn :: Set ComponentID -> DynamicQuery a -> SystemT Identity (Vector a)
mapDyn cIds q = SystemT (,DS.map cIds q)

-- | Map all entities with a dynamic query.
mapDynM :: Set ComponentID -> DynamicQueryT m a -> SystemT m (Vector a)
mapDynM cIds q = SystemT (,DS.mapM cIds q)

-- | Map a single entity with a dynamic query.
mapSingleMaybeDyn :: Set ComponentID -> DynamicQuery a -> SystemT Identity (Maybe a)
mapSingleMaybeDyn cIds q = SystemT (,DS.mapSingleMaybe cIds q)

-- | Map a single entity with a dynamic query.
mapSingleMaybeDynM :: Set ComponentID -> DynamicQueryT m a -> SystemT m (Maybe a)
mapSingleMaybeDynM cIds q = SystemT (,DS.mapSingleMaybeM cIds q)

-- | Filter and map all entities with a dynamic query.
filterMapDyn :: Set ComponentID -> (Node -> Bool) -> DynamicQuery a -> SystemT Identity (Vector a)
filterMapDyn cIds f q = SystemT (,DS.filterMap cIds f q)

-- | Filter and map all entities with a dynamic query.
filterMapDynM :: Set ComponentID -> (Node -> Bool) -> DynamicQueryT m a -> SystemT m (Vector a)
filterMapDynM cIds f q = SystemT (,DS.filterMapM cIds f q)
