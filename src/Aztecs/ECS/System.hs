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
    readQuery,
    readQueryFiltered,
    query,
    querySingleMaybe,
    queryFiltered,

    -- ** Dynamic queries
    readQueryDyn,
    readQueryFilteredDyn,
    queryDyn,
    querySingleMaybeDyn,
    queryFilteredDyn,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query (Query (..), QueryFilter (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQuery, DynamicQueryFilter (..))
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
readQuery :: (Monad m) => Query m a -> SystemT m (Vector a)
readQuery q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.all (Q.reads rws <> Q.writes rws) dynQ)

-- | Match all entities with a filter.
readQueryFiltered :: (Monad m) => Query m a -> QueryFilter -> SystemT m (Vector a)
readQueryFiltered q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', DS.filter (Q.reads rws <> Q.writes rws) dynQ flt)

-- | Map all matching entities.
query :: (Monad m) => Query m a -> SystemT m (Vector a)
query q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.map (Q.reads rws <> Q.writes rws) dynQ)

-- | Map a single matching entity, or @Nothing@.
querySingleMaybe :: (Monad m) => Query m a -> SystemT m (Maybe a)
querySingleMaybe q = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
   in (cs', DS.mapSingleMaybe (Q.reads rws <> Q.writes rws) dynQ)

-- | Filter and map all matching entities.
queryFiltered :: (Monad m) => Query m a -> QueryFilter -> SystemT m (Vector a)
queryFiltered q f = SystemT $ \cs ->
  let (rws, cs', dynQ) = runQuery q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', DS.filterMapM (Q.reads rws <> Q.writes rws) flt dynQ)

-- | Match all entities with a dynamic query.
readQueryDyn :: Set ComponentID -> DynamicQuery m a -> SystemT m (Vector a)
readQueryDyn cIds q = SystemT (,DS.all cIds q)

-- | Match all entities with a dynamic query and filter.
readQueryFilteredDyn :: Set ComponentID -> DynamicQuery m a -> (Node m -> Bool) -> SystemT m (Vector a)
readQueryFilteredDyn cIds q f = SystemT (,DS.filter cIds q f)

-- | Map all entities with a dynamic query.
queryDyn :: Set ComponentID -> DynamicQuery m a -> SystemT m (Vector a)
queryDyn cIds q = SystemT (,DS.map cIds q)

-- | Map a single entity with a dynamic query.
querySingleMaybeDyn :: Set ComponentID -> DynamicQuery m a -> SystemT m (Maybe a)
querySingleMaybeDyn cIds q = SystemT (,DS.mapSingleMaybe cIds q)

-- | Filter and map all entities with a dynamic query.
queryFilteredDyn :: Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> SystemT m (Vector a)
queryFilteredDyn cIds f q = SystemT (,DS.filterMapM cIds f q)
