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
  ( -- * Systems
    System (..),

    -- * Dynamic systems
    DynamicSystem (..),
    runDynamicSystem,

    -- ** Queries
    readQuery,
    readQueryFiltered,
    readQuerySingle,
    readQuerySingleMaybe,
    runQuery,
    runQueryFiltered,
    runQuerySingle,
    runQuerySingleMaybe,

    -- ** Dynamic queries
    readQueryDyn,
    readQueryFilteredDyn,
    readQuerySingleMaybeDyn,
    runQueryDyn,
    runQueryFilteredDyn,
    runQuerySingleMaybeDyn,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query (Query (..), QueryFilter (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQuery, DynamicQueryFilter (..))
import Aztecs.ECS.System.Dynamic (DynamicSystem (..), runDynamicSystem)
import qualified Aztecs.ECS.System.Dynamic as DS
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Components (Components)
import qualified Data.Foldable as F
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.Stack
import Prelude hiding (all, filter, map, mapM)

-- | System for querying entities.
newtype System m a = System {runSystem :: Components -> (Components, DynamicSystem m a)}

instance Functor (System m) where
  fmap f (System g) = System $ \cs ->
    let !(cs', dynS) = g cs in (cs', fmap f dynS)
  {-# INLINE fmap #-}

instance Applicative (System m) where
  pure a = System (,Pure a)
  {-# INLINE pure #-}

  (System f) <*> (System g) = System $ \cs ->
    let !(cs', dynF) = f cs
        !(cs'', dynG) = g cs'
     in (cs'', dynF <*> dynG)
  {-# INLINE (<*>) #-}

runner :: (Set ComponentID -> DynamicQuery m a -> DynamicSystem m b) -> Query m a -> System m b
runner f q = System $ \cs ->
  let (rws, cs', dynQ) = runQuery' q cs
   in (cs', f (Q.reads rws <> Q.writes rws) dynQ)

-- | Match all entities.
readQuery :: (Monad m) => Query m a -> System m (Vector a)
readQuery = runner DS.readQuery

readQuerySingle :: (HasCallStack, Monad m) => Query m a -> System m a
readQuerySingle = runner DS.readQuerySingle

readQuerySingleMaybe :: (Monad m) => Query m a -> System m (Maybe a)
readQuerySingleMaybe = runner DS.readQuerySingleMaybe

-- | Match all entities with a filter.
readQueryFiltered :: (Monad m) => Query m a -> QueryFilter -> System m (Vector a)
readQueryFiltered q f = System $ \cs ->
  let (rws, cs', dynQ) = runQuery' q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', DS.readQueryFiltered (Q.reads rws <> Q.writes rws) flt dynQ)

-- | Map all matching entities.
runQuery :: (Monad m) => Query m a -> System m (Vector a)
runQuery = runner DS.runQuery

runQuerySingle :: (HasCallStack, Monad m) => Query m a -> System m a
runQuerySingle = runner DS.runQuerySingle

-- | Map a single matching entity, or @Nothing@.
runQuerySingleMaybe :: (Monad m) => Query m a -> System m (Maybe a)
runQuerySingleMaybe = runner DS.runQuerySingleMaybe

-- | Filter and map all matching entities.
runQueryFiltered :: (Monad m) => Query m a -> QueryFilter -> System m (Vector a)
runQueryFiltered q f = System $ \cs ->
  let (rws, cs', dynQ) = runQuery' q cs
      (dynF, cs'') = runQueryFilter f cs'
      flt n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
   in (cs'', DS.runQueryFiltered (Q.reads rws <> Q.writes rws) dynQ flt)

-- | Match all entities with a dynamic query.
readQueryDyn :: Set ComponentID -> DynamicQuery m a -> System m (Vector a)
readQueryDyn cIds q = System (,DS.readQuery cIds q)

readQuerySingleMaybeDyn :: Set ComponentID -> DynamicQuery m a -> System m (Maybe a)
readQuerySingleMaybeDyn cIds q = System (,DS.readQuerySingleMaybe cIds q)

-- | Match all entities with a dynamic query and filter.
readQueryFilteredDyn :: Set ComponentID -> DynamicQuery m a -> (Node m -> Bool) -> System m (Vector a)
readQueryFilteredDyn cIds q f = System (,DS.readQueryFiltered cIds f q)

-- | Map all entities with a dynamic query.
runQueryDyn :: Set ComponentID -> DynamicQuery m a -> System m (Vector a)
runQueryDyn cIds q = System (,DS.runQuery cIds q)

-- | Map a single entity with a dynamic query.
runQuerySingleMaybeDyn :: Set ComponentID -> DynamicQuery m a -> System m (Maybe a)
runQuerySingleMaybeDyn cIds q = System (,DS.runQuerySingleMaybe cIds q)

-- | Filter and map all entities with a dynamic query.
runQueryFilteredDyn :: Set ComponentID -> (Node m -> Bool) -> DynamicQuery m a -> System m (Vector a)
runQueryFilteredDyn cIds f q = System (,DS.runQueryFiltered cIds q f)
