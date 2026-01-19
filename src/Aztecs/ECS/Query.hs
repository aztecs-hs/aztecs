{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.Query
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Query for matching entities.
--
-- === Do notation:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = proc () -> do
-- >   Velocity v <- Q.fetch -< ()
-- >   Position p <- Q.fetch -< ()
-- >   Q.set -< Position $ p + v
--
-- === Arrow combinators:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = Q.fetch &&& Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
--
-- === Applicative combinators:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = (,) <$> Q.fetch <*> Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
module Aztecs.ECS.Query
  ( -- * Queries
    Query,
    QueryT (..),
    DynamicQueryF (..),

    -- ** Operations
    fetch,
    fetchMaybe,
    fetchMap,
    fetchMap_,
    fetchMapM,

    -- ** Running
    readQuery,
    readQuery',
    readQueryM,
    readQueryM',
    readQuerySingle,
    readQuerySingle',
    readQuerySingleM,
    readQuerySingleM',
    readQuerySingleMaybe,
    readQuerySingleMaybe',
    readQuerySingleMaybeM,
    readQuerySingleMaybeM',
    query,
    queryM,
    querySingle,
    querySingleM,
    querySingleMaybe,
    querySingleMaybeM,

    -- * Filters
    QueryFilter (..),
    with,
    without,

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,
  )
where

import Aztecs.ECS.Access.Internal (AccessT)
import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Aztecs.ECS.World.Entities as E
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import GHC.Stack
import Prelude hiding (all, id, map, mapM, reads)

type Query = QueryT Identity

-- | Query for matching entities.
newtype QueryT m a = Query
  { -- | Run a query, producing a `DynamicQueryT`.
    --
    -- @since 0.10
    runQuery :: Components -> (ReadsWrites, Components, DynamicQueryT m a)
  }
  deriving (Functor)

instance (Monad m) => Applicative (QueryT m) where
  pure a = Query (mempty,,pure a)
  {-# INLINE pure #-}

  (Query f) <*> (Query g) = Query $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)
  {-# INLINE (<*>) #-}

instance (Monad m) => DynamicQueryF m (QueryT m) where
  entity = Query (mempty,,entity)
  {-# INLINE entity #-}

  fetchDyn = dynQueryReader fetchDyn
  {-# INLINE fetchDyn #-}

  fetchMaybeDyn = dynQueryReader fetchMaybeDyn
  {-# INLINE fetchMaybeDyn #-}

  adjustDyn f = dynQueryWriter $ adjustDyn f
  {-# INLINE adjustDyn #-}

  adjustDyn_ f = dynQueryWriter $ adjustDyn_ f
  {-# INLINE adjustDyn_ #-}

  adjustDynM f = dynQueryWriter $ adjustDynM f
  {-# INLINE adjustDynM #-}

  setDyn cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs', setDyn cId dynQ)
  {-# INLINE setDyn #-}

fetch :: forall m a. (Monad m, Component m a) => QueryT m a
fetch = queryReader @m @a fetchDyn
{-# INLINE fetch #-}

fetchMaybe :: forall m a. (Monad m, Component m a) => QueryT m (Maybe a)
fetchMaybe = queryReader @m @a fetchMaybeDyn
{-# INLINE fetchMaybe #-}

fetchMap :: forall m a b. (Monad m, Component m a) => (b -> a -> a) -> QueryT m b -> QueryT m a
fetchMap f = queryWriter @m @a $ adjustDyn f
{-# INLINE fetchMap #-}

fetchMap_ :: forall m a b. (Monad m, Component m a) => (b -> a -> a) -> QueryT m b -> QueryT m ()
fetchMap_ f = queryWriter @m @a $ adjustDyn_ f
{-# INLINE fetchMap_ #-}

fetchMapM :: forall m a b. (Monad m, Component m a) => (b -> a -> m a) -> QueryT m b -> QueryT m a
fetchMapM f = queryWriter @m @a $ adjustDynM f
{-# INLINE fetchMapM #-}

dynQueryReader :: (ComponentID -> DynamicQueryT m a) -> ComponentID -> QueryT m a
dynQueryReader f cId = Query (ReadsWrites {reads = Set.singleton cId, writes = Set.empty},,f cId)
{-# INLINE dynQueryReader #-}

dynQueryWriter :: (ComponentID -> DynamicQueryT m a -> DynamicQueryT m b) -> ComponentID -> QueryT m a -> QueryT m b
dynQueryWriter f cId q = Query $ \cs ->
  let !(rws, cs', dynQ) = runQuery q cs
   in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs', f cId dynQ)

queryReader :: forall m a b. (Component m a) => (ComponentID -> DynamicQueryT m b) -> QueryT m b
queryReader f = Query $ \cs ->
  let !(cId, cs') = CS.insert @a @m cs in (ReadsWrites {reads = Set.singleton cId, writes = Set.empty}, cs', f cId)
{-# INLINE queryReader #-}

queryWriter :: forall m a b c. (Component m a) => (ComponentID -> DynamicQueryT m b -> DynamicQueryT m c) -> QueryT m b -> QueryT m c
queryWriter f (Query g) = Query $ \cs ->
  let !(rws, cs', dynQ) = g cs
      !(cId, cs'') = CS.insert @a @m cs'
   in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs'', f cId dynQ)
{-# INLINE queryWriter #-}

-- | Reads and writes of a `Query`.
data ReadsWrites = ReadsWrites
  { -- | Component IDs being read.
    reads :: !(Set ComponentID),
    -- | Component IDs being written.
    writes :: !(Set ComponentID)
  }
  deriving (Show)

instance Semigroup ReadsWrites where
  ReadsWrites r1 w1 <> ReadsWrites r2 w2 = ReadsWrites (r1 <> r2) (w1 <> w2)

instance Monoid ReadsWrites where
  mempty = ReadsWrites mempty mempty

-- | `True` if the reads and writes of two `Query`s overlap.
disjoint :: ReadsWrites -> ReadsWrites -> Bool
disjoint a b =
  Set.disjoint (reads a) (writes b)
    || Set.disjoint (reads b) (writes a)
    || Set.disjoint (writes b) (writes a)

-- | Match all entities.
readQuery :: Query a -> Entities Identity -> (Vector a, Entities Identity)
readQuery q es = let (as, cs) = readQuery' q es in (as, es {E.components = cs})
{-# INLINE readQuery #-}

-- | Match all entities.
readQuery' :: Query a -> Entities Identity -> (Vector a, Components)
readQuery' es = runIdentity . readQueryM' es
{-# INLINE readQuery' #-}

-- | Match all entities.
readQueryM :: (Monad m) => QueryT m a -> Entities m -> m (Vector a, Entities m)
readQueryM q es = do
  (as, cs) <- readQueryM' q es
  return (as, es {E.components = cs})
{-# INLINE readQueryM #-}

-- | Match all entities.
readQueryM' :: (Monad m) => QueryT m a -> Entities m -> m (Vector a, Components)
readQueryM' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  as <- readQueryDynM cIds dynQ es
  return (as, cs')
{-# INLINE readQueryM' #-}

-- | Match a single entity.
readQuerySingle :: (HasCallStack) => Query a -> Entities Identity -> (a, Entities Identity)
readQuerySingle q es = let (a, cs) = readQuerySingle' q es in (a, es {E.components = cs})
{-# INLINE readQuerySingle #-}

-- | Match a single entity.
readQuerySingle' :: (HasCallStack) => Query a -> Entities Identity -> (a, Components)
readQuerySingle' q = runIdentity . readQuerySingleM' q
{-# INLINE readQuerySingle' #-}

-- | Match a single entity.
readQuerySingleM :: (HasCallStack, Monad m) => QueryT m a -> Entities m -> m (a, Entities m)
readQuerySingleM q es = do
  (a, cs) <- readQuerySingleM' q es
  return (a, es {E.components = cs})
{-# INLINE readQuerySingleM #-}

-- | Match a single entity.
readQuerySingleM' :: (HasCallStack, Monad m) => QueryT m a -> Entities m -> m (a, Components)
readQuerySingleM' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  a <- readQuerySingleDynM cIds dynQ es
  return (a, cs')
{-# INLINE readQuerySingleM' #-}

-- | Match a single entity.
readQuerySingleMaybe :: Query a -> Entities Identity -> (Maybe a, Entities Identity)
readQuerySingleMaybe q es = let (a, cs) = readQuerySingleMaybe' q es in (a, es {E.components = cs})
{-# INLINE readQuerySingleMaybe #-}

-- | Match a single entity.
readQuerySingleMaybe' :: Query a -> Entities Identity -> (Maybe a, Components)
readQuerySingleMaybe' q = runIdentity . readQuerySingleMaybeM' q
{-# INLINE readQuerySingleMaybe' #-}

-- | Match a single entity.
readQuerySingleMaybeM :: (Monad m) => QueryT m a -> Entities m -> m (Maybe a, Entities m)
readQuerySingleMaybeM q es = do
  (a, cs) <- readQuerySingleMaybeM' q es
  return (a, es {E.components = cs})
{-# INLINE readQuerySingleMaybeM #-}

-- | Match a single entity.
readQuerySingleMaybeM' :: (Monad m) => QueryT m a -> Entities m -> m (Maybe a, Components)
readQuerySingleMaybeM' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  a <- readQuerySingleMaybeDynM cIds dynQ es
  return (a, cs')
{-# INLINE readQuerySingleMaybeM' #-}

-- | Map all matched entities.
query :: Query o -> Entities Identity -> (Vector o, Entities Identity, AccessT Identity ())
query q = runIdentity . queryM q
{-# INLINE query #-}

-- | Map all matched entities.
queryM :: (Monad m) => QueryT m o -> Entities m -> m (Vector o, Entities m, AccessT m ())
queryM q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es', hook) <- queryDynM cIds dynQ es
  return (as, es' {components = cs'}, hook)
{-# INLINE queryM #-}

-- | Map a single matched entity.
querySingle :: (HasCallStack) => Query a -> Entities Identity -> (a, Entities Identity, AccessT Identity ())
querySingle q = runIdentity . querySingleM q
{-# INLINE querySingle #-}

-- | Map a single matched entity.
querySingleM :: (HasCallStack, Monad m) => QueryT m a -> Entities m -> m (a, Entities m, AccessT m ())
querySingleM q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es', hook) <- querySingleDynM cIds dynQ es
  return (as, es' {components = cs'}, hook)
{-# INLINE querySingleM #-}

-- | Map a single matched entity, or `Nothing`.
querySingleMaybe :: Query a -> Entities Identity -> (Maybe a, Entities Identity, AccessT Identity ())
querySingleMaybe q = runIdentity . querySingleMaybeM q
{-# INLINE querySingleMaybe #-}

-- | Map a single matched entity, or `Nothing`.
querySingleMaybeM :: (Monad m) => QueryT m a -> Entities m -> m (Maybe a, Entities m, AccessT m ())
querySingleMaybeM q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es', hook) <- querySingleMaybeDynM cIds dynQ es
  return (as, es' {components = cs'}, hook)
{-# INLINE querySingleMaybeM #-}

-- | Filter for a `Query`.
newtype QueryFilter = QueryFilter
  { -- | Run a query filter.
    runQueryFilter :: Components -> (DynamicQueryFilter, Components)
  }

instance Semigroup QueryFilter where
  a <> b =
    QueryFilter
      ( \cs ->
          let !(withA', cs') = runQueryFilter a cs
              !(withB', cs'') = runQueryFilter b cs'
           in (withA' <> withB', cs'')
      )

instance Monoid QueryFilter where
  mempty = QueryFilter (mempty,)

-- | Filter for entities containing this component.
with :: forall m a. (Component m a) => QueryFilter
with = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a @m cs in (mempty {filterWith = Set.singleton cId}, cs')

-- | Filter out entities containing this component.
without :: forall m a. (Component m a) => QueryFilter
without = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a @m cs in (mempty {filterWithout = Set.singleton cId}, cs')
