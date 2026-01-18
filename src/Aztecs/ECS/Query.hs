{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
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
    QueryF (..),
    DynamicQueryF (..),

    -- ** Running
    all,
    all',
    allM,
    allM',
    single,
    single',
    singleM,
    singleM',
    singleMaybe,
    singleMaybe',
    singleMaybeM,
    singleMaybeM',
    map,
    mapM,
    mapSingle,
    mapSingleM,
    mapSingleMaybe,
    mapSingleMaybeM,

    -- * Filters
    QueryFilter (..),
    with,
    without,

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Class
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

  fetchDyn cId = Query (ReadsWrites {reads = Set.singleton cId, writes = Set.empty},,fetchDyn cId)
  {-# INLINE fetchDyn #-}

  fetchMaybeDyn cId = Query (ReadsWrites {reads = Set.singleton cId, writes = Set.empty},,fetchMaybeDyn cId)
  {-# INLINE fetchMaybeDyn #-}

  adjustDyn f cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs', adjustDyn f cId dynQ)
  {-# INLINE adjustDyn #-}

  adjustDyn_ f cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs', adjustDyn_ f cId dynQ)
  {-# INLINE adjustDyn_ #-}

  adjustDynM f cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs', adjustDynM f cId dynQ)
  {-# INLINE adjustDynM #-}

  setDyn cId q = Query $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs', setDyn cId dynQ)
  {-# INLINE setDyn #-}

instance (Monad m) => QueryF m (QueryT m) where
  fetch :: forall a. (Component a) => QueryT m a
  fetch = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (ReadsWrites {reads = Set.singleton cId, writes = Set.empty}, cs', fetchDyn cId)
  {-# INLINE fetch #-}

  fetchMaybe :: forall a. (Component a) => QueryT m (Maybe a)
  fetchMaybe = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (ReadsWrites {reads = Set.singleton cId, writes = Set.empty}, cs', fetchMaybeDyn cId)
  {-# INLINE fetchMaybe #-}

  adjust :: forall a b. (Component a) => (b -> a -> a) -> QueryT m b -> QueryT m a
  adjust f q = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
        !(rws, cs'', dynQ) = runQuery q cs'
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs'', adjustDyn f cId dynQ)
  {-# INLINE adjust #-}

  adjust_ :: forall a b. (Component a) => (b -> a -> a) -> QueryT m b -> QueryT m ()
  adjust_ f q = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
        !(rws, cs'', dynQ) = runQuery q cs'
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs'', adjustDyn_ f cId dynQ)
  {-# INLINE adjust_ #-}

  adjustM :: forall a b. (Component a, Monad m) => (b -> a -> m a) -> QueryT m b -> QueryT m a
  adjustM f q = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
        !(rws, cs'', dynQ) = runQuery q cs'
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs'', adjustDynM f cId dynQ)
  {-# INLINE adjustM #-}

  set :: forall a. (Component a) => QueryT m a -> QueryT m a
  set q = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs
        !(rws, cs'', dynQ) = runQuery q cs'
     in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs'', setDyn cId dynQ)
  {-# INLINE set #-}

-- | Reads and writes of a `Query`.
data ReadsWrites = ReadsWrites
  { -- | Component IDs being read.
    --
    -- @since 0.9
    reads :: !(Set ComponentID),
    -- | Component IDs being written.
    --
    -- @since 0.9
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
all :: Query a -> Entities -> (Vector a, Entities)
all q es = let (as, cs) = all' q es in (as, es {E.components = cs})
{-# INLINE all #-}

-- | Match all entities.
all' :: Query a -> Entities -> (Vector a, Components)
all' es = runIdentity . allM' es
{-# INLINE all' #-}

-- | Match all entities.
allM :: (Monad m) => QueryT m a -> Entities -> m (Vector a, Entities)
allM q es = do
  (as, cs) <- allM' q es
  return (as, es {E.components = cs})
{-# INLINE allM #-}

-- | Match all entities.
allM' :: (Monad m) => QueryT m a -> Entities -> m (Vector a, Components)
allM' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  as <- allDynM cIds dynQ es
  return (as, cs')
{-# INLINE allM' #-}

-- | Match a single entity.
single :: (HasCallStack) => Query a -> Entities -> (a, Entities)
single q es = let (a, cs) = single' q es in (a, es {E.components = cs})
{-# INLINE single #-}

-- | Match a single entity.
single' :: (HasCallStack) => Query a -> Entities -> (a, Components)
single' q = runIdentity . singleM' q
{-# INLINE single' #-}

-- | Match a single entity.
singleM :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
singleM q es = do
  (a, cs) <- singleM' q es
  return (a, es {E.components = cs})
{-# INLINE singleM #-}

-- | Match a single entity.
singleM' :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Components)
singleM' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  a <- singleDynM cIds dynQ es
  return (a, cs')
{-# INLINE singleM' #-}

-- | Match a single entity.
singleMaybe :: Query a -> Entities -> (Maybe a, Entities)
singleMaybe q es = let (a, cs) = singleMaybe' q es in (a, es {E.components = cs})
{-# INLINE singleMaybe #-}

-- | Match a single entity.
singleMaybe' :: Query a -> Entities -> (Maybe a, Components)
singleMaybe' q = runIdentity . singleMaybeM' q
{-# INLINE singleMaybe' #-}

-- | Match a single entity.
singleMaybeM :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
singleMaybeM q es = do
  (a, cs) <- singleMaybeM' q es
  return (a, es {E.components = cs})
{-# INLINE singleMaybeM #-}

-- | Match a single entity.
singleMaybeM' :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Components)
singleMaybeM' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  a <- singleMaybeDynM cIds dynQ es
  return (a, cs')
{-# INLINE singleMaybeM' #-}

-- | Map all matched entities.
map :: Query o -> Entities -> (Vector o, Entities)
map q = runIdentity . mapM q
{-# INLINE map #-}

-- | Map all matched entities.
mapM :: (Monad m) => QueryT m o -> Entities -> m (Vector o, Entities)
mapM q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapDynM cIds dynQ es
  return (as, es' {components = cs'})
{-# INLINE mapM #-}

-- | Map a single matched entity.
mapSingle :: (HasCallStack) => Query a -> Entities -> (a, Entities)
mapSingle q = runIdentity . mapSingleM q
{-# INLINE mapSingle #-}

-- | Map a single matched entity.
mapSingleM :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
mapSingleM q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapSingleDynM cIds dynQ es
  return (as, es' {components = cs'})
{-# INLINE mapSingleM #-}

-- | Map a single matched entity, or `Nothing`.
mapSingleMaybe :: Query a -> Entities -> (Maybe a, Entities)
mapSingleMaybe q = runIdentity . mapSingleMaybeM q
{-# INLINE mapSingleMaybe #-}

-- | Map a single matched entity, or `Nothing`.
mapSingleMaybeM :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
mapSingleMaybeM q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapSingleMaybeDynM cIds dynQ es
  return (as, es' {components = cs'})
{-# INLINE mapSingleMaybeM #-}

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
with :: forall a. (Component a) => QueryFilter
with = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a cs in (mempty {filterWith = Set.singleton cId}, cs')

-- | Filter out entities containing this component.
without :: forall a. (Component a) => QueryFilter
without = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a cs in (mempty {filterWithout = Set.singleton cId}, cs')
