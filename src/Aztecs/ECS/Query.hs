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
    QueryReaderF (..),
    QueryF (..),
    DynamicQueryReaderF (..),
    DynamicQueryF (..),

    -- ** Running
    all,
    all',
    single,
    single',
    singleMaybe,
    singleMaybe',
    map,
    mapSingle,
    mapSingleMaybe,

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
import Aztecs.ECS.Query.Reader.Class
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Aztecs.ECS.World.Entities as E
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import GHC.Stack
import Prelude hiding (all, id, map, reads, (.))

-- | @since 0.10
type Query = QueryT Identity

-- | Query for matching entities.
--
-- @since 0.10
newtype QueryT m a = Query
  { -- | Run a query, producing a `DynamicQueryT`.
    --
    -- @since 0.10
    runQuery :: Components -> (ReadsWrites, Components, DynamicQueryT m a)
  }
  deriving (Functor)

-- | @since 0.10
instance (Monad m) => Applicative (QueryT m) where
  pure a = Query (mempty,,pure a)
  {-# INLINE pure #-}

  (Query f) <*> (Query g) = Query $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)
  {-# INLINE (<*>) #-}

-- | @since 0.10
instance (Monad m) => QueryReaderF (QueryT m) where
  fetch :: forall a. (Component a) => QueryT m a
  fetch = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (ReadsWrites {reads = Set.singleton cId, writes = Set.empty}, cs', fetchDyn cId)
  {-# INLINE fetch #-}

  fetchMaybe :: forall a. (Component a) => QueryT m (Maybe a)
  fetchMaybe = Query $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (ReadsWrites {reads = Set.singleton cId, writes = Set.empty}, cs', fetchMaybeDyn cId)
  {-# INLINE fetchMaybe #-}

-- | @since 0.10
instance (Monad m) => DynamicQueryReaderF (QueryT m) where
  entity = Query (mempty,,entity)
  {-# INLINE entity #-}

  fetchDyn cId = Query (ReadsWrites {reads = Set.singleton cId, writes = Set.empty},,fetchDyn cId)
  {-# INLINE fetchDyn #-}

  fetchMaybeDyn cId = Query (ReadsWrites {reads = Set.singleton cId, writes = Set.empty},,fetchMaybeDyn cId)
  {-# INLINE fetchMaybeDyn #-}

-- | @since 0.10
instance (Monad m) => DynamicQueryF m (QueryT m) where
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

-- | @since 0.9
instance (Monad m) => QueryF m (QueryT m) where
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
--
-- @since 0.9
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

-- | @since 0.9
instance Semigroup ReadsWrites where
  ReadsWrites r1 w1 <> ReadsWrites r2 w2 = ReadsWrites (r1 <> r2) (w1 <> w2)

-- | @since 0.9
instance Monoid ReadsWrites where
  mempty = ReadsWrites mempty mempty

-- | `True` if the reads and writes of two `Query`s overlap.
--
-- @since 0.9
disjoint :: ReadsWrites -> ReadsWrites -> Bool
disjoint a b =
  Set.disjoint (reads a) (writes b)
    || Set.disjoint (reads b) (writes a)
    || Set.disjoint (writes b) (writes a)

-- | Match all entities.
--
-- @since 0.10
all :: (Monad m) => QueryT m a -> Entities -> m (Vector a, Entities)
all q es = do
  (as, cs) <- all' q es
  return (as, es {E.components = cs})
{-# INLINE all #-}

-- | Match all entities.
--
-- @since 0.10
all' :: (Monad m) => QueryT m a -> Entities -> m (Vector a, Components)
all' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  as <- allDyn cIds dynQ es
  return (as, cs')
{-# INLINE all' #-}

-- | Match a single entity.
--
-- @since 0.10
single :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
single q es = do
  (a, cs) <- single' q es
  return (a, es {E.components = cs})
{-# INLINE single #-}

-- | Match a single entity.
--
-- @since 0.10
single' :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Components)
single' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  a <- singleDyn cIds dynQ es
  return (a, cs')
{-# INLINE single' #-}

-- | Match a single entity.
--
-- @since 0.10
singleMaybe :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
singleMaybe q es = do
  (a, cs) <- singleMaybe' q es
  return (a, es {E.components = cs})
{-# INLINE singleMaybe #-}

-- | Match a single entity.
--
-- @since 0.10
singleMaybe' :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Components)
singleMaybe' q es = do
  let !(rws, cs', dynQ) = runQuery q (E.components es)
      !cIds = reads rws <> writes rws
  a <- singleMaybeDyn cIds dynQ es
  return (a, cs')
{-# INLINE singleMaybe' #-}

-- | Map all matched entities.
--
-- @since 0.9
map :: (Monad m) => QueryT m o -> Entities -> m (Vector o, Entities)
map q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapDyn cIds dynQ es
  return (as, es' {components = cs'})
{-# INLINE map #-}

-- | Map a single matched entity.
--
-- @since 0.9
mapSingle :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
mapSingle q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapSingleDyn cIds dynQ es
  return (as, es' {components = cs'})
{-# INLINE mapSingle #-}

-- | Map a single matched entity, or `Nothing`.
--
-- @since 0.9
mapSingleMaybe :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
mapSingleMaybe q es = do
  let !(rws, cs', dynQ) = runQuery q $ components es
      !cIds = reads rws <> writes rws
  (as, es') <- mapSingleMaybeDyn cIds dynQ es
  return (as, es' {components = cs'})
{-# INLINE mapSingleMaybe #-}

-- | Filter for a `Query`.
--
-- @since 0.9
newtype QueryFilter = QueryFilter
  { -- | Run a query filter.
    runQueryFilter :: Components -> (DynamicQueryFilter, Components)
  }

-- | @since 0.9
instance Semigroup QueryFilter where
  a <> b =
    QueryFilter
      ( \cs ->
          let !(withA', cs') = runQueryFilter a cs
              !(withB', cs'') = runQueryFilter b cs'
           in (withA' <> withB', cs'')
      )

-- | @since 0.9
instance Monoid QueryFilter where
  mempty = QueryFilter (mempty,)

-- | Filter for entities containing this component.
--
-- @since 0.9
with :: forall a. (Component a) => QueryFilter
with = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a cs in (mempty {filterWith = Set.singleton cId}, cs')

-- | Filter out entities containing this component.
--
-- @since 0.9
without :: forall a. (Component a) => QueryFilter
without = QueryFilter $ \cs ->
  let !(cId, cs') = CS.insert @a cs in (mempty {filterWithout = Set.singleton cId}, cs')
