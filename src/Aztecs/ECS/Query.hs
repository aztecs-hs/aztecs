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
-- >   Velocity v <- Q.query -< ()
-- >   Position p <- Q.query -< ()
-- >   Q.set -< Position $ p + v
--
-- === Arrow combinators:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = Q.query &&& Q.query >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
--
-- === Applicative combinators:
-- > move :: (ArrowQuery arr) => arr () Position
-- > move = (,) <$> Q.query <*> Q.query >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
module Aztecs.ECS.Query
  ( -- * Queries
    Query (..),
    DynamicQueryF (..),

    -- ** Operations
    query,
    queryMaybe,
    queryMap,
    queryMap_,
    queryMapM,
    queryMapWith,
    queryMapWith_,
    queryMapWithM,
    queryMapWithAccum,
    queryMapWithAccum_,
    queryMapWithAccumM,

    -- ** Running
    readQuery,
    readQuery',
    readQuerySingle,
    readQuerySingle',
    readQuerySingleMaybe,
    readQuerySingleMaybe',
    runQuery,
    runQuerySingle,
    runQuerySingleMaybe,

    -- * Filters
    QueryFilter (..),
    with,
    without,

    -- * Reads and writes
    ReadsWrites (..),
    disjoint,
  )
where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Aztecs.ECS.World.Entities as E
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import GHC.Stack
import Prelude hiding (all, filter, id, map, mapM, reads)

-- | Query for matching entities.
newtype Query m a = Query
  { -- | Run a query, producing a `DynamicQuery`.
    --
    -- @since 0.10
    runQuery' :: Components -> (ReadsWrites, Components, DynamicQuery m a)
  }
  deriving (Functor)

instance (Monad m) => Applicative (Query m) where
  pure a = Query (mempty,,pure a)
  {-# INLINE pure #-}

  (Query f) <*> (Query g) = Query $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)
  {-# INLINE (<*>) #-}

instance (Monad m) => DynamicQueryF m (Query m) where
  entity = Query (mempty,,entity)
  {-# INLINE entity #-}

  queryDyn = dynQueryReader queryDyn
  {-# INLINE queryDyn #-}

  queryMaybeDyn = dynQueryReader queryMaybeDyn
  {-# INLINE queryMaybeDyn #-}

  queryMapDyn f = dynQueryWriter' $ queryMapDyn f
  {-# INLINE queryMapDyn #-}

  queryMapDyn_ f = dynQueryWriter' $ queryMapDyn_ f
  {-# INLINE queryMapDyn_ #-}

  queryMapDynM f = dynQueryWriter' $ queryMapDynM f
  {-# INLINE queryMapDynM #-}

  queryMapDynWith f = dynQueryWriter $ queryMapDynWith f
  {-# INLINE queryMapDynWith #-}

  queryMapDynWith_ f = dynQueryWriter $ queryMapDynWith_ f
  {-# INLINE queryMapDynWith_ #-}

  queryMapDynWithM f = dynQueryWriter $ queryMapDynWithM f
  {-# INLINE queryMapDynWithM #-}

  queryMapDynWithAccum f = dynQueryWriter $ queryMapDynWithAccum f
  {-# INLINE queryMapDynWithAccum #-}

  queryUntracked (Query q) = Query $ \cs ->
    let !(rws, cs', dynQ) = q cs
     in (rws, cs', queryUntracked dynQ)
  {-# INLINE queryUntracked #-}

  queryMapDynWithAccumM f = dynQueryWriter $ queryMapDynWithAccumM f
  {-# INLINE queryMapDynWithAccumM #-}

  queryFilter (Query q) p = Query $ \cs ->
    let !(rws, cs', dynQ) = q cs
     in (rws, cs', queryFilter dynQ p)
  {-# INLINE queryFilter #-}

query :: forall m a. (Monad m, Component m a) => Query m a
query = queryReader @m @a queryDyn
{-# INLINE query #-}

queryMaybe :: forall m a. (Monad m, Component m a) => Query m (Maybe a)
queryMaybe = queryReader @m @a queryMaybeDyn
{-# INLINE queryMaybe #-}

queryMap :: forall m a. (Monad m, Component m a) => (a -> a) -> Query m a
queryMap f = queryWriter' @m @a $ queryMapDyn f
{-# INLINE queryMap #-}

queryMap_ :: forall m a. (Monad m, Component m a) => (a -> a) -> Query m ()
queryMap_ f = queryWriter' @m @a $ queryMapDyn_ f
{-# INLINE queryMap_ #-}

queryMapM :: forall m a. (Monad m, Component m a) => (a -> m a) -> Query m a
queryMapM f = queryWriter' @m @a $ queryMapDynM f
{-# INLINE queryMapM #-}

queryMapWith :: forall m a b. (Monad m, Component m b) => (a -> b -> b) -> Query m a -> Query m b
queryMapWith f = queryWriter @m @b $ queryMapDynWith f
{-# INLINE queryMapWith #-}

queryMapWith_ :: forall m a b. (Monad m, Component m b) => (a -> b -> b) -> Query m a -> Query m ()
queryMapWith_ f = queryWriter @m @b $ queryMapDynWith_ f
{-# INLINE queryMapWith_ #-}

queryMapWithM :: forall m a b. (Monad m, Component m b) => (a -> b -> m b) -> Query m a -> Query m b
queryMapWithM f = queryWriter @m @b $ queryMapDynWithM f
{-# INLINE queryMapWithM #-}

queryMapWithAccum :: forall m a b c. (Monad m, Component m c) => (b -> c -> (a, c)) -> Query m b -> Query m (a, c)
queryMapWithAccum f = queryWriter @m @c $ queryMapDynWithAccum f
{-# INLINE queryMapWithAccum #-}

queryMapWithAccum_ :: forall m a b. (Monad m, Component m b) => (a -> b -> b) -> Query m a -> Query m ()
queryMapWithAccum_ f = queryWriter @m @b $ queryMapDynWith_ f
{-# INLINE queryMapWithAccum_ #-}

queryMapWithAccumM :: forall m a b c. (Monad m, Component m c) => (b -> c -> m (a, c)) -> Query m b -> Query m (a, c)
queryMapWithAccumM f = queryWriter @m @c $ queryMapDynWithAccumM f
{-# INLINE queryMapWithAccumM #-}

dynQueryReader :: (ComponentID -> DynamicQuery m a) -> ComponentID -> Query m a
dynQueryReader f cId = Query (ReadsWrites {reads = Set.singleton cId, writes = Set.empty},,f cId)
{-# INLINE dynQueryReader #-}

dynQueryWriter :: (ComponentID -> DynamicQuery m a -> DynamicQuery m b) -> ComponentID -> Query m a -> Query m b
dynQueryWriter f cId q = Query $ \cs ->
  let !(rws, cs', dynQ) = runQuery' q cs
   in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs', f cId dynQ)
{-# INLINE dynQueryWriter #-}

dynQueryWriter' :: (ComponentID -> DynamicQuery m a) -> ComponentID -> Query m a
dynQueryWriter' f cId = Query (ReadsWrites {reads = Set.empty, writes = Set.singleton cId},,f cId)
{-# INLINE dynQueryWriter' #-}

queryReader :: forall m a b. (Component m a) => (ComponentID -> DynamicQuery m b) -> Query m b
queryReader f = Query $ \cs ->
  let !(cId, cs') = CS.insert @a @m cs in (ReadsWrites {reads = Set.singleton cId, writes = Set.empty}, cs', f cId)
{-# INLINE queryReader #-}

queryWriter :: forall m a b c. (Component m a) => (ComponentID -> DynamicQuery m b -> DynamicQuery m c) -> Query m b -> Query m c
queryWriter f (Query g) = Query $ \cs ->
  let !(rws, cs', dynQ) = g cs
      !(cId, cs'') = CS.insert @a @m cs'
   in (rws <> ReadsWrites Set.empty (Set.singleton cId), cs'', f cId dynQ)
{-# INLINE queryWriter #-}

queryWriter' :: forall m a b. (Component m a) => (ComponentID -> DynamicQuery m b) -> Query m b
queryWriter' f = Query $ \cs ->
  let !(cId, cs') = CS.insert @a @m cs in (ReadsWrites {reads = Set.empty, writes = Set.singleton cId}, cs', f cId)
{-# INLINE queryWriter' #-}

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
readQuery :: (Monad m) => Query m a -> Entities m -> m (Vector a, Entities m)
readQuery q es = do
  (as, cs) <- readQuery' q es
  return (as, es {E.components = cs})
{-# INLINE readQuery #-}

-- | Match all entities.
readQuery' :: (Monad m) => Query m a -> Entities m -> m (Vector a, Components)
readQuery' q es = do
  let !(rws, cs', dynQ) = runQuery' q (E.components es)
      !cIds = reads rws <> writes rws
  as <- readQueryDyn cIds dynQ es
  return (as, cs')
{-# INLINE readQuery' #-}

-- | Match a single entity.
readQuerySingle :: (HasCallStack, Monad m) => Query m a -> Entities m -> m (a, Entities m)
readQuerySingle q es = do
  (a, cs) <- readQuerySingle' q es
  return (a, es {E.components = cs})
{-# INLINE readQuerySingle #-}

-- | Match a single entity.
readQuerySingle' :: (HasCallStack, Monad m) => Query m a -> Entities m -> m (a, Components)
readQuerySingle' q es = do
  let !(rws, cs', dynQ) = runQuery' q (E.components es)
      !cIds = reads rws <> writes rws
  a <- readQuerySingleDyn cIds dynQ es
  return (a, cs')
{-# INLINE readQuerySingle' #-}

-- | Match a single entity.
readQuerySingleMaybe :: (Monad m) => Query m a -> Entities m -> m (Maybe a, Entities m)
readQuerySingleMaybe q es = do
  (a, cs) <- readQuerySingleMaybe' q es
  return (a, es {E.components = cs})
{-# INLINE readQuerySingleMaybe #-}

-- | Match a single entity.
readQuerySingleMaybe' :: (Monad m) => Query m a -> Entities m -> m (Maybe a, Components)
readQuerySingleMaybe' q es = do
  let !(rws, cs', dynQ) = runQuery' q (E.components es)
      !cIds = reads rws <> writes rws
  a <- readQuerySingleMaybeDyn cIds dynQ es
  return (a, cs')
{-# INLINE readQuerySingleMaybe' #-}

-- | Map all matched entities.
runQuery :: (Monad m) => Query m o -> Entities m -> m (Vector o, Entities m, Access m ())
runQuery q es = do
  let !(rws, cs', dynQ) = runQuery' q $ components es
      !cIds = reads rws <> writes rws
  (as, es', hook) <- runQueryDyn cIds dynQ es
  return (as, es' {components = cs'}, hook)
{-# INLINE runQuery #-}

-- | Map a single matched entity.
runQuerySingle :: (HasCallStack, Monad m) => Query m a -> Entities m -> m (a, Entities m, Access m ())
runQuerySingle q es = do
  let !(rws, cs', dynQ) = runQuery' q $ components es
      !cIds = reads rws <> writes rws
  (as, es', hook) <- runQuerySingleDyn cIds dynQ es
  return (as, es' {components = cs'}, hook)
{-# INLINE runQuerySingle #-}

-- | Map a single matched entity, or `Nothing`.
runQuerySingleMaybe :: (Monad m) => Query m a -> Entities m -> m (Maybe a, Entities m, Access m ())
runQuerySingleMaybe q es = do
  let !(rws, cs', dynQ) = runQuery' q $ components es
      !cIds = reads rws <> writes rws
  (as, es', hook) <- runQuerySingleMaybeDyn cIds dynQ es
  return (as, es' {components = cs'}, hook)
{-# INLINE runQuerySingleMaybe #-}

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
