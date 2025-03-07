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

    -- ** Operations
    entity,
    fetch,
    fetchMap,
    zipFetchMap,
    zipFetchMapAccum,
    zipFetchMapM,
    zipFetchMapAccumM,

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
    queryEntities,
    readQueryEntities,

    -- ** Conversion
    fromDyn,

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
import Aztecs.ECS.Entity
import Aztecs.ECS.Query.Dynamic
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Monad.Identity
import qualified Data.Set as Set
import GHC.Stack
import Prelude hiding (all, map)

-- | @since 0.10
type Query = QueryT Identity

-- | Query for matching entities.
--
-- @since 0.10
newtype QueryT f a = Query
  { -- | Run a query, producing a `DynamicQueryT`.
    --
    -- @since 0.10
    runQuery :: Components -> (Components, DynamicQueryT f a)
  }
  deriving (Functor)

-- | @since 0.10
instance (Applicative f) => Applicative (QueryT f) where
  {-# INLINE pure #-}
  pure a = Query (,pure a)

  {-# INLINE (<*>) #-}
  (Query f) <*> (Query g) = Query $ \cs ->
    let !(cs', aQS) = g cs
        !(cs'', bQS) = f cs'
     in (cs'', bQS <*> aQS)

{-# INLINE entity #-}
entity :: QueryT f EntityID
entity = Query (,entityDyn)

{-# INLINE fetch #-}
fetch :: forall f a. (Component a) => QueryT f a
fetch = fromDynInternal @f @a $ fetchDyn

{-# INLINE fetchMap #-}
fetchMap :: forall m a. (Component a) => (a -> a) -> QueryT m a
fetchMap f = fromDynInternal @_ @a $ fetchMapDyn f

{-# INLINE zipFetchMap #-}
zipFetchMap :: forall m a b. (Component a) => (b -> a -> a) -> QueryT m b -> QueryT m a
zipFetchMap f = fromWriterInternal @a $ zipFetchMapDyn f

{-# INLINE zipFetchMapAccum #-}
zipFetchMapAccum ::
  forall m a b c. (Component a) => (b -> a -> (c, a)) -> QueryT m b -> QueryT m (c, a)
zipFetchMapAccum f = fromWriterInternal @a $ zipFetchMapAccumDyn f

{-# INLINE zipFetchMapM #-}
zipFetchMapM :: forall m a b. (Monad m, Component a) => (b -> a -> m a) -> QueryT m b -> QueryT m a
zipFetchMapM f = fromWriterInternal @a $ zipFetchMapDynM f

{-# INLINE zipFetchMapAccumM #-}
zipFetchMapAccumM ::
  forall m a b c. (Monad m, Component a) => (b -> a -> m (c, a)) -> QueryT m b -> QueryT m (c, a)
zipFetchMapAccumM f = fromWriterInternal @a $ zipFetchMapAccumDynM f

-- | Convert a `DynamicQueryT` to a `QueryT`.
--
-- @since 0.10
{-# INLINE fromDyn #-}
fromDyn :: DynamicQueryT f a -> QueryT f a
fromDyn q = Query (,q)

{-# INLINE fromDynInternal #-}
fromDynInternal ::
  forall f a.
  (Component a) =>
  (ComponentID -> DynamicQueryT f a) ->
  QueryT f a
fromDynInternal f = Query $ \cs ->
  let !(cId, cs') = CS.insert @a cs in (cs', f cId)

{-# INLINE fromWriterInternal #-}
fromWriterInternal ::
  forall c f a b.
  (Component c) =>
  (ComponentID -> DynamicQueryT f b -> DynamicQueryT f a) ->
  QueryT f b ->
  QueryT f a
fromWriterInternal f q = Query $ \cs ->
  let !(cId, cs') = CS.insert @c cs
      !(cs'', dynQ) = runQuery q cs'
   in (cs'', f cId dynQ)

-- | Match all entities.
--
-- @since 0.10
{-# INLINE all #-}
all :: (Monad m) => QueryT m a -> Entities -> m ([a], Entities)
all q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- allDyn dynQ es
  return (as, es {components = cs'})

-- | Match all entities.
--
-- @since 0.10
{-# INLINE all' #-}
all' :: (Monad m) => QueryT m a -> Entities -> m ([a], Components)
all' q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- allDyn dynQ es
  return (as, cs')

-- | Match a single entity.
--
-- @since 0.10
{-# INLINE single #-}
single :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
single q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- singleDyn dynQ es
  return (as, es {components = cs'})

-- | Match a single entity.
--
-- @since 0.10
{-# INLINE single' #-}
single' :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Components)
single' q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- singleDyn dynQ es
  return (as, cs')

-- | Match a single entity, or `Nothing`.
--
-- @since 0.10
{-# INLINE singleMaybe #-}
singleMaybe :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
singleMaybe q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- singleMaybeDyn dynQ es
  return (as, es {components = cs'})

-- | Match a single entity, or `Nothing`.
--
-- @since 0.10
{-# INLINE singleMaybe' #-}
singleMaybe' :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Components)
singleMaybe' q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- singleMaybeDyn dynQ es
  return (as, cs')

-- | Map all matched entities.
--
-- @since 0.10
{-# INLINE map #-}
map :: (Monad m) => QueryT m a -> Entities -> m ([a], Entities)
map q es = do
  let !(cs', dynQ) = runQuery q $ components es
  (as, es') <- mapDyn dynQ es
  return (as, es' {components = cs'})

-- | Map a single matched entity.
--
-- @since 0.10
{-# INLINE mapSingle #-}
mapSingle :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
mapSingle q es = do
  let !(cs', dynQ) = runQuery q $ components es
  (as, es') <- mapSingleDyn dynQ es
  return (as, es' {components = cs'})

-- | Map a single matched entity, or `Nothing`.
--
-- @since 0.10
{-# INLINE mapSingleMaybe #-}
mapSingleMaybe :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
mapSingleMaybe q es = do
  let !(cs', dynQ) = runQuery q $ components es
  (as, es') <- mapSingleMaybeDyn dynQ es
  return (as, es' {components = cs'})

queryEntities :: (Monad m) => [EntityID] -> QueryT m a -> Entities -> m ([a], Entities)
queryEntities eIds q es = do
  let !(cs', dynQ) = runQuery q $ components es
  (as, es') <- queryEntitiesDyn eIds dynQ es
  return (as, es' {components = cs'})

readQueryEntities :: (Monad m) => [EntityID] -> QueryT m a -> Entities -> m ([a], Entities)
readQueryEntities eIds q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- readQueryEntitiesDyn eIds dynQ es
  return (as, es {components = cs'})

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
