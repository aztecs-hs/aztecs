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
module Aztecs.ECS.Query
  ( -- * Queries
    Query,
    QueryT (..),

    -- ** Operations
    entity,
    fetch,
    fetchMap,
    fetchMapM,
    zipFetchMap,
    zipFetchMapAccum,
    zipFetchMapM,
    zipFetchMapAccumM,

    -- ** Filters
    with,
    without,

    -- ** Running
    query,
    readSingle,
    readSingle',
    readSingleMaybe,
    readSingleMaybe',
    querySingle,
    querySingleMaybe,
    queryEntities,
    readQueryEntities,

    -- ** Conversion
    fromDyn,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Query.Dynamic
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Monad.Identity
import GHC.Stack

-- | @since 0.11
type Query = QueryT Identity

-- | Query for matching entities.
--
-- @since 0.11
newtype QueryT f a = Query
  { -- | Run a query, producing a `DynamicQueryT`.
    --
    -- @since 0.11
    runQuery :: Components -> (Components, DynamicQueryT f a)
  }
  deriving (Functor)

-- | @since 0.11
instance (Applicative f) => Applicative (QueryT f) where
  {-# INLINE pure #-}
  pure a = Query (,pure a)

  {-# INLINE (<*>) #-}
  (Query f) <*> (Query g) = Query $ \cs ->
    let !(cs', aQS) = g cs
        !(cs'', bQS) = f cs'
     in (cs'', bQS <*> aQS)

-- | Fetch the current `EntityID`.
--
-- @since 0.11
{-# INLINE entity #-}
entity :: QueryT f EntityID
entity = Query (,entityDyn)

-- | Fetch a component.
--
-- @since 0.11
{-# INLINE fetch #-}
fetch :: forall f a. (Component a) => QueryT f a
fetch = fromDynInternal @f @a $ fetchDyn

-- | Fetch a component and map it, storing the result.
--
-- @since 0.11
{-# INLINE fetchMap #-}
fetchMap :: forall m a. (Component a) => (a -> a) -> QueryT m a
fetchMap f = fromDynInternal @_ @a $ fetchMapDyn f

-- | Fetch a component and map it with a monadic function, storing the result.
--
-- @since 0.11
{-# INLINE fetchMapM #-}
fetchMapM :: forall m a. (Monad m, Component a) => (a -> m a) -> QueryT m a
fetchMapM f = fromDynInternal @_ @a $ fetchMapDynM f

-- | Fetch a component and map it with some input, storing the result.
--
-- @since 0.11
{-# INLINE zipFetchMap #-}
zipFetchMap :: forall m a b. (Component a) => (b -> a -> a) -> QueryT m b -> QueryT m a
zipFetchMap f = fromWriterInternal @a $ zipFetchMapDyn f

-- | Fetch a component and map it with some input, storing the result and returning some output.
--
-- @since 0.11
{-# INLINE zipFetchMapAccum #-}
zipFetchMapAccum ::
  forall m a b c. (Component a) => (b -> a -> (c, a)) -> QueryT m b -> QueryT m (c, a)
zipFetchMapAccum f = fromWriterInternal @a $ zipFetchMapAccumDyn f

-- | Fetch a component and map it with some input and a monadic function, storing the result.
--
-- @since 0.11
{-# INLINE zipFetchMapM #-}
zipFetchMapM :: forall m a b. (Monad m, Component a) => (b -> a -> m a) -> QueryT m b -> QueryT m a
zipFetchMapM f = fromWriterInternal @a $ zipFetchMapDynM f

-- | Fetch a component and map it with some input and a monadic function,
-- storing the result and returning some output.
--
-- @since 0.11
{-# INLINE zipFetchMapAccumM #-}
zipFetchMapAccumM ::
  forall m a b c. (Monad m, Component a) => (b -> a -> m (c, a)) -> QueryT m b -> QueryT m (c, a)
zipFetchMapAccumM f = fromWriterInternal @a $ zipFetchMapAccumDynM f

-- | Filter for entities with a component.
--
-- @since 0.11
{-# INLINE with #-}
with :: forall f a. (Component a) => QueryT f ()
with = fromDynInternal @f @a $ withDyn

-- | Filter for entities without a component.
--
-- @since 0.11
{-# INLINE without #-}
without :: forall f a. (Component a) => QueryT f ()
without = fromDynInternal @f @a $ withDyn

-- | Convert a `DynamicQueryT` to a `QueryT`.
--
-- @since 0.11
{-# INLINE fromDyn #-}
fromDyn :: DynamicQueryT f a -> QueryT f a
fromDyn q = Query (,q)

{-# INLINE fromDynInternal #-}
fromDynInternal ::
  forall f a b.
  (Component a) =>
  (ComponentID -> DynamicQueryT f b) ->
  QueryT f b
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

-- | Match and update all entities.
--
-- @since 0.11
{-# INLINE query #-}
query :: (Monad m) => QueryT m a -> Entities -> m ([a], Entities)
query q es = do
  let !(cs', dynQ) = runQuery q $ components es
  (as, es') <- queryDyn dynQ es
  return (as, es' {components = cs'})

-- | Match and update a single matched entity.
--
-- @since 0.11
{-# INLINE querySingle #-}
querySingle :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
querySingle q es = do
  let !(cs', dynQ) = runQuery q $ components es
  (as, es') <- mapSingleDyn dynQ es
  return (as, es' {components = cs'})

-- | Match and update a single matched entity, or `Nothing`.
--
-- @since 0.11
{-# INLINE querySingleMaybe #-}
querySingleMaybe :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
querySingleMaybe q es = do
  let !(cs', dynQ) = runQuery q $ components es
  (as, es') <- mapSingleMaybeDyn dynQ es
  return (as, es' {components = cs'})

-- | Match and update the specified entities.
--
-- @since 0.11
queryEntities :: (Monad m) => [EntityID] -> QueryT m a -> Entities -> m ([a], Entities)
queryEntities eIds q es = do
  let !(cs', dynQ) = runQuery q $ components es
  (as, es') <- queryEntitiesDyn eIds dynQ es
  return (as, es' {components = cs'})

-- | Match a single entity.
--
-- @since 0.11
{-# INLINE readSingle #-}
readSingle :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Entities)
readSingle q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- singleDyn dynQ es
  return (as, es {components = cs'})

-- | Match a single entity.
--
-- @since 0.11
{-# INLINE readSingle' #-}
readSingle' :: (HasCallStack, Monad m) => QueryT m a -> Entities -> m (a, Components)
readSingle' q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- singleDyn dynQ es
  return (as, cs')

-- | Match a single entity, or `Nothing`.
--
-- @since 0.11
{-# INLINE readSingleMaybe #-}
readSingleMaybe :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Entities)
readSingleMaybe q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- singleMaybeDyn dynQ es
  return (as, es {components = cs'})

-- | Match a single entity, or `Nothing`.
--
-- @since 0.11
{-# INLINE readSingleMaybe' #-}
readSingleMaybe' :: (Monad m) => QueryT m a -> Entities -> m (Maybe a, Components)
readSingleMaybe' q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- singleMaybeDyn dynQ es
  return (as, cs')

-- | Match the specified entities.
--
-- @since 0.11
readQueryEntities :: (Monad m) => [EntityID] -> QueryT m a -> Entities -> m ([a], Entities)
readQueryEntities eIds q es = do
  let !(cs', dynQ) = runQuery q $ components es
  as <- readQueryEntitiesDyn eIds dynQ es
  return (as, es {components = cs'})
