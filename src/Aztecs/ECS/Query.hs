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

    -- ** Conversion
    fromDyn,
    liftQuery,

    -- ** Running

    -- *** Writing
    query,
    querySingle,
    querySingleMaybe,
    queryEntities,

    -- *** Reading
    readQueryEntities,
    readQuery,
    readQuerySingle,
    readQuerySingleMaybe,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Query.Dynamic
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Monad.Identity
import Control.Monad.Trans
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
fetchMap :: forall f a. (Component a) => (a -> a) -> QueryT f a
fetchMap f = fromDynInternal @_ @a $ fetchMapDyn f

-- | Fetch a component and map it with a monadic function, storing the result.
--
-- @since 0.11
{-# INLINE fetchMapM #-}
fetchMapM :: forall f a. (Monad f, Component a) => (a -> f a) -> QueryT f a
fetchMapM f = fromDynInternal @_ @a $ fetchMapDynM f

-- | Fetch a component and map it with some input, storing the result.
--
-- @since 0.11
{-# INLINE zipFetchMap #-}
zipFetchMap :: forall f a b. (Component a) => (b -> a -> a) -> QueryT f b -> QueryT f a
zipFetchMap f = fromWriterInternal @a $ zipFetchMapDyn f

-- | Fetch a component and map it with some input, storing the result and returning some output.
--
-- @since 0.11
{-# INLINE zipFetchMapAccum #-}
zipFetchMapAccum ::
  forall f a b c. (Component a) => (b -> a -> (c, a)) -> QueryT f b -> QueryT f (c, a)
zipFetchMapAccum f = fromWriterInternal @a $ zipFetchMapAccumDyn f

-- | Fetch a component and map it with some input and a monadic function, storing the result.
--
-- @since 0.11
{-# INLINE zipFetchMapM #-}
zipFetchMapM :: forall f a b. (Monad f, Component a) => (b -> a -> f a) -> QueryT f b -> QueryT f a
zipFetchMapM f = fromWriterInternal @a $ zipFetchMapDynM f

-- | Fetch a component and map it with some input and a monadic function,
-- storing the result and returning some output.
--
-- @since 0.11
{-# INLINE zipFetchMapAccumM #-}
zipFetchMapAccumM ::
  forall f a b c. (Monad f, Component a) => (b -> a -> f (c, a)) -> QueryT f b -> QueryT f (c, a)
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

liftQuery :: (MonadTrans g, Monad (g f), Monad f) => QueryT f a -> QueryT (g f) a
liftQuery q = Query $ \cs -> let !(cs', dynQ) = runQuery q cs in (cs', liftQueryDyn dynQ)

-- | Match and update all entities.
--
-- @since 0.11
{-# INLINE query #-}
query :: (Applicative f) => QueryT f a -> Entities -> f ([a], Entities)
query = runQueryWithDyn queryDyn

-- | Match and update a single matched entity.
--
-- @since 0.12
{-# INLINE querySingle #-}
querySingle :: (HasCallStack, Applicative f) => QueryT f a -> Entities -> f (a, Entities)
querySingle = runQueryWithDyn querySingleDyn

-- | Match and update a single matched entity, or `Nothing`.
--
-- @since 0.12
{-# INLINE querySingleMaybe #-}
querySingleMaybe :: (Applicative m) => QueryT m a -> Entities -> m (Maybe a, Entities)
querySingleMaybe = runQueryWithDyn querySingleMaybeDyn

-- | Match and update the specified entities.
--
-- @since 0.12
{-# INLINE queryEntities #-}
queryEntities :: (Monad m) => [EntityID] -> QueryT m a -> Entities -> m ([a], Entities)
queryEntities eIds = runQueryWithDyn $ queryEntitiesDyn eIds

-- | Match and update all entities.
--
-- @since 0.12
{-# INLINE readQuery #-}
readQuery :: (Applicative f) => QueryT f a -> Entities -> f ([a], Entities)
readQuery = runQueryWithDyn $ \q es -> (,es) <$> readQueryDyn q es

-- | Match a single entity.
--
-- @since 0.12
{-# INLINE readQuerySingle #-}
readQuerySingle :: (HasCallStack, Applicative f) => QueryT f a -> Entities -> f (a, Entities)
readQuerySingle = runQueryWithDyn $ \q es -> (,es) <$> readQuerySingleDyn q es

-- | Match a single entity, or `Nothing`.
--
-- @since 0.12
{-# INLINE readQuerySingleMaybe #-}
readQuerySingleMaybe :: (Applicative f) => QueryT f a -> Entities -> f (Maybe a, Entities)
readQuerySingleMaybe = runQueryWithDyn $ \q es -> (,es) <$> readQuerySingleMaybeDyn q es

-- | Match the specified entities.
--
-- @since 0.12
{-# INLINE readQueryEntities #-}
readQueryEntities :: (Applicative f) => [EntityID] -> QueryT f a -> Entities -> f ([a], Entities)
readQueryEntities eIds = runQueryWithDyn $ \q es -> (,es) <$> readQueryEntitiesDyn eIds q es

{-# INLINE runQueryWithDyn #-}
runQueryWithDyn ::
  (Applicative f) =>
  (DynamicQueryT f a -> Entities -> b) ->
  QueryT f a ->
  Entities ->
  b
runQueryWithDyn f q es =
  let !(cs', dynQ) = runQuery q $ components es in f dynQ es {components = cs'}
