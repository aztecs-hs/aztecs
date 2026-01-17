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
-- Module      : Aztecs.ECS.Query.Dynamic
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Reader
  ( -- * Queries
    QueryReader (..),
    QueryReaderF (..),
    DynamicQueryReaderF (..),

    -- ** Running
    all,
    all',
    single,
    single',
    singleMaybe,
    singleMaybe',

    -- * Filters
    QueryFilter (..),
    with,
    without,
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Reader
import Aztecs.ECS.Query.Reader.Class
import Aztecs.ECS.World.Components (Components)
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Aztecs.ECS.World.Entities as E
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import GHC.Stack
import Prelude hiding (all)

-- | Query to read from entities.
--
-- @since 0.10
newtype QueryReader a
  = QueryReader
  { -- | Run a query reader.
    --
    -- @since 0.10
    runQueryReader :: Components -> (Set ComponentID, Components, DynamicQueryReader a)
  }
  deriving (Functor)

-- | @since 0.10
instance Applicative QueryReader where
  pure a = QueryReader (mempty,,pure a)
  {-# INLINE pure #-}

  (QueryReader f) <*> (QueryReader g) = QueryReader $ \cs ->
    let !(cIdsG, cs', aQS) = g cs
        !(cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)
  {-# INLINE (<*>) #-}

-- | @since 0.10
instance QueryReaderF QueryReader where
  fetch :: forall a. (Component a) => QueryReader a
  fetch = QueryReader $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchDyn cId)
  {-# INLINE fetch #-}

  fetchMaybe :: forall a. (Component a) => QueryReader (Maybe a)
  fetchMaybe = QueryReader $ \cs ->
    let !(cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', fetchMaybeDyn cId)
  {-# INLINE fetchMaybe #-}

-- | @since 0.10
instance DynamicQueryReaderF QueryReader where
  entity = QueryReader (mempty,,entity)
  {-# INLINE entity #-}

  fetchDyn cId = QueryReader (Set.singleton cId,,fetchDyn cId)
  {-# INLINE fetchDyn #-}

  fetchMaybeDyn cId = QueryReader (Set.singleton cId,,fetchMaybeDyn cId)
  {-# INLINE fetchMaybeDyn #-}

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

-- | Match all entities.
--
-- @since 0.10
all :: QueryReader a -> Entities -> (Vector a, Entities)
all q es = let !(as, cs) = all' q es in (as, es {E.components = cs})
{-# INLINE all #-}

-- | Match all entities.
--
-- @since 0.10
all' :: QueryReader a -> Entities -> (Vector a, Components)
all' q es = let !(rs, cs', dynQ) = runQueryReader q (E.components es) in (allDyn rs dynQ es, cs')
{-# INLINE all' #-}

-- | Match a single entity.
--
-- @since 0.10
single :: (HasCallStack) => QueryReader a -> Entities -> (a, Entities)
single q es = let !(a, cs) = single' q es in (a, es {E.components = cs})
{-# INLINE single #-}

-- | Match a single entity.
--
-- @since 0.10
single' :: (HasCallStack) => QueryReader a -> Entities -> (a, Components)
single' q es = let !(rs, cs', dynQ) = runQueryReader q (E.components es) in (singleDyn rs dynQ es, cs')
{-# INLINE single' #-}

-- | Match a single entity.
--
-- @since 0.10
singleMaybe :: QueryReader a -> Entities -> (Maybe a, Entities)
singleMaybe q es = let !(a, cs) = singleMaybe' q es in (a, es {E.components = cs})
{-# INLINE singleMaybe #-}

-- | Match a single entity.
--
-- @since 0.10
singleMaybe' :: QueryReader a -> Entities -> (Maybe a, Components)
singleMaybe' q es = let !(rs, cs', dynQ) = runQueryReader q (E.components es) in (singleMaybeDyn rs dynQ es, cs')
{-# INLINE singleMaybe' #-}
