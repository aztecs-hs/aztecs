{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Aztecs.ECS.Query.Dynamic.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Dynamic.Class (DynamicQueryF (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Control.Monad

-- | Dynamic query functor.
class (Monad m, Functor f) => DynamicQueryF m f | f -> m where
  -- | Fetch the currently matched `EntityID`.
  entity :: f EntityID

  -- | Fetch a `Component` by its `ComponentID`.
  queryDyn :: (Component m a) => ComponentID -> f a

  -- | Try to query a `Component` by its `ComponentID`.
  queryMaybeDyn :: (Component m a) => ComponentID -> f (Maybe a)
  queryMaybeDyn cId = Just <$> queryDyn cId

  -- | Map over a `Component` by its `ComponentID`.
  queryMapDyn :: (Component m a) => (a -> a) -> ComponentID -> f a

  -- | Map over a `Component` by its `ComponentID`, ignoring any output.
  queryMapDyn_ :: (Component m a) => (a -> a) -> ComponentID -> f ()
  queryMapDyn_ f cId = void $ queryMapDyn f cId

  -- | Map over a `Component` by its `ComponentID` with a monadic function.
  queryMapDynM :: (Monad m, Component m a) => (a -> m a) -> ComponentID -> f a

  -- | Map over a `Component` by its `ComponentID` with input.
  queryMapDynWith :: (Component m b) => (a -> b -> b) -> ComponentID -> f a -> f b

  -- | Map over a `Component` by its `ComponentID` with input, ignoring any output.
  queryMapDynWith_ :: (Component m b) => (a -> b -> b) -> ComponentID -> f a -> f ()
  queryMapDynWith_ f cId = void . queryMapDynWith f cId

  -- | Map over a `Component` by its `ComponentID` with input and a monadic function.
  queryMapDynWithM :: (Monad m, Component m b) => (a -> b -> m b) -> ComponentID -> f a -> f b

  -- | Map over a `Component` by its `ComponentID` with input, returning a tuple of the result and the updated component.
  queryMapDynWithAccum :: (Component m c) => (b -> c -> (a, c)) -> ComponentID -> f b -> f (a, c)

  -- | Map over a `Component` by its `ComponentID` with input and a monadic function, returning a tuple.
  queryMapDynWithAccumM :: (Monad m, Component m c) => (b -> c -> m (a, c)) -> ComponentID -> f b -> f (a, c)

  -- | Filter a query and map the results, constraining the query to entities that satisfy the predicate.
  queryFilterMap :: (a -> Maybe b) -> f a -> f b

  -- | Filter a query, constraining it to entities that satisfy the predicate.
  queryFilter :: (a -> Bool) -> f a -> f a
  queryFilter p fa = queryFilterMap (\a -> if p a then Just a else Nothing) fa

  -- | Run a query without tracking changes.
  queryUntracked :: f a -> f a
