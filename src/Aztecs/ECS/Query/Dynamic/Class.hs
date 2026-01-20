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
  fetchDyn :: (Component m a) => ComponentID -> f a

  -- | Try to fetch a `Component` by its `ComponentID`.
  fetchMaybeDyn :: (Component m a) => ComponentID -> f (Maybe a)
  fetchMaybeDyn cId = Just <$> fetchDyn cId

  -- | Map over a `Component` by its `ComponentID`.
  mapDyn :: (Component m a) => (a -> a) -> ComponentID -> f a

  -- | Map over a `Component` by its `ComponentID`, ignoring any output.
  mapDyn_ :: (Component m a) => (a -> a) -> ComponentID -> f ()
  mapDyn_ f cId = void $ mapDyn f cId

  -- | Map over a `Component` by its `ComponentID` with a monadic function.
  mapDynM :: (Monad m, Component m a) => (a -> m a) -> ComponentID -> f a

  -- | Map over a `Component` by its `ComponentID` with input.
  mapDynWith :: (Component m b) => (a -> b -> b) -> ComponentID -> f a -> f b

  -- | Map over a `Component` by its `ComponentID` with input, ignoring any output.
  mapDynWith_ :: (Component m b) => (a -> b -> b) -> ComponentID -> f a -> f ()
  mapDynWith_ f cId = void . mapDynWith f cId

  -- | Map over a `Component` by its `ComponentID` with input and a monadic function.
  mapDynWithM :: (Monad m, Component m b) => (a -> b -> m b) -> ComponentID -> f a -> f b

  -- | Map over a `Component` by its `ComponentID` with input, returning a tuple of the result and the updated component.
  mapDynWithAccum :: (Component m c) => (b -> c -> (a, c)) -> ComponentID -> f b -> f (a, c)

  -- | Map over a `Component` by its `ComponentID` with input and a monadic function, returning a tuple.
  mapDynWithAccumM :: (Monad m, Component m c) => (b -> c -> m (a, c)) -> ComponentID -> f b -> f (a, c)

  filter :: f a  -> (a -> Bool) -> f a

  -- | Run a query without tracking changes.
  untracked :: f a -> f a
