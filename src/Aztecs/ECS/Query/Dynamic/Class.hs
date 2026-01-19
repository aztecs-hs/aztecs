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

  -- | Adjust a `Component` by its `ComponentID`.
  adjustDyn :: (Component m a) => (b -> a -> a) -> ComponentID -> f b -> f a

  -- | Adjust a `Component` by its `ComponentID`, ignoring any output.
  adjustDyn_ :: (Component m a) => (b -> a -> a) -> ComponentID -> f b -> f ()
  adjustDyn_ f cId = void . adjustDyn f cId

  -- | Adjust a `Component` by its `ComponentID` with some applicative functor @g@.
  adjustDynM :: (Monad m, Component m a) => (b -> a -> m a) -> ComponentID -> f b -> f a

  -- | Set a `Component` by its `ComponentID`.
  setDyn :: (Component m a) => ComponentID -> f a -> f a
