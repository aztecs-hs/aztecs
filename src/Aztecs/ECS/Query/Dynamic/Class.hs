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
import Control.Monad

-- | Dynamic query functor.
--
-- @since 0.10
class (Monad m, Functor f) => DynamicQueryF m f | f -> m where
  -- | Adjust a `Component` by its `ComponentID`.
  --
  -- @since 0.10
  adjustDyn :: (Component a) => (b -> a -> a) -> ComponentID -> f b -> f a

  -- | Adjust a `Component` by its `ComponentID`, ignoring any output.
  --
  -- @since 0.10
  adjustDyn_ :: (Component a) => (b -> a -> a) -> ComponentID -> f b -> f ()
  adjustDyn_ f cId = void . adjustDyn f cId

  -- | Adjust a `Component` by its `ComponentID` with some applicative functor @g@.
  --
  -- @since 0.10
  adjustDynM :: (Component a) => (b -> a -> m a) -> ComponentID -> f b -> f a

  -- | Set a `Component` by its `ComponentID`.
  --
  -- @since 0.10
  setDyn :: (Component a) => ComponentID -> f a -> f a
