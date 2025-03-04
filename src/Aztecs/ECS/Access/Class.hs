{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Aztecs.ECS.Access.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Access.Class (MonadAccess (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Bundle.Class

-- | Monadic access to a `World`.
--
-- @since 9.0
class (MonoidBundle b, Monad m) => MonadAccess b m | m -> b where
  -- | Spawn an entity with a component.
  --
  -- @since 9.0
  spawn :: b -> m EntityID

  -- | Spawn an entity with a component.
  --
  -- @since 9.0
  spawn_ :: b -> m ()
  spawn_ c = do
    _ <- spawn c
    return ()

  -- | Insert a component into an entity.
  --
  -- @since 9.0
  insert :: EntityID -> b -> m ()

  -- | Lookup a component on an entity.
  --
  -- @since 9.0
  lookup :: (Component a) => EntityID -> m (Maybe a)

  -- | Remove a component from an entity.
  --
  -- @since 9.0
  remove :: (Component a) => EntityID -> m (Maybe a)

  -- | Despawn an entity.
  --
  -- @since 9.0
  despawn :: EntityID -> m ()
