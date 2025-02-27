{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.Access.Class (MonadAccess (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Bundle.Class

-- | Monadic access to a `World`.
class (MonoidBundle b, Monad m) => MonadAccess b m | m -> b where
  -- | Spawn an entity with a component.
  spawn :: b -> m EntityID

  -- | Spawn an entity with a component.
  spawn_ :: b -> m ()
  spawn_ c = do
    _ <- spawn c
    return ()

  -- | Insert a component into an entity.
  insert :: (Component a) => EntityID -> a -> m ()

  -- | Lookup a component on an entity.
  lookup :: (Component a) => EntityID -> m (Maybe a)

  -- | Remove a component from an entity.
  remove :: (Component a) => EntityID -> m (Maybe a)

  -- | Despawn an entity.
  despawn :: EntityID -> m ()
