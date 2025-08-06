{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Class (ECS (..)) where

import Aztecs.ECS.Bundle
import Data.Kind

-- | Entity Component System (ECS) implementation.
class ECS m where
  -- | Entity identifier.
  type Entity m :: Type

  -- | Task monad for running systems.
  type Task m :: Type -> Type

  -- | Spawn a new entity with a `Bundle` of components.
  spawn :: Bundle (Entity m) m -> m (Entity m)

  -- | Insert a `Bundle` of components into an existing entity
  -- (otherwise this will do nothing).
  insert :: Entity m -> Bundle (Entity m) m -> m ()

  -- | Remove an entity and its components.
  remove :: Entity m -> m ()

  -- | Run a `Task`.
  task :: (Task m) a -> m a
