{-# LANGUAGE FlexibleContexts #-}

module Data.Aztecs.Access.Class (MonadAccess (..)) where

import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (EntityID (..))
import Data.Aztecs.World.Archetype (Bundle (..))
import Data.Data (Typeable)
import Prelude hiding (all, lookup, map)

-- | Monadic access to a `World`.
class (Monad m) => MonadAccess m where
  -- | Spawn an entity with a component.
  spawn :: Bundle -> m EntityID

  -- | Spawn an entity with a component.
  spawn_ :: Bundle -> m ()
  spawn_ c = do
    _ <- spawn c
    return ()

  -- | Insert a component into an entity.
  insert :: (Component a, Typeable (StorageT a)) => EntityID -> a -> m ()

  -- | Lookup a component on an entity.
  lookup :: (Component a) => EntityID -> m (Maybe a)

  -- | Despawn an entity.
  despawn :: EntityID -> m ()
