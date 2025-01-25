{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Core
  ( EntityID (..),
    Component (..),
    ComponentID (..),
  )
where

import Data.Aztecs.Storage (Storage)
import Data.IntMap (IntMap)
import Data.Kind (Type)
import Data.Typeable (Typeable)

-- | Entity ID.
newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show)

-- | Component ID.
newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

-- | Component that can be stored in the `World`.
class (Typeable a, Storage (StorageT a) a) => Component a where
  -- | `Storage` of this component.
  type StorageT a :: Type -> Type

  type StorageT a = IntMap
