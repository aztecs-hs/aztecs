{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Component (Component (..), ComponentID (..)) where

import Aztecs.ECS.World.Storage (Storage)
import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Component ID.
newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Component that can be stored in the `World`.
class (Typeable a, Storage a (StorageT a)) => Component a where
  -- | `Storage` of this component.
  type StorageT a :: Type

  type StorageT a = [a]
