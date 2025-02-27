{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Component (Component (..), ComponentID (..)) where

import Aztecs.ECS.World.Storage
import Control.DeepSeq
import Data.Typeable
import GHC.Generics

-- | Component ID.
newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Component that can be stored in the `World`.
class (Typeable a, Storage a (StorageT a)) => Component a where
  -- | `Storage` of this component.
  type StorageT a

  type StorageT a = [a]
