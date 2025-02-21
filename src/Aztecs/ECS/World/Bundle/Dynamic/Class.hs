{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aztecs.ECS.World.Bundle.Dynamic.Class (MonoidDynamicBundle (..)) where

import Aztecs.ECS.Component (Component (..))
import Aztecs.ECS.World.Components (ComponentID)
import Data.Data (Typeable)

-- | Monoid bundle of dynamic components.
class MonoidDynamicBundle a where
  -- | Add a component to the bundle by its `ComponentID`.
  dynBundle :: (Component c, Typeable (StorageT c)) => ComponentID -> c -> a
