-- |
-- Module      : Aztecs.ECS.World.Bundle.Dynamic
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Bundle.Dynamic (DynamicBundle (..), MonoidDynamicBundle (..)) where

import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype
import Aztecs.ECS.World.Bundle.Dynamic.Class

-- | Dynamic bundle of components.
--
-- @since 0.9
newtype DynamicBundle = DynamicBundle {runDynamicBundle :: EntityID -> Archetype -> Archetype}

-- | @since 0.9
instance Semigroup DynamicBundle where
  DynamicBundle d1 <> DynamicBundle d2 = DynamicBundle $ \eId arch -> d2 eId (d1 eId arch)

-- | @since 0.9
instance Monoid DynamicBundle where
  mempty = DynamicBundle $ \_ arch -> arch

-- | @since 0.9
instance MonoidDynamicBundle DynamicBundle where
  dynBundle cId a = DynamicBundle $ \eId arch -> insertComponent eId cId a arch
