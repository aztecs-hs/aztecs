module Aztecs.ECS.World.Bundle.Dynamic
  ( DynamicBundle (..),
    MonoidDynamicBundle (..),
  )
where

import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.World.Archetype (Archetype, insertComponent)
import Aztecs.ECS.World.Bundle.Dynamic.Class (MonoidDynamicBundle (..))

-- | Dynamic bundle of components.
newtype DynamicBundle = DynamicBundle {runDynamicBundle :: EntityID -> Archetype -> Archetype}

instance Semigroup DynamicBundle where
  DynamicBundle d1 <> DynamicBundle d2 = DynamicBundle $ \eId arch -> d2 eId (d1 eId arch)

instance Monoid DynamicBundle where
  mempty = DynamicBundle $ \_ arch -> arch

instance MonoidDynamicBundle DynamicBundle where
  dynBundle cId a = DynamicBundle $ \eId arch -> insertComponent eId cId a arch
