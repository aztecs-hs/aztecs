{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      : Aztecs.ECS.World.Archetype.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Archetype.Internal (Archetype (..), empty) where

import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.World.Storage.Dynamic (DynamicStorage)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

-- | Archetype of entities and components.
-- An archetype is guranteed to contain one of each stored component per entity.
data Archetype (m :: Type -> Type) = Archetype
  { -- | Component storages.
    storages :: !(IntMap DynamicStorage),
    -- | Entities stored in this archetype.
    entities :: !(Set EntityID)
  }
  deriving (Show, Generic)

instance Semigroup (Archetype m) where
  a <> b = Archetype {storages = storages a <> storages b, entities = entities a <> entities b}

instance Monoid (Archetype m) where
  mempty = empty

-- | Empty archetype.
empty :: Archetype m
empty = Archetype {storages = IntMap.empty, entities = Set.empty}
