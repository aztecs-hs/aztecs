{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Aztecs.ECS.World.Entities.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Entities.Internal (Entities (..)) where

import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.World.Archetypes.Internal (ArchetypeID, Archetypes)
import Aztecs.ECS.World.Components.Internal (Components)
import Data.Map (Map)
import GHC.Generics

-- | World of entities and their components.
data Entities m = Entities
  { -- | Archetypes.
    archetypes :: !(Archetypes m),
    -- | Components.
    components :: !Components,
    -- | Entities and their archetype identifiers.
    entities :: !(Map EntityID ArchetypeID)
  }
  deriving (Show, Generic)
