{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Aztecs.ECS.World.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Internal (World (..)) where

import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.World.Entities.Internal (Entities)
import GHC.Generics

-- | World of entities and their components.
data World m = World
  { -- | Entities and their components.
    entities :: !(Entities m),
    -- | Next unique entity identifier.
    nextEntityId :: !EntityID
  }
  deriving (Show, Generic)
