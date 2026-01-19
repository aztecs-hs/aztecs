{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      : Aztecs.ECS.World.Archetypes.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Archetypes.Internal
  ( ArchetypeID (..),
    Node (..),
    Archetypes (..),
  )
where

import Aztecs.ECS.Component.Internal (ComponentID)
import Aztecs.ECS.World.Archetype.Internal (Archetype)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.Generics

-- | `Archetype` ID.
newtype ArchetypeID = ArchetypeID
  { -- | Unique integer identifier.
    unArchetypeId :: Int
  }
  deriving newtype (Eq, Ord, Show)

-- | Node in `Archetypes`.
data Node (m :: Type -> Type) = Node
  { -- | Unique set of `ComponentID`s of this `Node`.
    nodeComponentIds :: !(Set ComponentID),
    -- | `Archetype` of this `Node`.
    nodeArchetype :: !(Archetype m)
  }
  deriving (Show, Generic)

-- | `Archetype` map.
data Archetypes (m :: Type -> Type) = Archetypes
  { -- | Archetype nodes in the map.
    nodes :: !(Map ArchetypeID (Node m)),
    -- | Mapping of unique `ComponentID` sets to `ArchetypeID`s.
    archetypeIds :: !(Map (Set ComponentID) ArchetypeID),
    -- | Next unique `ArchetypeID`.
    nextArchetypeId :: !ArchetypeID,
    -- | Mapping of `ComponentID`s to `ArchetypeID`s of `Archetypes` that contain them.
    componentIds :: !(Map ComponentID (Set ArchetypeID))
  }
  deriving (Show, Generic)
