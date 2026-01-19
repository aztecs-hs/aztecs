{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Aztecs.ECS.World.Components.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Components.Internal (Components (..)) where

import Aztecs.ECS.Component.Internal
import Data.Map.Strict (Map)
import Data.Typeable
import GHC.Generics

-- | Component ID map.
data Components = Components
  { -- | Map of component types to identifiers.
    componentIds :: !(Map TypeRep ComponentID),
    -- | Next unique component identifier.
    nextComponentId :: !ComponentID
  }
  deriving (Show, Generic)
