{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.ECS.World.Components
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Components
  ( ComponentID (..),
    Components (..),
    empty,
    lookup,
    insert,
    insert',
  )
where

import Aztecs.ECS.Component
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable
import GHC.Generics
import Prelude hiding (lookup)

-- | Component ID map.
data Components = Components
  { -- | Map of component types to identifiers.
    componentIds :: !(Map TypeRep ComponentID),
    -- | Next unique component identifier.
    nextComponentId :: !ComponentID
  }
  deriving (Show, Generic)

-- | Empty `Components`.
empty :: Components
empty =
  Components
    { componentIds = mempty,
      nextComponentId = ComponentID 0
    }

-- | Lookup a component ID by type.
lookup :: forall a. (Typeable a) => Components -> Maybe ComponentID
lookup cs = Map.lookup (typeOf (Proxy @a)) (componentIds cs)

-- | Insert a component ID by type, if it does not already exist.
insert :: forall a. (Component a) => Components -> (ComponentID, Components)
insert cs = case lookup @a cs of
  Just cId -> (cId, cs)
  Nothing -> insert' @a cs

-- | Insert a component ID by type.
insert' :: forall c. (Component c) => Components -> (ComponentID, Components)
insert' cs =
  let !cId = nextComponentId cs
   in ( cId,
        cs
          { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds cs),
            nextComponentId = ComponentID (unComponentId cId + 1)
          }
      )
