{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
import Aztecs.ECS.World.Components.Internal (Components (..))
import qualified Data.Map.Strict as Map
import Data.Typeable
import Prelude hiding (lookup)

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
insert :: forall a m. (Component m a) => Components -> (ComponentID, Components)
insert cs = case lookup @a cs of
  Just cId -> (cId, cs)
  Nothing -> insert' @a @m cs

-- | Insert a component ID by type.
insert' :: forall c m. (Component m c) => Components -> (ComponentID, Components)
insert' cs =
  let !cId = nextComponentId cs
   in ( cId,
        cs
          { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds cs),
            nextComponentId = ComponentID (unComponentId cId + 1)
          }
      )
