{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Components
  ( ComponentID (..),
    Components (..),
    empty,
    insert,
    lookup,
  )
where

import Data.Data (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (..), TypeRep, typeOf)
import Prelude hiding (lookup)

-- | Component ID.
newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

-- | World of entities and components.
data Components = Components
  { componentIds :: Map TypeRep ComponentID,
    nextComponentId :: ComponentID
  }
  deriving (Show)

-- | Empty world.
empty :: Components
empty =
  Components
    { componentIds = Map.empty,
      nextComponentId = ComponentID 0
    }

-- | Insert a `ComponentID` into the `World`.
insert :: forall c. (Typeable c) => Components -> (ComponentID, Components)
insert w = case Map.lookup (typeOf (Proxy @c)) (componentIds w) of
  Just cId -> (cId, w)
  Nothing ->
    let cId = nextComponentId w
        w' =
          w
            { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds w),
              nextComponentId = ComponentID (unComponentId cId + 1)
            }
     in (cId, w')

lookup :: forall c. (Typeable c) => Components -> Maybe ComponentID
lookup w = Map.lookup (typeOf (Proxy @c)) (componentIds w)
