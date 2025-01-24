{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Core
  ( EntityID (..),
    Component (..),
    ComponentID (..),
    Components (..),
    emptyComponents,
    insertComponentId,
    lookupComponentId,
    componentId
  )
where

import Data.Aztecs.Storage (Storage)
import Data.IntMap (IntMap)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (..), TypeRep, Typeable, typeOf)

newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show)

newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

class (Typeable a, Storage (StorageT a) a) => Component a where
  type StorageT a :: Type -> Type
  type StorageT a = IntMap

data Components = Components
  { componentIds :: Map TypeRep ComponentID,
    nextComponentId :: ComponentID
  }
  deriving (Show)

emptyComponents :: Components
emptyComponents =
  Components
    { componentIds = mempty,
      nextComponentId = ComponentID 0
    }

lookupComponentId :: forall a. (Typeable a) => Components -> Maybe ComponentID
lookupComponentId cs = Map.lookup (typeOf (Proxy @a)) (componentIds cs)

insertComponentId :: forall c. (Component c) => Components -> (ComponentID, Components)
insertComponentId cs =
  let cId = nextComponentId cs
   in ( cId,
        cs
          { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds cs),
            nextComponentId = ComponentID (unComponentId cId + 1)
          }
      )

componentId :: forall a. (Component a) => Components -> (ComponentID, Components)
componentId cs = case lookupComponentId @a cs of
  Just cId -> (cId, cs)
  Nothing -> insertComponentId @a cs