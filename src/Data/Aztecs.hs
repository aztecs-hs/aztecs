{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs where

import Data.Aztecs.Archetype (Archetype, Component)
import qualified Data.Aztecs.Archetype as A
import Data.Aztecs.Entity (EntityID (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Proxy (..), TypeRep, Typeable, typeOf)

newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

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

insertComponentId :: forall c. (Component c) => Components -> (ComponentID, Components)
insertComponentId cs =
  let cId = nextComponentId cs
   in ( cId,
        cs
          { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds cs),
            nextComponentId = ComponentID (unComponentId cId + 1)
          }
      )

-- | World of entities and their components.
data World = World
  { archetypes :: Map ArchetypeID Archetype,
    archetypeIds :: Map (Set ComponentID) ArchetypeID,
    nextArchetypeId :: ArchetypeID,
    components :: Components,
    nextEntityId :: EntityID
  }
  deriving (Show)

-- | Empty `World`.
empty :: World
empty =
  World
    { archetypes = mempty,
      archetypeIds = mempty,
      nextArchetypeId = ArchetypeID 0,
      components = emptyComponents,
      nextEntityId = EntityID 0
    }

-- | Spawn an entity with a component.
spawn :: forall a. (Component a, Typeable (A.StorageT a)) => a -> World -> (EntityID, World)
spawn c w = case Map.lookup (typeOf (Proxy @a)) (componentIds (components w)) of
  Just cId -> spawnWithId c cId w
  Nothing ->
    let (cId, cs) = insertComponentId @a (components w)
     in spawnWithId c cId w {components = cs}

-- | Spawn an empty entity.
spawnEmpty :: World -> (EntityID, World)
spawnEmpty w = let e = nextEntityId w in (e, w {nextEntityId = EntityID (unEntityId e + 1)})

-- | Spawn an entity with a component and its `ComponentID`.
spawnWithId ::
  forall a.
  (Component a, Typeable (A.StorageT a)) =>
  a ->
  ComponentID ->
  World ->
  (EntityID, World)
spawnWithId c cId w =
  let (e, w') = spawnEmpty w
   in case Map.lookup (Set.singleton cId) (archetypeIds w) of
        Just arch -> (e, w' {archetypes = Map.adjust (A.insert e c) arch (archetypes w)})
        Nothing -> (e, snd $ insertArchetype (Set.singleton cId) (A.insert e c A.empty) w')

-- | Spawn an entity with a component and its `ComponentID` directly into an archetype.
spawnWithArchetypeId ::
  forall a.
  (Component a, Typeable (A.StorageT a)) =>
  a ->
  ComponentID ->
  ArchetypeID ->
  World ->
  (EntityID, World)
spawnWithArchetypeId c cId aId w =
  let (e, w') = spawnEmpty w
      f = A.insert e c
   in (e, w' {archetypes = Map.adjust f aId (archetypes w)})

-- | Insert an archetype by its set of `ComponentID`s.
insertArchetype :: Set ComponentID -> Archetype -> World -> (ArchetypeID, World)
insertArchetype cIds a w =
  let aId = nextArchetypeId w
   in ( aId,
        w
          { archetypes = Map.insert aId a (archetypes w),
            archetypeIds = Map.insert cIds aId (archetypeIds w),
            nextArchetypeId = ArchetypeID (unArchetypeId aId + 1)
          }
      )
