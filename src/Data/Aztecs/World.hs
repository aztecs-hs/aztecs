{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World where

import Data.Aztecs (Component (..), ComponentID, Components (..), EntityID (..), emptyComponents, insertComponentId)
import Data.Aztecs.Archetype (Archetype)
import qualified Data.Aztecs.Archetype as A
import Data.Dynamic (Dynamic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Proxy (..), Typeable, typeOf)

newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

-- | World of entities and their components.
data World = World
  { archetypes :: Map ArchetypeID Archetype,
    archetypeIds :: Map (Set ComponentID) ArchetypeID,
    nextArchetypeId :: ArchetypeID,
    components :: Components,
    entities :: Map EntityID ArchetypeID,
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
      entities = mempty,
      nextEntityId = EntityID 0
    }

-- | Spawn an entity with a component.
spawn :: forall a. (Component a, Typeable (StorageT a)) => a -> World -> (EntityID, World)
spawn c w = case Map.lookup (typeOf (Proxy @a)) (componentIds (components w)) of
  Just cId -> spawnWithId cId c w
  Nothing ->
    let (cId, cs) = insertComponentId @a (components w)
     in spawnWithId cId c w {components = cs}

-- | Spawn an empty entity.
spawnEmpty :: World -> (EntityID, World)
spawnEmpty w = let e = nextEntityId w in (e, w {nextEntityId = EntityID (unEntityId e + 1)})

-- | Spawn an entity with a component and its `ComponentID`.
spawnWithId ::
  forall a.
  (Component a, Typeable (StorageT a)) =>
  ComponentID ->
  a ->
  World ->
  (EntityID, World)
spawnWithId cId c w =
  let (e, w') = spawnEmpty w
   in case Map.lookup (Set.singleton cId) (archetypeIds w) of
        Just aId -> (e, spawnWithArchetypeId' e aId cId c w')
        Nothing ->
          ( e,
            snd $
              insertArchetype
                (Set.singleton cId)
                (A.insert e cId c A.empty)
                w' {entities = Map.insert e (nextArchetypeId w) (entities w)}
          )

-- | Spawn an entity with a component and its `ComponentID` directly into an archetype.
spawnWithArchetypeId ::
  forall a.
  (Component a, Typeable (StorageT a)) =>
  a ->
  ComponentID ->
  ArchetypeID ->
  World ->
  (EntityID, World)
spawnWithArchetypeId c cId aId w =
  let (e, w') = spawnEmpty w
   in (e, spawnWithArchetypeId' e aId cId c w')

spawnWithArchetypeId' ::
  forall a.
  (Component a, Typeable (StorageT a)) =>
  EntityID ->
  ArchetypeID ->
  ComponentID ->
  a ->
  World ->
  World
spawnWithArchetypeId' e aId cId c w =
  let f = A.insert e cId c
   in w
        { archetypes = Map.adjust f aId (archetypes w),
          entities = Map.insert e aId (entities w)
        }

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

despawn :: EntityID -> World -> (Map ComponentID Dynamic, World)
despawn e w =
  let res = do
        aId <- Map.lookup e (entities w)
        arch <- Map.lookup aId (archetypes w)
        return (aId, arch)
   in case res of
        Just (aId, arch) ->
          let (dynAcc, arch') = A.remove e arch
           in ( dynAcc,
                w
                  { archetypes = Map.insert aId arch' (archetypes w),
                    entities = Map.delete e (entities w)
                  }
              )
        Nothing -> (Map.empty, w)
