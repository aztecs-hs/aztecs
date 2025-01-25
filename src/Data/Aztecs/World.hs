{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World
  ( ArchetypeID (..),
    World (..),
    empty,
    spawn,
    spawnWithId,
    spawnWithArchetypeId,
    spawnEmpty,
    insert,
    insertWithId,
    insertArchetype,
    despawn,
  )
where

import Data.Aztecs.Core
  ( Component (..),
    ComponentID,
    EntityID (..),
  )
import Data.Aztecs.World.Archetype (Archetype (..))
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (Components (..))
import qualified Data.Aztecs.World.Components as CS
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
    archetypeComponents :: Map ArchetypeID (Set ComponentID),
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
      archetypeComponents = mempty,
      nextArchetypeId = ArchetypeID 0,
      components = CS.empty,
      entities = mempty,
      nextEntityId = EntityID 0
    }

-- | Spawn an entity with a component.
spawn :: forall a. (Component a, Typeable (StorageT a)) => a -> World -> (EntityID, World)
spawn c w = case Map.lookup (typeOf (Proxy @a)) (componentIds (components w)) of
  Just cId -> spawnWithId cId c w
  Nothing ->
    let (cId, cs) = CS.insert @a (components w)
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
            archetypeComponents = Map.insert aId cIds (archetypeComponents w),
            nextArchetypeId = ArchetypeID (unArchetypeId aId + 1)
          }
      )

insert :: forall a. (Component a, Typeable (StorageT a)) => EntityID -> a -> World -> World
insert e c w =
  let (cId, components') = CS.insert @a (components w)
   in insertWithId e cId c w {components = components'}

insertWithId :: (Component a, Typeable (StorageT a)) => EntityID -> ComponentID -> a -> World -> World
insertWithId e cId c w = case Map.lookup e (entities w) of
  Just aId -> case Map.lookup aId (archetypeComponents w) of
    Just cIds ->
      if Set.member cId cIds
        then w {archetypes = Map.adjust (A.insert e cId c) aId (archetypes w)}
        else
          let arch = archetypes w Map.! aId
           in case Map.lookup (Set.insert cId cIds) (archetypeIds w) of
                Just nextAId ->
                  let (cs, arch') = A.remove e arch
                      w' = w {archetypes = Map.insert aId arch' (archetypes w)}
                      f (itemCId, dyn) archAcc =
                        archAcc
                          { A.storages =
                              Map.adjust
                                (\s -> s {A.storageDyn = A.insertDyn s (unEntityId e) dyn (A.storageDyn s)})
                                itemCId
                                (A.storages archAcc)
                          }
                   in w'
                        { archetypes =
                            Map.adjust
                              (\nextArch -> A.insert e cId c $ foldr f nextArch (Map.toList cs))
                              nextAId
                              (archetypes w')
                        }
                Nothing ->
                  let (s, arch') = A.removeStorages e arch
                      w' = w {archetypes = Map.insert aId arch' (archetypes w)}
                      newArch = Archetype {A.storages = s}
                      newArch' = A.insert e cId c newArch
                   in snd $ insertArchetype (Set.insert cId cIds) newArch' w'
    Nothing -> w
  Nothing -> w

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
