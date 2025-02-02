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
    spawnComponent,
    spawnWithId,
    spawnWithArchetypeId,
    spawnEmpty,
    insert,
    insertWithId,
    despawn,
  )
where

import Data.Aztecs.Component
  ( Component (..),
    ComponentID,
  )
import Data.Aztecs.Entity (ComponentIds, Entity, EntityID (..), ToEntity (..), EntityT)
import qualified Data.Aztecs.Entity as E
import Data.Aztecs.World.Archetype (Archetype (..), Insert)
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (ArchetypeID, Archetypes, Node (..))
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (Components (..))
import qualified Data.Aztecs.World.Components as CS
import Data.Dynamic (Dynamic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Typeable (Proxy (..), Typeable, typeOf)

-- | World of entities and their components.
data World = World
  { archetypes :: Archetypes,
    components :: Components,
    entities :: Map EntityID ArchetypeID,
    nextEntityId :: EntityID
  }
  deriving (Show)

-- | Empty `World`.
empty :: World
empty =
  World
    { archetypes = AS.empty,
      components = CS.empty,
      entities = mempty,
      nextEntityId = EntityID 0
    }

spawn ::
  forall a.
  (ComponentIds (EntityT a), ToEntity a, Insert (Entity (EntityT a))) =>
   a ->
  World ->
  (EntityID, World)
spawn e w =
  let (eId, w') = spawnEmpty w
      (cIds, components') = E.componentIds @(EntityT a) (components w)
   in case AS.lookupArchetypeId cIds (archetypes w) of
        Just aId -> fromMaybe (eId, w') $ do
          node <- AS.lookupNode aId (archetypes w)
          let (_, arch', components'') = A.insert eId (toEntity e) (nodeArchetype node) components'
          return
            ( eId,
              w
                { archetypes = (archetypes w) {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)},
                  components = components'',
                  entities = Map.insert eId aId (entities w)
                }
            )
        Nothing ->
          let (_, arch, components'') = A.insert eId (toEntity e) A.empty components'
              (aId, arches) =
                AS.insertArchetype
                  cIds
                  ( Node
                      { nodeComponentIds = cIds,
                        nodeArchetype = arch,
                        nodeAdd = Map.empty,
                        nodeRemove = Map.empty
                      }
                  )
                  (archetypes w')
           in ( eId,
                w'
                  { archetypes = arches,
                    entities = Map.insert eId aId (entities w),
                    components = components''
                  }
              )

-- | Spawn an entity with a component.
spawnComponent :: forall a. (Component a, Typeable (StorageT a)) => a -> World -> (EntityID, World)
spawnComponent c w = case Map.lookup (typeOf (Proxy @a)) (componentIds (components w)) of
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
   in case AS.lookupArchetypeId (Set.singleton cId) (archetypes w) of
        Just aId -> (e, spawnWithArchetypeId' e aId cId c w')
        Nothing ->
          let (aId, arches) =
                AS.insertArchetype
                  (Set.singleton cId)
                  ( Node
                      { nodeComponentIds = Set.singleton cId,
                        nodeArchetype = A.insertComponent e cId c A.empty,
                        nodeAdd = Map.empty,
                        nodeRemove = Map.empty
                      }
                  )
                  (archetypes w')
           in (e, w' {archetypes = arches, entities = Map.insert e aId (entities w)})

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
  let f n = n {nodeArchetype = A.insertComponent e cId c (nodeArchetype n)}
   in w
        { archetypes = (archetypes w) {AS.nodes = Map.adjust f aId (AS.nodes $ archetypes w)},
          entities = Map.insert e aId (entities w)
        }

-- | Insert a component into an entity.
insert :: forall a. (Component a, Typeable (StorageT a)) => EntityID -> a -> World -> World
insert e c w =
  let (cId, components') = CS.insert @a (components w)
   in insertWithId e cId c w {components = components'}

-- | Insert a component into an entity with its `ComponentID`.
insertWithId :: (Component a, Typeable (StorageT a)) => EntityID -> ComponentID -> a -> World -> World
insertWithId e cId c w = case Map.lookup e (entities w) of
  Just aId -> case AS.lookupNode aId (archetypes w) of
    Just node ->
      if Set.member cId (nodeComponentIds node)
        then w {archetypes = (archetypes w) {AS.nodes = Map.adjust (\n -> n {nodeArchetype = A.insertComponent e cId c (nodeArchetype n)}) aId (AS.nodes $ archetypes w)}}
        else case AS.lookupArchetypeId (Set.insert cId (nodeComponentIds node)) (archetypes w) of
          Just nextAId ->
            let (cs, arch') = A.remove e (nodeArchetype node)
                w' = w {archetypes = (archetypes w) {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)}}
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
                      (archetypes w')
                        { AS.nodes =
                            Map.adjust
                              ( \nextNode ->
                                  nextNode
                                    { nodeArchetype =
                                        A.insertComponent e cId c $
                                          foldr
                                            f
                                            (nodeArchetype nextNode)
                                            (Map.toList cs)
                                    }
                              )
                              nextAId
                              (AS.nodes $ archetypes w')
                        },
                    entities = Map.insert e nextAId (entities w)
                  }
          Nothing ->
            let (s, arch') = A.removeStorages e (nodeArchetype node)
                n =
                  Node
                    { nodeComponentIds = Set.insert cId (nodeComponentIds node),
                      nodeArchetype = A.insertComponent e cId c (Archetype {A.storages = s}),
                      nodeAdd = Map.empty,
                      nodeRemove = Map.singleton cId aId
                    }
                (nextAId, arches) = AS.insertArchetype (Set.insert cId (nodeComponentIds node)) n (archetypes w)
             in w
                  { archetypes =
                      arches
                        { AS.nodes =
                            Map.insert
                              aId
                              node
                                { nodeArchetype = arch',
                                  nodeAdd = Map.insert cId nextAId (nodeAdd node)
                                }
                              (AS.nodes arches)
                        },
                    entities = Map.insert e nextAId (entities w)
                  }
    Nothing -> w
  Nothing -> w

-- | Despawn an entity, returning its components.
despawn :: EntityID -> World -> (Map ComponentID Dynamic, World)
despawn e w =
  let res = do
        aId <- Map.lookup e (entities w)
        node <- AS.lookupNode aId (archetypes w)
        return (aId, node)
   in case res of
        Just (aId, node) ->
          let (dynAcc, arch') = A.remove e (nodeArchetype node)
           in ( dynAcc,
                w
                  { archetypes = (archetypes w) {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)},
                    entities = Map.delete e (entities w)
                  }
              )
        Nothing -> (Map.empty, w)
