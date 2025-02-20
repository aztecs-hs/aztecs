{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.ECS.World
  ( World (..),
    empty,
    spawn,
    spawnComponent,
    spawnWithId,
    spawnWithArchetypeId,
    spawnEmpty,
    insert,
    insertWithId,
    lookup,
    remove,
    removeWithId,
    despawn,
  )
where

import Aztecs.ECS.Component
  ( Component (..),
    ComponentID,
  )
import Aztecs.ECS.Entity (EntityID (..))
import Aztecs.ECS.World.Archetype (Bundle (..), DynamicBundle (..))
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (ArchetypeID, Archetypes, Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Components (Components (..))
import qualified Aztecs.ECS.World.Components as CS
import Control.DeepSeq (NFData)
import Data.Dynamic (Dynamic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Typeable (Proxy (..), Typeable, typeOf)
import GHC.Generics (Generic)
import Prelude hiding (lookup)

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

-- | World of entities and their components.
data World = World
  { archetypes :: !Archetypes,
    components :: !Components,
    entities :: !(Map EntityID ArchetypeID),
    nextEntityId :: !EntityID
  }
  deriving (Show, Generic)

instance NFData World

-- | Empty `World`.
empty :: World
empty =
  World
    { archetypes = AS.empty,
      components = CS.empty,
      entities = mempty,
      nextEntityId = EntityID 0
    }

spawn :: Bundle -> World -> (EntityID, World)
spawn b w =
  let (eId, w') = spawnEmpty w
      (cIds, components', dynB) = unBundle b (components w')
   in case AS.lookupArchetypeId cIds (archetypes w') of
        Just aId -> fromMaybe (eId, w') $ do
          node <- AS.lookupNode aId (archetypes w')
          let arch' = runDynamicBundle dynB eId (nodeArchetype node)
          return
            ( eId,
              w'
                { archetypes = (archetypes w') {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)},
                  components = components',
                  entities = Map.insert eId aId (entities w')
                }
            )
        Nothing ->
          let arch' = runDynamicBundle dynB eId A.empty
              (aId, arches) =
                AS.insertArchetype
                  cIds
                  ( Node
                      { nodeComponentIds = cIds,
                        nodeArchetype = arch',
                        nodeAdd = Map.empty,
                        nodeRemove = Map.empty
                      }
                  )
                  (archetypes w')
           in ( eId,
                w'
                  { archetypes = arches,
                    entities = Map.insert eId aId (entities w'),
                    components = components'
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
  let !(e, w') = spawnEmpty w
   in case AS.lookupArchetypeId (Set.singleton cId) (archetypes w) of
        Just aId -> (e, spawnWithArchetypeId' e aId cId c w')
        Nothing ->
          let !(aId, arches) =
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
  let !(e, w') = spawnEmpty w
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
  let !(cId, components') = CS.insert @a (components w)
   in insertWithId e cId c w {components = components'}

-- | Insert a component into an entity with its `ComponentID`.
insertWithId :: (Component a, Typeable (StorageT a)) => EntityID -> ComponentID -> a -> World -> World
insertWithId e cId c w = case Map.lookup e (entities w) of
  Just aId ->
    let (maybeNextAId, arches) = AS.insert e aId cId c $ archetypes w
        es = case maybeNextAId of
          Just nextAId -> Map.insert e nextAId (entities w)
          Nothing -> entities w
     in w {archetypes = arches, entities = es}
  Nothing -> case AS.lookupArchetypeId (Set.singleton cId) (archetypes w) of
    Just aId -> spawnWithArchetypeId' e aId cId c w
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
              (archetypes w)
       in w {archetypes = arches, entities = Map.insert e aId (entities w)}

lookup :: forall a. (Component a) => EntityID -> World -> Maybe a
lookup e w = do
  !cId <- CS.lookup @a (components w)
  !aId <- Map.lookup e (entities w)
  !node <- AS.lookupNode aId (archetypes w)
  A.lookupComponent e cId (nodeArchetype node)

-- | Insert a component into an entity.
remove :: forall a. (Component a, Typeable (StorageT a)) => EntityID -> World -> (Maybe a, World)
remove e w =
  let !(cId, components') = CS.insert @a (components w)
   in removeWithId @a e cId w {components = components'}

removeWithId :: forall a. (Component a, Typeable (StorageT a)) => EntityID -> ComponentID -> World -> (Maybe a, World)
removeWithId e cId w = case Map.lookup e (entities w) of
  Just aId ->
    let (res, as) = AS.remove @a e aId cId $ archetypes w
        (maybeA, es) = case res of
          Just (a, nextAId) -> (Just a, Map.insert e nextAId (entities w))
          Nothing -> (Nothing, entities w)
     in (maybeA, w {archetypes = as, entities = es})
  Nothing -> (Nothing, w)

-- | Despawn an entity, returning its components.
despawn :: EntityID -> World -> (Map ComponentID Dynamic, World)
despawn e w =
  let res = do
        !aId <- Map.lookup e (entities w)
        !node <- AS.lookupNode aId (archetypes w)
        return (aId, node)
   in case res of
        Just (aId, node) ->
          let !(dynAcc, arch') = A.remove e (nodeArchetype node)
           in ( dynAcc,
                w
                  { archetypes = (archetypes w) {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)},
                    entities = Map.delete e (entities w)
                  }
              )
        Nothing -> (Map.empty, w)
