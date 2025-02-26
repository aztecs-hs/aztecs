{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.World.Entities
  ( Entities (..),
    empty,
    spawn,
    spawnComponent,
    spawnWithId,
    spawnWithArchetypeId,
    insert,
    insertWithId,
    lookup,
    remove,
    removeWithId,
    despawn,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (ArchetypeID, Archetypes, Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Bundle
import Aztecs.ECS.World.Bundle.Dynamic
import Aztecs.ECS.World.Components (Components (..))
import qualified Aztecs.ECS.World.Components as CS
import Control.DeepSeq
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics
import Prelude hiding (lookup)

-- | World of entities and their components.
data Entities = Entities
  { archetypes :: !Archetypes,
    components :: !Components,
    entities :: !(Map EntityID ArchetypeID)
  }
  deriving (Show, Generic, NFData)

-- | Empty `World`.
empty :: Entities
empty =
  Entities
    { archetypes = AS.empty,
      components = CS.empty,
      entities = mempty
    }

spawn :: EntityID -> Bundle -> Entities -> Entities
spawn eId b w =
  let (cIds, components', dynB) = unBundle b (components w)
   in case AS.lookupArchetypeId cIds (archetypes w) of
        Just aId -> fromMaybe w $ do
          node <- AS.lookup aId $ archetypes w
          let arch' =
                runDynamicBundle
                  dynB
                  eId
                  ( (nodeArchetype node)
                      { A.entities = Set.insert eId . A.entities $ nodeArchetype node
                      }
                  )
          return
            w
              { archetypes = (archetypes w) {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)},
                components = components',
                entities = Map.insert eId aId (entities w)
              }
        Nothing ->
          let arch' = runDynamicBundle dynB eId A.empty {A.entities = Set.singleton eId}
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
                  (archetypes w)
           in w
                { archetypes = arches,
                  entities = Map.insert eId aId (entities w),
                  components = components'
                }

-- | Spawn an entity with a component.
spawnComponent :: forall a. (Component a) => EntityID -> a -> Entities -> Entities
spawnComponent e c w = case Map.lookup (typeOf (Proxy @a)) (componentIds (components w)) of
  Just cId -> spawnWithId e cId c w
  Nothing ->
    let (cId, cs) = CS.insert @a (components w) in spawnWithId e cId c w {components = cs}

-- | Spawn an entity with a component and its `ComponentID`.
spawnWithId ::
  forall a.
  (Component a) =>
  EntityID ->
  ComponentID ->
  a ->
  Entities ->
  Entities
spawnWithId e cId c w = case AS.lookupArchetypeId (Set.singleton cId) (archetypes w) of
  Just aId -> spawnWithArchetypeId e aId cId c w
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
            (archetypes w)
     in w {archetypes = arches, entities = Map.insert e aId (entities w)}

spawnWithArchetypeId ::
  forall a.
  (Component a) =>
  EntityID ->
  ArchetypeID ->
  ComponentID ->
  a ->
  Entities ->
  Entities
spawnWithArchetypeId e aId cId c w =
  let f n = n {nodeArchetype = A.insertComponent e cId c (nodeArchetype n)}
   in w
        { archetypes = (archetypes w) {AS.nodes = Map.adjust f aId (AS.nodes $ archetypes w)},
          entities = Map.insert e aId (entities w)
        }

-- | Insert a component into an entity.
insert :: forall a. (Component a) => EntityID -> a -> Entities -> Entities
insert e c w =
  let !(cId, components') = CS.insert @a (components w)
   in insertWithId e cId c w {components = components'}

-- | Insert a component into an entity with its `ComponentID`.
insertWithId :: (Component a) => EntityID -> ComponentID -> a -> Entities -> Entities
insertWithId e cId c w = case Map.lookup e (entities w) of
  Just aId ->
    let (maybeNextAId, arches) = AS.insert e aId cId c $ archetypes w
        es = case maybeNextAId of
          Just nextAId -> Map.insert e nextAId (entities w)
          Nothing -> entities w
     in w {archetypes = arches, entities = es}
  Nothing -> case AS.lookupArchetypeId (Set.singleton cId) (archetypes w) of
    Just aId -> spawnWithArchetypeId e aId cId c w
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

lookup :: forall a. (Component a) => EntityID -> Entities -> Maybe a
lookup e w = do
  !cId <- CS.lookup @a $ components w
  !aId <- Map.lookup e $ entities w
  !node <- AS.lookup aId $ archetypes w
  A.lookupComponent e cId $ nodeArchetype node

-- | Insert a component into an entity.
remove :: forall a. (Component a) => EntityID -> Entities -> (Maybe a, Entities)
remove e w =
  let !(cId, components') = CS.insert @a (components w)
   in removeWithId @a e cId w {components = components'}

removeWithId :: forall a. (Component a) => EntityID -> ComponentID -> Entities -> (Maybe a, Entities)
removeWithId e cId w = case Map.lookup e (entities w) of
  Just aId ->
    let (res, as) = AS.remove @a e aId cId $ archetypes w
        (maybeA, es) = case res of
          Just (a, nextAId) -> (Just a, Map.insert e nextAId (entities w))
          Nothing -> (Nothing, entities w)
     in (maybeA, w {archetypes = as, entities = es})
  Nothing -> (Nothing, w)

-- | Despawn an entity, returning its components.
despawn :: EntityID -> Entities -> (Map ComponentID Dynamic, Entities)
despawn e w =
  let res = do
        !aId <- Map.lookup e $ entities w
        !node <- AS.lookup aId $ archetypes w
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
