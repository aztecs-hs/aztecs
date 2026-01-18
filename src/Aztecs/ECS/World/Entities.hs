{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.World.Entities
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Entities
  ( Entities (..),
    empty,
    spawn,
    spawnWithArchetypeId,
    insert,
    insertDyn,
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
import Data.Dynamic
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Prelude hiding (lookup)

-- | World of entities and their components.
data Entities = Entities
  { -- | Archetypes.
    --
    -- @since 0.9
    archetypes :: !Archetypes,
    -- | Components.
    --
    -- @since 0.9
    components :: !Components,
    -- | Entities and their archetype identifiers.
    --
    -- @since 0.9
    entities :: !(Map EntityID ArchetypeID)
  }
  deriving (Show, Generic)

-- | Empty `World`.
empty :: Entities
empty =
  Entities
    { archetypes = AS.empty,
      components = CS.empty,
      entities = mempty
    }

-- | Spawn a `Bundle`.
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
          let arch' = runDynamicBundle dynB eId $ A.singleton eId
              node' = Node {nodeComponentIds = cIds, nodeArchetype = arch'}
              (aId, arches) = AS.insertArchetype cIds node' $ archetypes w
           in w
                { archetypes = arches,
                  entities = Map.insert eId aId (entities w),
                  components = components'
                }

-- | Spawn a `DynamicBundle` with a specified `ArchetypeID`.
spawnWithArchetypeId ::
  EntityID ->
  ArchetypeID ->
  DynamicBundle ->
  Entities ->
  Entities
spawnWithArchetypeId e aId b w =
  let f n = n {nodeArchetype = runDynamicBundle b e ((nodeArchetype n) {A.entities = Set.insert e . A.entities $ nodeArchetype n})}
   in w
        { archetypes = (archetypes w) {AS.nodes = Map.adjust f aId (AS.nodes $ archetypes w)},
          entities = Map.insert e aId (entities w)
        }

-- | Insert a component into an entity.
insert :: EntityID -> Bundle -> Entities -> Entities
insert e b w =
  let !(cIds, components', dynB) = unBundle b (components w)
   in insertDyn e cIds dynB w {components = components'}

-- | Insert a component into an entity with its `ComponentID`.
insertDyn :: EntityID -> Set ComponentID -> DynamicBundle -> Entities -> Entities
insertDyn e cIds b w = case Map.lookup e $ entities w of
  Just aId ->
    let (maybeNextAId, arches) = AS.insert e aId cIds b $ archetypes w
        es = case maybeNextAId of
          Just nextAId -> Map.insert e nextAId $ entities w
          Nothing -> entities w
     in w {archetypes = arches, entities = es}
  Nothing -> case AS.lookupArchetypeId cIds $ archetypes w of
    Just aId -> spawnWithArchetypeId e aId b w
    Nothing ->
      let arch = runDynamicBundle b e $ A.singleton e
          node = Node {nodeComponentIds = cIds, nodeArchetype = arch}
          (aId, arches) = AS.insertArchetype cIds node $ archetypes w
       in w {archetypes = arches, entities = Map.insert e aId (entities w)}

-- | Lookup a component in an entity.
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

-- | Remove a component from an entity with its `ComponentID`.
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
despawn :: EntityID -> Entities -> (IntMap Dynamic, Entities)
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
        Nothing -> (IntMap.empty, w)
