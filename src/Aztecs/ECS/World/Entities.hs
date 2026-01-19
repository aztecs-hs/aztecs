{-# LANGUAGE BangPatterns #-}
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

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (ArchetypeID, Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Bundle
import Aztecs.ECS.World.Bundle.Dynamic
import qualified Aztecs.ECS.World.Components as CS
import Aztecs.ECS.World.Entities.Internal (Entities (..))
import Data.Dynamic
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (lookup)

-- | Empty `World`.
empty :: Entities m
empty =
  Entities
    { archetypes = AS.empty,
      components = CS.empty,
      entities = mempty
    }

-- | Spawn a `Bundle`. Returns the updated entities and the onInsert hook to run.
spawn :: (Monad m) => EntityID -> BundleT m -> Entities m -> (Entities m, Access m ())
spawn eId b w =
  let (cIds, components', dynB) = unBundle b (components w)
   in case AS.lookupArchetypeId cIds (archetypes w) of
        Just aId -> case AS.lookup aId $ archetypes w of
          Just node ->
            let (arch', hook) =
                  runDynamicBundle
                    dynB
                    eId
                    ( (nodeArchetype node)
                        { A.entities = Set.insert eId . A.entities $ nodeArchetype node
                        }
                    )
             in ( w
                    { archetypes = (archetypes w) {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)},
                      components = components',
                      entities = Map.insert eId aId (entities w)
                    },
                  hook
                )
          Nothing -> (w, return ())
        Nothing ->
          let (arch', hook) = runDynamicBundle dynB eId $ A.singleton eId
              node' = Node {nodeComponentIds = cIds, nodeArchetype = arch'}
              (aId, arches) = AS.insertArchetype cIds node' $ archetypes w
           in ( w
                  { archetypes = arches,
                    entities = Map.insert eId aId (entities w),
                    components = components'
                  },
                hook
              )

-- | Spawn a `DynamicBundle` with a specified `ArchetypeID`. Returns the updated entities and the onInsert hook.
spawnWithArchetypeId ::
  (Monad m) =>
  EntityID ->
  ArchetypeID ->
  DynamicBundle m ->
  Entities m ->
  (Entities m, Access m ())
spawnWithArchetypeId e aId b w =
  let f n =
        let (arch', hook) = runDynamicBundle b e ((nodeArchetype n) {A.entities = Set.insert e . A.entities $ nodeArchetype n})
         in (n {nodeArchetype = arch'}, hook)
      (hooks, nodes') =
        Map.alterF
          ( \maybeN -> case maybeN of
              Just n -> let (n', hook) = f n in (hook, Just n')
              Nothing -> (return (), Nothing)
          )
          aId
          (AS.nodes $ archetypes w)
   in ( w
          { archetypes = (archetypes w) {AS.nodes = nodes'},
            entities = Map.insert e aId (entities w)
          },
        hooks
      )

-- | Insert a component into an entity. Returns the updated entities and the onInsert hook.
insert :: (Monad m) => EntityID -> BundleT m -> Entities m -> (Entities m, Access m ())
insert e b w =
  let !(cIds, components', dynB) = unBundle b (components w)
   in insertDyn e cIds dynB w {components = components'}

-- | Insert a component into an entity with its `ComponentID`. Returns the updated entities and the onInsert hook.
insertDyn :: (Monad m) => EntityID -> Set ComponentID -> DynamicBundle m -> Entities m -> (Entities m, Access m ())
insertDyn e cIds b w = case Map.lookup e $ entities w of
  Just aId ->
    let (maybeNextAId, arches, hook) = AS.insert e aId cIds b $ archetypes w
        es = case maybeNextAId of
          Just nextAId -> Map.insert e nextAId $ entities w
          Nothing -> entities w
     in (w {archetypes = arches, entities = es}, hook)
  Nothing -> case AS.lookupArchetypeId cIds $ archetypes w of
    Just aId -> spawnWithArchetypeId e aId b w
    Nothing ->
      let (arch, hook) = runDynamicBundle b e $ A.singleton e
          node = Node {nodeComponentIds = cIds, nodeArchetype = arch}
          (aId, arches) = AS.insertArchetype cIds node $ archetypes w
       in (w {archetypes = arches, entities = Map.insert e aId (entities w)}, hook)

-- | Lookup a component in an entity.
lookup :: forall m a. (Component m a) => EntityID -> Entities m -> Maybe a
lookup e w = do
  !cId <- CS.lookup @a $ components w
  !aId <- Map.lookup e $ entities w
  !node <- AS.lookup aId $ archetypes w
  A.lookupComponent e cId $ nodeArchetype node

-- | Remove a component from an entity. Returns the component (if found), updated entities, and the onRemove hook.
remove :: forall m a. (Component m a) => EntityID -> Entities m -> (Maybe a, Entities m, Access m ())
remove e w =
  let !(cId, components') = CS.insert @a @m (components w)
   in removeWithId e cId w {components = components'}

-- | Remove a component from an entity with its `ComponentID`. Returns the component (if found), updated entities, and the onRemove hook.
removeWithId :: forall m a. (Component m a) => EntityID -> ComponentID -> Entities m -> (Maybe a, Entities m, Access m ())
removeWithId e cId w = case Map.lookup e (entities w) of
  Just aId ->
    let (res, as, hook) = AS.remove @m @a e aId cId $ archetypes w
        (maybeA, es) = case res of
          Just (a, nextAId) -> (Just a, Map.insert e nextAId (entities w))
          Nothing -> (Nothing, entities w)
     in (maybeA, w {archetypes = as, entities = es}, hook)
  Nothing -> (Nothing, w, return ())

-- | Despawn an entity, returning its components.
despawn :: EntityID -> Entities m -> (IntMap Dynamic, Entities m)
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
