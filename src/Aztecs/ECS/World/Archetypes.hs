{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Aztecs.ECS.World.Archetypes
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Archetypes
  ( ArchetypeID (..),
    Node (..),
    Archetypes (..),
    empty,
    insertArchetype,
    lookupArchetypeId,
    findArchetypeIds,
    lookup,
    find,
    map,
    adjustArchetype,
    insert,
    remove,
  )
where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Event
import Aztecs.ECS.World.Archetype (Archetype (..))
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes.Internal
import Aztecs.ECS.World.Bundle.Dynamic
import Aztecs.ECS.World.Storage.Dynamic
import Data.Dynamic
import Data.Foldable hiding (find)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Prelude hiding (all, lookup, map)

-- | Empty `Archetypes`.
empty :: Archetypes m
empty =
  Archetypes
    { nodes = mempty,
      archetypeIds = mempty,
      nextArchetypeId = ArchetypeID 0,
      componentIds = mempty
    }

-- | Insert an archetype by its set of `ComponentID`s.
insertArchetype :: Set ComponentID -> Node m -> Archetypes m -> (ArchetypeID, Archetypes m)
insertArchetype cIds n arches =
  let aId = nextArchetypeId arches
   in ( aId,
        arches
          { nodes = Map.insert aId n (nodes arches),
            archetypeIds = Map.insert cIds aId (archetypeIds arches),
            nextArchetypeId = ArchetypeID (unArchetypeId aId + 1),
            componentIds = Map.unionWith (<>) (Map.fromSet (const (Set.singleton aId)) cIds) (componentIds arches)
          }
      )

-- | Adjust an `Archetype` by its `ArchetypeID`.
adjustArchetype :: ArchetypeID -> (Archetype m -> Archetype m) -> Archetypes m -> Archetypes m
adjustArchetype aId f arches =
  arches {nodes = Map.adjust (\node -> node {nodeArchetype = f (nodeArchetype node)}) aId (nodes arches)}

-- | Find `ArchetypeID`s containing a set of `ComponentID`s.
findArchetypeIds :: Set ComponentID -> Archetypes m -> Set ArchetypeID
findArchetypeIds cIds arches = case mapMaybe (\cId -> Map.lookup cId (componentIds arches)) (Set.elems cIds) of
  (aId : aIds') -> foldl' Set.intersection aId aIds'
  [] -> Set.empty

-- | Lookup `Archetype`s containing a set of `ComponentID`s.
find :: Set ComponentID -> Archetypes m -> Map ArchetypeID (Node m)
find cIds arches = Map.fromSet (\aId -> nodes arches Map.! aId) (findArchetypeIds cIds arches)

-- | Map over `Archetype`s containing a set of `ComponentID`s.
map :: Set ComponentID -> (Archetype m -> (a, Archetype m)) -> Archetypes m -> ([a], Archetypes m)
map cIds f arches =
  let go (acc, archAcc) aId =
        let !node = nodes archAcc Map.! aId
            !(a, arch') = f (nodeArchetype node)
            nodes' = Map.insert aId (node {nodeArchetype = arch'}) (nodes archAcc)
         in (a : acc, archAcc {nodes = nodes'})
   in foldl' go ([], arches) $ findArchetypeIds cIds arches

-- | Lookup an `ArchetypeID` by its set of `ComponentID`s.
lookupArchetypeId :: Set ComponentID -> Archetypes m -> Maybe ArchetypeID
lookupArchetypeId cIds arches = Map.lookup cIds (archetypeIds arches)

-- | Lookup an `Archetype` by its `ArchetypeID`.
lookup :: ArchetypeID -> Archetypes m -> Maybe (Node m)
lookup aId arches = Map.lookup aId (nodes arches)

-- | Insert a component into an entity with its `ComponentID`.
insert ::
  (Monad m) =>
  EntityID ->
  ArchetypeID ->
  Set ComponentID ->
  DynamicBundle m ->
  Archetypes m ->
  (Maybe ArchetypeID, Archetypes m, Access m ())
insert e aId cIds b arches = case lookup aId arches of
  Just node ->
    if Set.isSubsetOf cIds $ nodeComponentIds node
      then
        let go n =
              let (arch', hook) = runDynamicBundle b e $ nodeArchetype n
               in (n {nodeArchetype = arch'}, hook)
            (hooks, nodes') =
              Map.alterF
                ( \maybeN -> case maybeN of
                    Just n -> let (n', hook) = go n in (hook, Just n')
                    Nothing -> (return (), Nothing)
                )
                aId
                $ nodes arches
         in (Nothing, arches {nodes = nodes'}, hooks)
      else
        let cIds' = cIds <> nodeComponentIds node
         in case lookupArchetypeId cIds' arches of
              Just nextAId ->
                let !(cs, arch) = A.remove e $ nodeArchetype node
                    node' = node {nodeArchetype = arch}
                    !nodes' = Map.insert aId node' $ nodes arches
                    adjustNode nextNode =
                      let nextArch = nodeArchetype nextNode
                          nextArch' = nextArch {A.entities = Set.insert e $ A.entities nextArch}
                          !nextArch'' = A.insertComponents e cs nextArch'
                          (finalArch, hook) = runDynamicBundle b e nextArch''
                       in (nextNode {nodeArchetype = finalArch}, hook)
                    (hooks, nodes'') =
                      Map.alterF
                        ( \maybeN -> case maybeN of
                            Just n -> let (n', hook) = adjustNode n in (hook, Just n')
                            Nothing -> (return (), Nothing)
                        )
                        nextAId
                        nodes'
                 in (Just nextAId, arches {nodes = nodes''}, hooks)
              Nothing ->
                let !(s, arch) = A.removeStorages e $ nodeArchetype node
                    nodes' = Map.insert aId node {nodeArchetype = arch} $ nodes arches
                    (nextArch, hook) = runDynamicBundle b e Archetype {storages = s, entities = Set.singleton e}
                    !n = Node {nodeComponentIds = cIds', nodeArchetype = nextArch}
                    !(nextAId, arches') = insertArchetype cIds' n arches {nodes = nodes'}
                 in (Just nextAId, arches', hook)
  Nothing -> (Nothing, arches, return ())

-- | Remove a component from an entity with its `ComponentID`.
remove ::
  (Component m a) =>
  EntityID ->
  ArchetypeID ->
  ComponentID ->
  Archetypes m ->
  (Maybe (a, ArchetypeID), Archetypes m, Access m ())
remove e aId cId arches = case lookup aId arches of
  Just node -> case lookupArchetypeId (Set.delete cId (nodeComponentIds node)) arches of
    Just nextAId ->
      let !(cs, arch') = A.remove e (nodeArchetype node)
          !arches' = arches {nodes = Map.insert aId node {nodeArchetype = arch'} (nodes arches)}
          (a, cs') = IntMap.updateLookupWithKey (\_ _ -> Nothing) (unComponentId cId) cs
          go' archAcc (itemCId, dyn) =
            let adjustStorage s = fromAscVectorDyn (V.fromList . Map.elems . Map.insert e dyn . Map.fromAscList . zip (Set.toList $ entities archAcc) . V.toList $ toAscVectorDyn s) s
             in archAcc {storages = IntMap.adjust adjustStorage itemCId (storages archAcc)}
          go nextNode =
            nextNode {nodeArchetype = foldl' go' (nodeArchetype nextNode) (IntMap.toList cs')}
          maybeA = a >>= fromDynamic
          hook = maybe (return ()) (\comp -> componentOnRemove e comp >> triggerEntityEvent e (OnRemove comp)) maybeA
       in ( (,nextAId) <$> maybeA,
            arches' {nodes = Map.adjust go nextAId (nodes arches')},
            hook
          )
    Nothing ->
      let !(cs, arch') = A.removeStorages e (nodeArchetype node)
          (a, cs') = IntMap.updateLookupWithKey (\_ _ -> Nothing) (unComponentId cId) cs
          destCIds = Set.delete cId (nodeComponentIds node)
          !n =
            Node
              { nodeComponentIds = destCIds,
                nodeArchetype = Archetype {storages = cs', entities = Set.singleton e}
              }
          !(nextAId, arches') = insertArchetype destCIds n arches
          node' = node {nodeArchetype = arch'}
          maybeA = a >>= (\dynS -> V.headM (toAscVectorDyn dynS) >>= fromDynamic)
          hook = maybe (return ()) (\comp -> componentOnRemove e comp >> triggerEntityEvent e (OnRemove comp)) maybeA
       in ( (,nextAId) <$> maybeA,
            arches' {nodes = Map.insert aId node' (nodes arches')},
            hook
          )
  Nothing -> (Nothing, arches, return ())
