{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

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

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype (Archetype (..))
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Bundle.Dynamic
import Aztecs.ECS.World.Storage.Dynamic
import Control.DeepSeq (NFData (..))
import Data.Dynamic
import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Prelude hiding (all, lookup, map)

-- | `Archetype` ID.
--
-- @since 9.0
newtype ArchetypeID = ArchetypeID
  { -- | Unique integer identifier.
    --
    -- @since 9.0
    unArchetypeId :: Int
  }
  deriving newtype (Eq, Ord, Show, NFData)

-- | Node in `Archetypes`.
--
-- @since 9.0
data Node = Node
  { -- | Unique set of `ComponentID`s of this `Node`.
    --
    -- @since 9.0
    nodeComponentIds :: !(Set ComponentID),
    -- | `Archetype` of this `Node`.
    --
    -- @since 9.0
    nodeArchetype :: !Archetype
  }
  deriving (Show, Generic, NFData)

-- | `Archetype` map.
data Archetypes = Archetypes
  { -- | Archetype nodes in the map.
    --
    -- @since 9.0
    nodes :: !(Map ArchetypeID Node),
    -- | Mapping of unique `ComponentID` sets to `ArchetypeID`s.
    --
    -- @since 9.0
    archetypeIds :: !(Map (Set ComponentID) ArchetypeID),
    -- | Next unique `ArchetypeID`.
    --
    -- @since 9.0
    nextArchetypeId :: !ArchetypeID,
    -- | Mapping of `ComponentID`s to `ArchetypeID`s of `Archetypes` that contain them.
    --
    -- @since 9.0
    componentIds :: !(Map ComponentID (Set ArchetypeID))
  }
  deriving (Show, Generic, NFData)

-- | Empty `Archetypes`.
--
-- @since 9.0
empty :: Archetypes
empty =
  Archetypes
    { nodes = mempty,
      archetypeIds = mempty,
      nextArchetypeId = ArchetypeID 0,
      componentIds = mempty
    }

-- | Insert an archetype by its set of `ComponentID`s.
--
-- @since 9.0
insertArchetype :: Set ComponentID -> Node -> Archetypes -> (ArchetypeID, Archetypes)
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
--
-- @since 9.0
adjustArchetype :: ArchetypeID -> (Archetype -> Archetype) -> Archetypes -> Archetypes
adjustArchetype aId f arches =
  arches {nodes = Map.adjust (\node -> node {nodeArchetype = f (nodeArchetype node)}) aId (nodes arches)}

-- | Find `ArchetypeID`s containing a set of `ComponentID`s.
--
-- @since 9.0
findArchetypeIds :: Set ComponentID -> Archetypes -> Set ArchetypeID
findArchetypeIds cIds arches = case mapMaybe (\cId -> Map.lookup cId (componentIds arches)) (Set.elems cIds) of
  (aId : aIds') -> foldl' Set.intersection aId aIds'
  [] -> Set.empty

-- | Lookup `Archetype`s containing a set of `ComponentID`s.
--
-- @since 9.0
find :: Set ComponentID -> Archetypes -> Map ArchetypeID Node
find cIds arches = Map.fromSet (\aId -> nodes arches Map.! aId) (findArchetypeIds cIds arches)

-- | Map over `Archetype`s containing a set of `ComponentID`s.
--
-- @since 9.0
map :: Set ComponentID -> (Archetype -> (a, Archetype)) -> Archetypes -> ([a], Archetypes)
map cIds f arches =
  let go (acc, archAcc) aId =
        let !node = nodes archAcc Map.! aId
            !(a, arch') = f (nodeArchetype node)
            nodes' = Map.insert aId (node {nodeArchetype = arch'}) (nodes archAcc)
         in (a : acc, archAcc {nodes = nodes'})
   in foldl' go ([], arches) $ findArchetypeIds cIds arches

-- | Lookup an `ArchetypeID` by its set of `ComponentID`s.
--
-- @since 9.0
lookupArchetypeId :: Set ComponentID -> Archetypes -> Maybe ArchetypeID
lookupArchetypeId cIds arches = Map.lookup cIds (archetypeIds arches)

-- | Lookup an `Archetype` by its `ArchetypeID`.
--
-- @since 9.0
lookup :: ArchetypeID -> Archetypes -> Maybe Node
lookup aId arches = Map.lookup aId (nodes arches)

-- | Insert a component into an entity with its `ComponentID`.
--
-- @since 9.0
insert ::
  EntityID ->
  ArchetypeID ->
  Set ComponentID ->
  DynamicBundle ->
  Archetypes ->
  (Maybe ArchetypeID, Archetypes)
insert e aId cIds b arches = case lookup aId arches of
  Just node ->
    if cIds == nodeComponentIds node
      then
        let go n = n {nodeArchetype = runDynamicBundle b e $ nodeArchetype n}
         in (Nothing, arches {nodes = Map.adjust go aId $ nodes arches})
      else case lookupArchetypeId cIds arches of
        Just nextAId ->
          let !(cs, arch') = A.remove e $ nodeArchetype node
              node' = node {nodeArchetype = arch'}
              !arches' = arches {nodes = Map.insert aId node' (nodes arches)}
              adjustNode nextNode =
                let !nextArch = A.insertComponents e cs $ nodeArchetype nextNode
                 in nextNode {nodeArchetype = runDynamicBundle b e nextArch}
           in (Just nextAId, arches' {nodes = Map.adjust adjustNode nextAId (nodes arches')})
        Nothing ->
          let !(s, arch') = A.removeStorages e $ nodeArchetype node
              !n =
                Node
                  { nodeComponentIds = cIds,
                    nodeArchetype = runDynamicBundle b e (Archetype {storages = s, entities = Set.singleton e})
                  }
              !(nextAId, arches') = insertArchetype cIds n arches
           in let nodes' = Map.insert aId (node {nodeArchetype = arch'}) (nodes arches')
               in (Just nextAId, arches' {nodes = nodes'})
  Nothing -> (Nothing, arches)

-- | Remove a component from an entity with its `ComponentID`.
--
-- @since 9.0
remove ::
  (Component a) =>
  EntityID ->
  ArchetypeID ->
  ComponentID ->
  Archetypes ->
  (Maybe (a, ArchetypeID), Archetypes)
remove e aId cId arches = case lookup aId arches of
  Just node -> case lookupArchetypeId (Set.delete cId (nodeComponentIds node)) arches of
    Just nextAId ->
      let !(cs, arch') = A.remove e (nodeArchetype node)
          !arches' = arches {nodes = Map.insert aId node {nodeArchetype = arch'} (nodes arches)}
          (a, cs') = IntMap.updateLookupWithKey (\_ _ -> Nothing) (unComponentId cId) cs
          go' archAcc (itemCId, dyn) =
            let adjustStorage s = fromAscListDyn (Map.elems . Map.insert e dyn . Map.fromAscList . zip (Set.toList $ entities archAcc) $ toAscListDyn s) s
             in archAcc {storages = IntMap.adjust adjustStorage itemCId (storages archAcc)}
          go nextNode =
            nextNode {nodeArchetype = foldl' go' (nodeArchetype nextNode) (IntMap.toList cs')}
       in ( (,nextAId) <$> (a >>= fromDynamic),
            arches' {nodes = Map.adjust go nextAId (nodes arches')}
          )
    Nothing ->
      let !(cs, arch') = A.removeStorages e (nodeArchetype node)
          (a, cs') = IntMap.updateLookupWithKey (\_ _ -> Nothing) (unComponentId cId) cs
          !n =
            Node
              { nodeComponentIds = Set.insert cId (nodeComponentIds node),
                nodeArchetype = Archetype {storages = cs', entities = Set.singleton e}
              }
          !(nextAId, arches') = insertArchetype (Set.insert cId (nodeComponentIds node)) n arches
          node' = node {nodeArchetype = arch'}
          removeDyn s =
            let (res, dyns) = Map.updateLookupWithKey (\_ _ -> Nothing) e . Map.fromAscList . zip (Set.toList $ entities arch') $ toAscListDyn s
             in (res, fromAscListDyn $ Map.elems dyns)
       in ( (,nextAId) <$> (a >>= (\a' -> fst (removeDyn a') >>= fromDynamic)),
            arches' {nodes = Map.insert aId node' (nodes arches')}
          )
  Nothing -> (Nothing, arches)
