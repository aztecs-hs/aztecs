{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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

import Aztecs.ECS.Component (Component (..), ComponentID)
import Aztecs.ECS.Entity (EntityID (..))
import Aztecs.ECS.World.Archetype
  ( Archetype (..),
    insertComponent,
    removeStorages,
  )
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Storage.Dynamic (insertDyn, removeDyn)
import Control.DeepSeq (NFData (..))
import Data.Dynamic (fromDynamic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Prelude hiding (all, lookup, map)

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

-- | `Archetype` ID.
newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving newtype (Eq, Ord, Show, NFData)

-- | Node in `Archetypes`.
data Node = Node
  { -- | Unique set of `ComponentID`s of this `Node`.
    nodeComponentIds :: !(Set ComponentID),
    -- | `Archetype` of this `Node`.
    nodeArchetype :: !Archetype,
    -- | Edges to other `Archetype`s by adding a `ComponentID`.
    nodeAdd :: !(Map ComponentID ArchetypeID),
    -- | Edges to other `Archetype`s by removing a `ComponentID`.
    nodeRemove :: !(Map ComponentID ArchetypeID)
  }
  deriving (Show, Generic, NFData)

-- | `Archetype` graph.
data Archetypes = Archetypes
  { -- | Archetype nodes in the graph.
    nodes :: !(Map ArchetypeID Node),
    -- | Mapping of unique `ComponentID` sets to `ArchetypeID`s.
    archetypeIds :: !(Map (Set ComponentID) ArchetypeID),
    -- | Next unique `ArchetypeID`.
    nextArchetypeId :: !ArchetypeID,
    -- | Mapping of `ComponentID`s to `ArchetypeID`s of `Archetypes` that contain them.
    componentIds :: !(Map ComponentID (Set ArchetypeID))
  }
  deriving (Show, Generic, NFData)

-- | Empty `Archetypes`.
empty :: Archetypes
empty =
  Archetypes
    { nodes = mempty,
      archetypeIds = mempty,
      nextArchetypeId = ArchetypeID 0,
      componentIds = mempty
    }

-- | Insert an archetype by its set of `ComponentID`s.
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

adjustArchetype :: ArchetypeID -> (Archetype -> Archetype) -> Archetypes -> Archetypes
adjustArchetype aId f arches = arches {nodes = Map.adjust (\node -> node {nodeArchetype = f (nodeArchetype node)}) aId (nodes arches)}

-- | Find `ArchetypeID`s containing a set of `ComponentID`s.
findArchetypeIds :: Set ComponentID -> Archetypes -> Set ArchetypeID
findArchetypeIds cIds arches = case mapMaybe (\cId -> Map.lookup cId (componentIds arches)) (Set.elems cIds) of
  (aId : aIds') -> foldl' Set.intersection aId aIds'
  [] -> Set.empty

-- | Lookup `Archetype`s containing a set of `ComponentID`s.
find :: Set ComponentID -> Archetypes -> Map ArchetypeID Node
find cIds arches = Map.fromSet (\aId -> nodes arches Map.! aId) (findArchetypeIds cIds arches)

-- | Map over `Archetype`s containing a set of `ComponentID`s.
map :: Set ComponentID -> (Archetype -> (a, Archetype)) -> Archetypes -> ([a], Archetypes)
map cIds f arches =
  let go (acc, archAcc) aId =
        let !node = nodes archAcc Map.! aId
            !(a, arch') = f (nodeArchetype node)
            nodes' = Map.insert aId (node {nodeArchetype = arch'}) (nodes archAcc)
         in (a : acc, archAcc {nodes = nodes'})
   in foldl' go ([], arches) $ findArchetypeIds cIds arches

lookupArchetypeId :: Set ComponentID -> Archetypes -> Maybe ArchetypeID
lookupArchetypeId cIds arches = Map.lookup cIds (archetypeIds arches)

lookup :: ArchetypeID -> Archetypes -> Maybe Node
lookup aId arches = Map.lookup aId (nodes arches)

-- | Insert a component into an entity with its `ComponentID`.
insert ::
  (Component a) =>
  EntityID ->
  ArchetypeID ->
  ComponentID ->
  a ->
  Archetypes ->
  (Maybe ArchetypeID, Archetypes)
insert e aId cId c arches = case lookup aId arches of
  Just node ->
    if Set.member cId (nodeComponentIds node)
      then
        let go n = n {nodeArchetype = insertComponent e cId c (nodeArchetype n)}
         in (Nothing, arches {nodes = Map.adjust go aId (nodes arches)})
      else case lookupArchetypeId (Set.insert cId (nodeComponentIds node)) arches of
        Just nextAId ->
          let !(cs, arch') = A.remove e (nodeArchetype node)
              node' = node {nodeArchetype = arch'}
              !arches' = arches {nodes = Map.insert aId node' (nodes arches)}
              f archAcc (itemCId, dyn) =
                let go s = insertDyn (unEntityId e) dyn s
                    storages' = Map.adjust go itemCId (storages archAcc)
                 in archAcc {storages = storages'}
              adjustNode nextNode =
                let nextArch = foldl' f (nodeArchetype nextNode) (Map.toList cs)
                 in nextNode {nodeArchetype = insertComponent e cId c nextArch}
           in (Just nextAId, arches' {nodes = Map.adjust adjustNode nextAId (nodes $ arches')})
        Nothing ->
          let !(s, arch') = removeStorages e (nodeArchetype node)
              !n =
                Node
                  { nodeComponentIds = Set.insert cId (nodeComponentIds node),
                    nodeArchetype = insertComponent e cId c (Archetype {storages = s}),
                    nodeAdd = Map.empty,
                    nodeRemove = Map.singleton cId aId
                  }
              cIds = Set.insert cId (nodeComponentIds node)
              !(nextAId, arches') = insertArchetype cIds n arches
           in let node' =
                    node {nodeArchetype = arch', nodeAdd = Map.insert cId nextAId (nodeAdd node)}
                  nodes' = Map.insert aId node' (nodes arches')
               in (Just nextAId, arches' {nodes = nodes'})
  Nothing -> (Nothing, arches)

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
          (a, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) cId cs
          go' archAcc (itemCId, dyn) =
            let adjustStorage s = insertDyn (unEntityId e) dyn s
             in archAcc {storages = Map.adjust adjustStorage itemCId (storages archAcc)}
          go nextNode =
            nextNode {nodeArchetype = foldl' go' (nodeArchetype nextNode) (Map.toList cs')}
       in ( (,nextAId) <$> (a >>= fromDynamic),
            arches' {nodes = Map.adjust go nextAId (nodes $ arches')}
          )
    Nothing ->
      let !(cs, arch') = removeStorages e (nodeArchetype node)
          (a, cs') = Map.updateLookupWithKey (\_ _ -> Nothing) cId cs
          !n =
            Node
              { nodeComponentIds = Set.insert cId (nodeComponentIds node),
                nodeArchetype = Archetype {storages = cs'},
                nodeAdd = Map.empty,
                nodeRemove = Map.singleton cId aId
              }
          !(nextAId, arches') = insertArchetype (Set.insert cId (nodeComponentIds node)) n arches
          node' = node {nodeArchetype = arch', nodeAdd = Map.insert cId nextAId (nodeAdd node)}
       in ( (,nextAId) <$> (a >>= (\a' -> (fst $ removeDyn (unEntityId e) a') >>= fromDynamic)),
            arches' {nodes = Map.insert aId node' (nodes arches')}
          )
  Nothing -> (Nothing, arches)
