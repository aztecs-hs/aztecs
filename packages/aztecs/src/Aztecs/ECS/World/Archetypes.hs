{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.World.Archetypes
  ( ArchetypeID (..),
    Node (..),
    Archetypes (..),
    empty,
    insertArchetype,
    lookupArchetypeId,
    findArchetypeIds,
    lookupNode,
    lookup,
    map,
    adjustArchetype,
    insert,
  )
where

import Aztecs.ECS.Component (Component (..), ComponentID)
import Aztecs.ECS.Entity (EntityID (..))
import Aztecs.ECS.World.Archetype hiding (empty)
import Data.Data (Typeable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, lookup, map)

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

-- | `Archetype` ID.
newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

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
  deriving (Show)

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
  deriving (Show)

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
lookup :: Set ComponentID -> Archetypes -> Map ArchetypeID Node
lookup cIds arches =
  Map.fromSet
    (\aId -> nodes arches Map.! aId)
    (findArchetypeIds cIds arches)

-- | Map over `Archetype`s containing a set of `ComponentID`s.
map :: Set ComponentID -> (Archetype -> (a, Archetype)) -> Archetypes -> ([a], Archetypes)
map cIds f arches =
  foldl'
    ( \(acc, archAcc) aId ->
        let !node = nodes archAcc Map.! aId
            !(a, arch') = f (nodeArchetype node)
         in (a : acc, archAcc {nodes = Map.insert aId (node {nodeArchetype = arch'}) (nodes archAcc)})
    )
    ([], arches)
    (findArchetypeIds cIds arches)

lookupArchetypeId :: Set ComponentID -> Archetypes -> Maybe ArchetypeID
lookupArchetypeId cIds arches = Map.lookup cIds (archetypeIds arches)

lookupNode :: ArchetypeID -> Archetypes -> Maybe Node
lookupNode aId arches = Map.lookup aId (nodes arches)

-- | Insert a component into an entity with its `ComponentID`.
insert ::
  (Component a, Typeable (StorageT a)) =>
  EntityID ->
  ArchetypeID ->
  ComponentID ->
  a ->
  Archetypes ->
  (Maybe ArchetypeID, Archetypes)
insert e aId cId c arches = case lookupNode aId arches of
  Just node ->
    if Set.member cId (nodeComponentIds node)
      then (Nothing, arches {nodes = Map.adjust (\n -> n {nodeArchetype = insertComponent e cId c (nodeArchetype n)}) aId (nodes arches)})
      else case lookupArchetypeId (Set.insert cId (nodeComponentIds node)) arches of
        Just nextAId ->
          let !(cs, arch') = remove e (nodeArchetype node)
              !arches' = arches {nodes = Map.insert aId node {nodeArchetype = arch'} (nodes arches)}
              f archAcc (itemCId, dyn) =
                archAcc
                  { storages =
                      Map.adjust
                        (\s -> s {storageDyn = insertDyn s (unEntityId e) dyn (storageDyn s)})
                        itemCId
                        (storages archAcc)
                  }
           in ( Just nextAId,
                arches'
                  { nodes =
                      Map.adjust
                        ( \nextNode ->
                            nextNode
                              { nodeArchetype =
                                  insertComponent e cId c $
                                    foldl'
                                      f
                                      (nodeArchetype nextNode)
                                      (Map.toList cs)
                              }
                        )
                        nextAId
                        (nodes $ arches')
                  }
              )
        Nothing ->
          let !(s, arch') = removeStorages e (nodeArchetype node)
              !n =
                Node
                  { nodeComponentIds = Set.insert cId (nodeComponentIds node),
                    nodeArchetype = insertComponent e cId c (Archetype {storages = s}),
                    nodeAdd = Map.empty,
                    nodeRemove = Map.singleton cId aId
                  }
              !(nextAId, arches') = insertArchetype (Set.insert cId (nodeComponentIds node)) n arches
           in ( Just nextAId,
                arches'
                  { nodes =
                      Map.insert
                        aId
                        node
                          { nodeArchetype = arch',
                            nodeAdd = Map.insert cId nextAId (nodeAdd node)
                          }
                        (nodes arches')
                  }
              )
  Nothing -> (Nothing, arches)
