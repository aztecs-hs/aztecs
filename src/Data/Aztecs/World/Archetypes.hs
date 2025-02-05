{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.World.Archetypes
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
  )
where

import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.World.Archetype hiding (empty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, lookup, map)

-- | `Archetype` ID.
newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

-- | Node in `Archetypes`.
data Node = Node
  { -- | Unique set of `ComponentID`s of this `Node`.
    nodeComponentIds :: Set ComponentID,
    -- | `Archetype` of this `Node`.
    nodeArchetype :: Archetype,
    -- | Edges to other `Archetype`s by adding a `ComponentID`.
    nodeAdd :: Map ComponentID ArchetypeID,
    -- | Edges to other `Archetype`s by removing a `ComponentID`.
    nodeRemove :: Map ComponentID ArchetypeID
  }
  deriving (Show)

-- | `Archetype` graph.
data Archetypes = Archetypes
  { -- | Archetype nodes in the graph.
    nodes :: Map ArchetypeID Node,
    -- | Mapping of unique `ComponentID` sets to `ArchetypeID`s.
    archetypeIds :: Map (Set ComponentID) ArchetypeID,
    -- | Next unique `ArchetypeID`.
    nextArchetypeId :: ArchetypeID,
    -- | Mapping of `ComponentID`s to `ArchetypeID`s of `Archetypes` that contain them.
    componentIds :: Map ComponentID (Set ArchetypeID)
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
  (aId : aIds') -> foldr Set.intersection aId aIds'
  [] -> Set.empty

-- | Lookup `Archetype`s containing a set of `ComponentID`s.
lookup :: Set ComponentID -> Archetypes -> Map ArchetypeID Archetype
lookup cIds arches =
  Map.fromSet
    (\aId -> nodeArchetype $ nodes arches Map.! aId)
    (findArchetypeIds cIds arches)

-- | Map over `Archetype`s containing a set of `ComponentID`s.
map :: Set ComponentID -> (Archetype -> (a, Archetype)) -> Archetypes -> ([a], Archetypes)
map cIds f arches =
  foldr
    ( \aId (acc, archAcc) ->
        let node = nodes archAcc Map.! aId
            (a, arch') = f (nodeArchetype node)
         in (a : acc, archAcc {nodes = Map.insert aId (node {nodeArchetype = arch'}) (nodes archAcc)})
    )
    ([], arches)
    (findArchetypeIds cIds arches)

lookupArchetypeId :: Set ComponentID -> Archetypes -> Maybe ArchetypeID
lookupArchetypeId cIds arches = Map.lookup cIds (archetypeIds arches)

lookupNode :: ArchetypeID -> Archetypes -> Maybe Node
lookupNode aId arches = Map.lookup aId (nodes arches)
