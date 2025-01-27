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
    lookupArchetypes,
    mapArchetypes,
  )
where

import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.World.Archetype hiding (lookup, empty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, lookup, map)

newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

data Node = Node
  { nodeComponentIds :: Set ComponentID,
    nodeArchetype :: Archetype,
    nodeAdd :: Map ComponentID ArchetypeID,
    nodeRemove :: Map ComponentID ArchetypeID
  }
  deriving (Show)

data Archetypes = Archetypes
  { nodes :: Map ArchetypeID Node,
    archetypeIds :: Map (Set ComponentID) ArchetypeID,
    nextArchetypeId :: ArchetypeID,
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

findArchetypeIds :: Set ComponentID -> Archetypes -> Set ArchetypeID
findArchetypeIds cIds arches = case mapMaybe (\cId -> Map.lookup cId (componentIds arches)) (Set.elems cIds) of
  (aId : aIds') -> foldr Set.intersection aId aIds'
  [] -> Set.empty

lookup :: Set ComponentID -> Archetypes -> [Archetype]
lookup cIds arches = fmap (\aId -> nodeArchetype $ nodes arches Map.! aId) (Set.toList $ findArchetypeIds cIds arches)

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

lookupArchetypes :: ArchetypeID -> Archetypes -> [Archetype]
lookupArchetypes aId arches = case lookupNode aId arches of
  Just n -> nodeArchetype n : concatMap (`lookupArchetypes` arches) (Map.elems (nodeAdd n))
  Nothing -> []

mapArchetypes :: ArchetypeID -> (Archetype -> (a, Archetype)) -> Archetypes -> ([a], Archetypes)
mapArchetypes aId f arches = fromMaybe ([], arches) $ do
  node <- lookupNode aId arches
  let next = Map.elems (nodeAdd node)
      (a, arch) = f (nodeArchetype node)
      node' = node {nodeArchetype = arch}
      arches' = arches {nodes = Map.insert aId node' (nodes arches)}
  return $
    foldr
      ( \aId' (acc, archAcc) ->
          let (as, archAcc') = mapArchetypes aId' f archAcc in (as ++ acc, archAcc')
      )
      ([a], arches')
      next
