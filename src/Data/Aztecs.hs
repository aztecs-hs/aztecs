{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs
  ( Entity (..),
    ArchetypeID (..),
    Archetype (..),
    ComponentID (..),
    EntityRecord (..),
    World (..),
    empty,
    spawn,
    insert,
    lookup,
    remove,
    removeWithId,
    despawn,
  )
where

import Data.Aztecs.Table (ColumnID (ColumnID), Table, TableID (..))
import qualified Data.Aztecs.Table as Table
import Data.Data (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Proxy (..), TypeRep, typeOf)
import Prelude hiding (lookup)

newtype Entity = Entity {unEntity :: Int}
  deriving (Eq, Ord, Show)

newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

newtype ComponentIDSet = ComponentIDSet {unComponentIdSet :: (Set ComponentID)}
  deriving (Eq, Ord, Show)

data Archetype = Archetype ComponentIDSet Table

newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

data EntityRecord = EntityRecord
  { recordArchetypeId :: ArchetypeID,
    recordTableId :: TableID
  }

data ComponentState = ComponentState
  { componentColumnIds :: (Map ArchetypeID ColumnID),
    removeComponent :: TableID -> ColumnID -> Table -> Table
  }

data World = World
  { archetypes :: Map ArchetypeID Archetype,
    nextArchetypeId :: ArchetypeID,
    componentIds :: Map TypeRep ComponentID,
    componentStates :: Map ComponentID ComponentState,
    nextComponentId :: ComponentID,
    entities :: Map Entity EntityRecord,
    nextEntity :: Entity
  }

empty :: World
empty =
  World
    Map.empty
    (ArchetypeID 0)
    Map.empty
    Map.empty
    (ComponentID 0)
    Map.empty
    (Entity 0)

spawn :: (Typeable c) => ComponentID -> c -> World -> (Entity, World)
spawn cId c w = do
  let e = nextEntity w
      w' = insertNew e cId c (w {nextEntity = Entity (unEntity e + 1)})
   in (e, w')

insert :: (Typeable c) => Entity -> ComponentID -> c -> World -> World
insert e cId c w = case Map.lookup e (entities w) of
  Just record -> error "TODO"
  Nothing -> insertNew e cId c w

insertNew :: forall c. (Typeable c) => Entity -> ComponentID -> c -> World -> World
insertNew e cId c w = case Map.lookup cId (componentStates w) of
  Just colIds -> error "TODO"
  Nothing ->
    let archId = nextArchetypeId w
        table = Table.singleton c
        archetypes' = Map.insert archId (Archetype (ComponentIDSet (Set.singleton cId)) table) (archetypes w)
        f tId colId t = fromMaybe t $ snd <$> Table.remove @c tId colId t
        componentStates' =
          Map.insert
            cId
            (ComponentState (Map.singleton archId (ColumnID 0)) f)
            (componentStates w)
        entities' = Map.insert e (EntityRecord archId (TableID 0)) (entities w)
     in w
          { archetypes = archetypes',
            componentStates = componentStates',
            entities = entities',
            nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
          }

lookup :: (Typeable c) => Entity -> ComponentID -> World -> Maybe c
lookup e cId w = case Map.lookup e (entities w) of
  Just (EntityRecord archId tableId) -> case Map.lookup cId (componentStates w) of
    Just cState -> case Map.lookup archId (componentColumnIds cState) of
      Just colId -> do
        let Archetype _ table = (archetypes w) Map.! archId
        Table.lookup table tableId colId
      Nothing -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

despawn :: Entity -> World -> World
despawn e w =
  let res = do
        record <- Map.lookup e (entities w)
        let archId = recordArchetypeId record
            (Archetype (ComponentIDSet cs) table) = archetypes w Map.! archId
            table' = foldr (removeWithId' archId record w) table (Set.toList cs)
            archetypes' = Map.insert archId (Archetype (ComponentIDSet cs) table') (archetypes w)
        return $ w {archetypes = archetypes'}
   in fromMaybe w res

remove :: forall c. (Typeable c) => Entity -> World -> World
remove e w = case Map.lookup (typeOf (Proxy @c)) (componentIds w) of
  Just cId -> removeWithId e cId w
  Nothing -> w

removeWithId :: Entity -> ComponentID -> World -> World
removeWithId e cId w =
  let res = do
        record <- Map.lookup e (entities w)
        let archId = recordArchetypeId record
            (Archetype (ComponentIDSet cs) table) = archetypes w Map.! archId
            table' = removeWithId' archId record w cId table
            archetypes' = Map.insert archId (Archetype (ComponentIDSet cs) table') (archetypes w)
        return $ w {archetypes = archetypes'}
   in fromMaybe w res

removeWithId' :: ArchetypeID -> EntityRecord -> World -> ComponentID -> Table -> Table
removeWithId' archId record w cId table =
  let cState = componentStates w Map.! cId
   in removeComponent cState (recordTableId record) (componentColumnIds cState Map.! archId) table
