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
  )
where

import Data.Aztecs.Table (ColumnID (ColumnID), Table, TableID (..))
import qualified Data.Aztecs.Table as Table
import Data.Data (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)

newtype Entity = Entity {unEntity :: Int}
  deriving (Eq, Ord, Show)

newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

data Archetype = Archetype Table

newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

data EntityRecord = EntityRecord
  { recordArchetypeId :: ArchetypeID,
    recordTableId :: TableID
  }

data World = World
  { archetypes :: Map ArchetypeID Archetype,
    nextArchetypeId :: ArchetypeID,
    componentIds :: Map ComponentID (Map ArchetypeID ColumnID),
    nextComponentId :: ComponentID,
    entities :: Map Entity EntityRecord,
    nextEntity :: Entity
  }

empty :: World
empty = World Map.empty (ArchetypeID 0) Map.empty (ComponentID 0) Map.empty (Entity 0)

spawn :: (Typeable c) => ComponentID -> c -> World -> IO (Entity, World)
spawn cId c w = do
  let e = nextEntity w
  w' <- insertNew e cId c (w {nextEntity = Entity (unEntity e + 1)})
  return (e, w')

insert :: (Typeable c) => Entity -> ComponentID -> c -> World -> IO World
insert e cId c w = case Map.lookup e (entities w) of
  Just record -> error "TODO"
  Nothing -> insertNew e cId c w

insertNew :: (Typeable c) => Entity -> ComponentID -> c -> World -> IO World
insertNew e cId c w = case Map.lookup cId (componentIds w) of
  Just colIds -> error "TODO"
  Nothing -> do
    let archId = nextArchetypeId w
    table <- Table.replicate 1 c
    let archetypes' = Map.insert archId (Archetype table) (archetypes w)
        componentIds' = Map.insert cId (Map.singleton archId (ColumnID 0)) (componentIds w)
        entities' = Map.insert e (EntityRecord archId (TableID 0)) (entities w)
    return
      w
        { archetypes = archetypes',
          componentIds = componentIds',
          entities = entities',
          nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
        }

lookup :: (Typeable c) => Entity -> ComponentID -> World -> IO (Maybe c)
lookup e cId w = case Map.lookup e (entities w) of
  Just (EntityRecord archId tableId) -> case Map.lookup cId (componentIds w) of
    Just colIds -> case Map.lookup archId colIds of
      Just colId -> do
        let Archetype table = (archetypes w) Map.! archId
        Table.lookup table tableId colId
      Nothing -> return Nothing
    Nothing -> return Nothing
  Nothing -> return Nothing
