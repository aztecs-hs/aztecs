{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Archetypes
  ( Entity (..),
    ArchetypeID (..),
    Archetype (..),
    ComponentID (..),
    ComponentIDSet (..),
    ComponentState (..),
    EntityRecord (..),
    Archetypes (..),
    empty,
    insert,
    insertDyn,
    insertUnchecked,
    insertNewDyn,
    insertNewComponent,
    lookupDyn,
    lookupWithId,
    removeWithId,
    despawn,
  )
where

import Data.Aztecs.Components (ComponentID (..))
import Data.Aztecs.Table (Column, ColumnID (ColumnID), Table, TableID (..))
import qualified Data.Aztecs.Table as Table
import Data.Data (Typeable)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (lookup)

-- | Entity ID.
newtype Entity = Entity {unEntity :: Int}
  deriving (Eq, Ord, Show)

-- | Archetype ID.
newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

-- | Set of component IDs.
newtype ComponentIDSet = ComponentIDSet {unComponentIdSet :: (Set ComponentID)}
  deriving (Eq, Ord, Show)

-- | Archetype component storage.
data Archetype = Archetype ComponentIDSet Table deriving (Show)

data EntityRecord = EntityRecord
  { recordArchetypeId :: ArchetypeID,
    recordTableId :: TableID
  }
  deriving (Show)

data ComponentState = ComponentState
  { componentColumnIds :: (Map ArchetypeID ColumnID)
  }
  deriving (Show)

-- | Archetypes of entities and components.
data Archetypes = Archetypes
  { archetypes :: Map ArchetypeID Archetype,
    archetypeIds :: Map ComponentIDSet ArchetypeID,
    nextArchetypeId :: ArchetypeID,
    componentStates :: Map ComponentID ComponentState,
    entities :: Map Entity EntityRecord
  }
  deriving (Show)

-- | Empty archetypes.
empty :: Archetypes
empty =
  Archetypes
    { archetypes = Map.empty,
      archetypeIds = Map.empty,
      nextArchetypeId = ArchetypeID 0,
      componentStates = Map.empty,
      entities = Map.empty
    }

-- | Insert a component into an `Entity`.
insertUnchecked ::
  forall c.
  (Typeable c) =>
  Entity ->
  ComponentID ->
  c ->
  Archetypes ->
  Archetypes
insertUnchecked e cId c w = case Map.lookup e (entities w) of
  Just record ->
    let arch@(Archetype (ComponentIDSet idSet) table) =
          archetypes w Map.! (recordArchetypeId record)
        (_, arch') = despawnArch arch (recordTableId record)
        archetypes' = Map.insert (recordArchetypeId record ) arch' (archetypes w)
        idSet' = ComponentIDSet $ Set.insert cId idSet
     in case Map.lookup idSet' (archetypeIds w) of
          Just archId -> error "TODO"
          Nothing ->
            let archId = nextArchetypeId w
                table' = Table.cons (recordTableId record) c table
             in w
                  { archetypes = Map.insert archId (Archetype idSet' table') archetypes',
                    archetypeIds = Map.insert idSet' archId (archetypeIds w),
                    nextArchetypeId = ArchetypeID (unArchetypeId archId + 1),
                    entities =
                      Map.insert
                        e
                        (EntityRecord archId (TableID $ Table.length table' - 1))
                        (entities w)
                  }
  Nothing -> error "TODO"

-- | Insert a component into an `Entity` with its `ComponentID`.
insert :: forall c. (Typeable c) => Entity -> ComponentID -> c -> Archetypes -> Archetypes
insert e cId c = insertDyn e cId $ toDyn c

insertDyn :: Entity -> ComponentID -> Dynamic -> Archetypes -> Archetypes
insertDyn e cId c w = case Map.lookup e (entities w) of
  Just record ->
    let arch@(Archetype (ComponentIDSet idSet) table) = archetypes w Map.! (recordArchetypeId record)
        idSet' = ComponentIDSet $ Set.insert cId idSet
     in case Map.lookup idSet' (archetypeIds w) of
          Just archId ->
            let (col, arch') = despawnArch arch (recordTableId record)
                Archetype _ newTable = archetypes w Map.! archId
                newTable' = Table.fromList [Table.colFromList [c] <> col] <> newTable
                archetypes' = Map.insert (recordArchetypeId record) arch' (archetypes w)
             in w
                  { archetypes = Map.insert archId (Archetype idSet' newTable') archetypes',
                    entities = Map.insert e (EntityRecord archId (TableID $ Table.length newTable' - 1)) (entities w)
                  }
          Nothing ->
            let (_, arch') = despawnArch arch (recordTableId record)
                archetypes' = Map.insert (recordArchetypeId record) arch' (archetypes w)
                archId = nextArchetypeId w
                table' = Table.consDyn (recordTableId record) c table
             in w
                  { archetypes = Map.insert archId (Archetype idSet' table') archetypes',
                    archetypeIds = Map.insert idSet' archId (archetypeIds w),
                    nextArchetypeId = ArchetypeID (unArchetypeId archId + 1),
                    entities = Map.insert e (EntityRecord archId (TableID $ Table.length table' - 1)) (entities w)
                  }
  Nothing -> insertNewDyn e cId c w

insertNewDyn :: Entity -> ComponentID -> Dynamic -> Archetypes -> Archetypes
insertNewDyn e cId c w = case Map.lookup cId (componentStates w) of
  Just cState ->
    let archId = archetypeIds w Map.! (ComponentIDSet (Set.singleton cId))
        Archetype _ table = archetypes w Map.! archId
        table' = Table.singletonDyn c <> table
     in w
          { archetypes =
              Map.insert
                archId
                (Archetype (ComponentIDSet (Set.singleton cId)) table')
                (archetypes w),
            archetypeIds =
              Map.insert (ComponentIDSet (Set.singleton cId)) archId (archetypeIds w),
            componentStates =
              Map.insert
                cId
                (ComponentState (Map.insert archId (ColumnID 0) (componentColumnIds cState)))
                (componentStates w),
            entities =
              Map.insert
                e
                (EntityRecord archId (TableID (Table.length table' - 1)))
                (entities w),
            nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
          }
  Nothing -> insertNewComponent e cId c w

insertNewComponent :: forall c. (Typeable c) => Entity -> ComponentID -> c -> Archetypes -> Archetypes
insertNewComponent e cId c w =
  let archId = nextArchetypeId w
      table = Table.singleton c
   in w
        { archetypes = Map.insert archId (Archetype (ComponentIDSet (Set.singleton cId)) table) (archetypes w),
          archetypeIds = Map.insert (ComponentIDSet (Set.singleton cId)) archId (archetypeIds w),
          componentStates =
            Map.insert
              cId
              (ComponentState (Map.singleton archId (ColumnID 0)))
              (componentStates w),
          entities = Map.insert e (EntityRecord archId (TableID 0)) (entities w),
          nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
        }

-- | Lookup a component in an `Entity` with its `ComponentID`.
lookupWithId :: (Typeable c) => Entity -> ComponentID -> Archetypes -> Maybe c
lookupWithId e cId w = lookupDyn e cId w >>= fromDynamic

lookupDyn :: Entity -> ComponentID -> Archetypes -> Maybe Dynamic
lookupDyn e cId w = do
  (EntityRecord archId tableId) <- Map.lookup e (entities w)
  cState <- Map.lookup cId (componentStates w)
  colId <- Map.lookup archId (componentColumnIds cState)
  let Archetype _ table = (archetypes w) Map.! archId
  Table.lookupDyn tableId colId table

-- | Despawn an `Entity`.
despawn :: Entity -> Archetypes -> Archetypes
despawn e w =
  let res = do
        record <- Map.lookup e (entities w)
        let arch = archetypes w Map.! (recordArchetypeId record)
            (_, arch') = despawnArch arch (recordTableId record)
        return w {archetypes = Map.insert (recordArchetypeId record) arch' (archetypes w)}
   in fromMaybe w res

despawnArch :: Archetype -> TableID -> (Column, Archetype)
despawnArch (Archetype idSet t) tableId =
  let (col, t') = Table.removeCol tableId t
   in (col, Archetype idSet t')

-- | Remove a component from an `Entity` with its `ComponentID`.
removeWithId :: Entity -> ComponentID -> Archetypes -> Maybe (Dynamic, Archetypes)
removeWithId e cId archs = do
  record <- Map.lookup e (entities archs)
  let archId = recordArchetypeId record
      (Archetype (ComponentIDSet cs) table) = archetypes archs Map.! archId
  (dyn, table') <- removeWithId' archId (recordTableId record) archs cId table
  let archetypes' = Map.insert archId (Archetype (ComponentIDSet cs) table') (archetypes archs)
  return (dyn, archs {archetypes = archetypes'})

removeWithId' :: ArchetypeID -> TableID -> Archetypes -> ComponentID -> Table -> Maybe (Dynamic, Table)
removeWithId' archId tId w cId t = do
  let cState = componentStates w Map.! cId
  colId <- Map.lookup archId (componentColumnIds cState)
  Table.removeDyn tId colId t
