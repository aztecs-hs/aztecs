{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Archetypes
  ( ArchetypeID (..),
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

import Data.Aztecs
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

-- | Archetype ID.
newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

-- | Set of component IDs.
newtype ComponentIDSet = ComponentIDSet {unComponentIdSet :: (Set ComponentID)}
  deriving (Eq, Ord, Show)

-- | Archetype component storage.
data Archetype = Archetype
  { archetypeIdSet :: ComponentIDSet,
    archetypeTable :: Table,
    archetypeAdd :: Map ComponentID ArchetypeID,
    archetypeRemove :: Map ComponentID ArchetypeID
  }
  deriving (Show)

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
    let archId = nextArchetypeId w
        arch =
          archetypes w Map.! (recordArchetypeId record)
        (_, arch') = despawnArch arch (recordTableId record)
        arch'' =
          arch'
            { archetypeAdd = Map.insert cId archId (archetypeAdd arch')
            }
        archetypes' = Map.insert (recordArchetypeId record) arch'' (archetypes w)
        idSet = unComponentIdSet $ archetypeIdSet arch
        idSet' = ComponentIDSet $ Set.insert cId idSet
        table' = Table.snocDyn (recordTableId record) (toDyn c) (archetypeTable arch)
        newArch =
          Archetype
            { archetypeIdSet = idSet',
              archetypeTable = table',
              archetypeAdd = Map.empty,
              archetypeRemove =
                Map.fromList $
                  map
                    (\x -> (x, archId))
                    (Set.toList $ unComponentIdSet idSet')
            }
     in w
          { archetypes = Map.insert archId newArch archetypes',
            archetypeIds = Map.insert idSet' archId (archetypeIds w),
            nextArchetypeId = ArchetypeID (unArchetypeId archId + 1),
            entities =
              Map.insert
                e
                (EntityRecord archId (TableID $ Table.length table' - 1))
                (entities w),
            componentStates =
              Map.insert
                cId
                ( ComponentState $
                    Map.singleton
                      archId
                      ( ColumnID
                          ( fromMaybe 0 $
                              (\col -> Table.colLength col - 1)
                                <$> Table.lookupColumn (recordTableId record) table'
                          )
                      )
                )
                (componentStates w)
          }
  Nothing -> error "TODO"

-- | Insert a component into an `Entity` with its `ComponentID`.
insert :: forall c. (Typeable c) => Entity -> ComponentID -> c -> Archetypes -> Archetypes
insert e cId c = insertDyn e cId $ toDyn c

insertDyn :: Entity -> ComponentID -> Dynamic -> Archetypes -> Archetypes
insertDyn e cId c w = case Map.lookup e (entities w) of
  Just record ->
    let arch = archetypes w Map.! (recordArchetypeId record)
        idSet' = ComponentIDSet $ Set.insert cId (unComponentIdSet $ archetypeIdSet arch)
     in case Map.lookup idSet' (archetypeIds w) of
          Just archId ->
            let (col, arch') = despawnArch arch (recordTableId record)
                col' = col <> Table.colFromList [c]
                newArch = archetypes w Map.! archId
                newTable' = Table.fromList [col'] <> (archetypeTable newArch)
                archetypes' = Map.insert (recordArchetypeId record) arch' (archetypes w)
                f (i, idx) states =
                  let cState = fromMaybe (ComponentState Map.empty) (Map.lookup i states)
                      cState' =
                        cState
                          { componentColumnIds =
                              Map.insert archId (ColumnID idx) (componentColumnIds cState)
                          }
                   in Map.insert i cState' states
             in w
                  { archetypes = Map.insert archId newArch {archetypeTable = newTable'} archetypes',
                    entities = Map.insert e (EntityRecord archId (TableID $ Table.length newTable' - 1)) (entities w),
                    componentStates = foldr f (componentStates w) (zip (Set.toList (unComponentIdSet $ archetypeIdSet arch)) [0 ..])
                  }
          Nothing ->
            let archId = nextArchetypeId w
                (_, arch') = despawnArch arch (recordTableId record)
                arch'' =
                  arch' {archetypeAdd = Map.insert cId archId (archetypeAdd arch')}
                archetypes' = Map.insert (recordArchetypeId record) arch' (archetypes w)
                table' = Table.snocDyn (recordTableId record) c (archetypeTable arch)
                newArch =
                  Archetype
                    { archetypeIdSet = idSet',
                      archetypeTable = table',
                      archetypeAdd = Map.empty,
                      archetypeRemove = Map.insert cId (recordArchetypeId record) (archetypeRemove arch'')
                    }
             in w
                  { archetypes = Map.insert archId newArch archetypes',
                    archetypeIds = Map.insert idSet' archId (archetypeIds w),
                    nextArchetypeId = ArchetypeID (unArchetypeId archId + 1),
                    entities = Map.insert e (EntityRecord archId (TableID $ Table.length table' - 1)) (entities w),
                    componentStates = Map.insert cId (ComponentState $ Map.singleton archId (ColumnID 0)) (componentStates w)
                  }
  Nothing -> insertNewDyn e cId c w

insertNewDyn :: Entity -> ComponentID -> Dynamic -> Archetypes -> Archetypes
insertNewDyn e cId c w = case Map.lookup cId (componentStates w) of
  Just cState ->
    let archId = archetypeIds w Map.! (ComponentIDSet (Set.singleton cId))
        arch = archetypes w Map.! archId
        table' = Table.singletonDyn c <> archetypeTable arch
     in w
          { archetypes =
              Map.insert
                archId
                ( arch
                    { archetypeTable = table'
                    }
                )
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
                (entities w)
          }
  Nothing -> insertNewComponent e cId c w

insertNewComponent :: forall c. (Typeable c) => Entity -> ComponentID -> c -> Archetypes -> Archetypes
insertNewComponent e cId c w =
  let archId = nextArchetypeId w
      table = Table.singleton c
      newArch =
        Archetype
          { archetypeIdSet = ComponentIDSet (Set.singleton cId),
            archetypeTable = table,
            archetypeAdd = Map.empty,
            archetypeRemove = Map.empty
          }
   in w
        { archetypes = Map.insert archId newArch (archetypes w),
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
  let arch = (archetypes w) Map.! archId
  Table.lookupDyn tableId colId (archetypeTable arch)

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
despawnArch arch tableId =
  let (col, t') = Table.removeCol tableId (archetypeTable arch)
   in (col, arch {archetypeTable = t'})

-- | Remove a component from an `Entity` with its `ComponentID`.
removeWithId :: Entity -> ComponentID -> Archetypes -> Maybe (Dynamic, Archetypes)
removeWithId e cId archs = do
  record <- Map.lookup e (entities archs)
  let archId = recordArchetypeId record
      arch = archetypes archs Map.! archId
  (dyn, table') <- removeWithId' archId (recordTableId record) archs cId (archetypeTable arch)
  let archetypes' = Map.insert archId (arch {archetypeTable = table'}) (archetypes archs)
  return (dyn, archs {archetypes = archetypes'})

removeWithId' :: ArchetypeID -> TableID -> Archetypes -> ComponentID -> Table -> Maybe (Dynamic, Table)
removeWithId' archId tId w cId t = do
  let cState = componentStates w Map.! cId
  colId <- Map.lookup archId (componentColumnIds cState)
  Table.removeDyn tId colId t
