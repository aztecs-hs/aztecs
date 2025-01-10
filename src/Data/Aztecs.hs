{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs
  ( Entity (..),
    ArchetypeID (..),
    Archetype (..),
    ComponentID (..),
    ComponentIDSet (..),
    ComponentState (..),
    EntityRecord (..),
    World (..),
    empty,
    spawn,
    spawnWithId,
    insert,
    insertWithId,
    lookup,
    lookupWithId,
    remove,
    removeWithId,
    despawn,
  )
where

import Data.Aztecs.Components (ComponentID (..), Components)
import qualified Data.Aztecs.Components as CS
import Data.Aztecs.Table (ColumnID (ColumnID), Table, TableID (..))
import qualified Data.Aztecs.Table as Table
import Data.Data (Typeable)
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
  { componentColumnIds :: (Map ArchetypeID ColumnID),
    removeComponent :: TableID -> ColumnID -> Table -> Table
  }

instance Show ComponentState where
  show (ComponentState cs _) = "ComponentState " ++ show cs

-- | World of entities and components.
data World = World
  { archetypes :: Map ArchetypeID Archetype,
    archetypeIds :: Map ComponentIDSet ArchetypeID,
    nextArchetypeId :: ArchetypeID,
    components :: Components,
    componentStates :: Map ComponentID ComponentState,
    entities :: Map Entity EntityRecord,
    nextEntity :: Entity
  }
  deriving (Show)

-- | Empty world.
empty :: World
empty =
  World
    { archetypes = Map.empty,
      archetypeIds = Map.empty,
      nextArchetypeId = ArchetypeID 0,
      components = CS.empty,
      componentStates = Map.empty,
      entities = Map.empty,
      nextEntity = Entity 0
    }

-- | Spawn an entity with a component.
spawn :: forall c. (Typeable c) => c -> World -> (Entity, World)
spawn c w = case CS.lookup @c (components w) of
  Just cId -> spawnWithId cId c w
  Nothing ->
    let (cId, cs) = CS.insert @c (components w)
        w' = w {components = cs}
        e = nextEntity w'
     in (e, insertNewComponent e cId c (w' {nextEntity = Entity (unEntity e + 1)}))

-- | Spawn an entity with a component and its `ComponentID`.
spawnWithId :: (Typeable c) => ComponentID -> c -> World -> (Entity, World)
spawnWithId cId c w = do
  let e = nextEntity w
      w' = insertNew e cId c (w {nextEntity = Entity (unEntity e + 1)})
   in (e, w')

-- | Insert a component into an `Entity`.
insert :: forall c. (Typeable c) => Entity -> c -> World -> World
insert e c w = case CS.lookup @c (components w) of
  Just cId -> insertWithId e cId c w
  Nothing -> case Map.lookup e (entities w) of
    Just record ->
      let arch@(Archetype (ComponentIDSet idSet) table) = archetypes w Map.! (recordArchetypeId record)
          w' = despawnRecord arch record w
          (cId, cs) = CS.insert @c (components w')
          idSet' = ComponentIDSet $ Set.insert cId idSet
       in case Map.lookup idSet' (archetypeIds w') of
            Just archId -> error "TODO"
            Nothing ->
              let archId = nextArchetypeId w'
                  table' = Table.cons (recordTableId record) c table
                  f tId colId t = fromMaybe t $ snd <$> Table.remove @c tId colId t
                  g (i, idx) acc = Map.insert i (ComponentState (Map.singleton archId (ColumnID idx)) f) acc
               in w'
                    { archetypes = Map.insert archId (Archetype idSet' table') (archetypes w'),
                      archetypeIds = Map.insert idSet' archId (archetypeIds w'),
                      nextArchetypeId = ArchetypeID (unArchetypeId archId + 1),
                      entities = Map.insert e (EntityRecord archId (TableID $ Table.length table' - 1)) (entities w'),
                      components = cs,
                      componentStates =
                        foldr g (componentStates w) (zip (reverse . Set.toList $ unComponentIdSet idSet') [0 ..])
                    }
    Nothing -> error "TODO"

-- | Insert a component into an `Entity` with its `ComponentID`.
insertWithId :: (Typeable c) => Entity -> ComponentID -> c -> World -> World
insertWithId e cId c w = case Map.lookup e (entities w) of
  Just record -> error "TODO"
  Nothing -> insertNew e cId c w

insertNew :: forall c. (Typeable c) => Entity -> ComponentID -> c -> World -> World
insertNew e cId c w = case Map.lookup cId (componentStates w) of
  Just cState ->
    let archId = archetypeIds w Map.! (ComponentIDSet (Set.singleton cId))
        Archetype _ table = archetypes w Map.! archId
        table' = Table.singleton c <> table
        f tId colId t = fromMaybe t $ snd <$> Table.remove @c tId colId t
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
                (ComponentState (Map.insert archId (ColumnID 0) (componentColumnIds cState)) f)
                (componentStates w),
            entities =
              Map.insert
                e
                (EntityRecord archId (TableID (Table.length table' - 1)))
                (entities w),
            nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
          }
  Nothing -> insertNewComponent e cId c w

insertNewComponent :: forall c. (Typeable c) => Entity -> ComponentID -> c -> World -> World
insertNewComponent e cId c w =
  let archId = nextArchetypeId w
      table = Table.singleton c
      f tId colId t = fromMaybe t $ snd <$> Table.remove @c tId colId t
   in w
        { archetypes = Map.insert archId (Archetype (ComponentIDSet (Set.singleton cId)) table) (archetypes w),
          archetypeIds = Map.insert (ComponentIDSet (Set.singleton cId)) archId (archetypeIds w),
          componentStates =
            Map.insert
              cId
              (ComponentState (Map.singleton archId (ColumnID 0)) f)
              (componentStates w),
          entities = Map.insert e (EntityRecord archId (TableID 0)) (entities w),
          nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
        }

-- | Lookup a component in an `Entity`.
lookup :: forall c. (Typeable c) => Entity -> World -> Maybe c
lookup e w = case CS.lookup @c (components w) of
  Just cId -> lookupWithId e cId w
  Nothing -> Nothing

-- | Lookup a component in an `Entity` with its `ComponentID`.
lookupWithId :: (Typeable c) => Entity -> ComponentID -> World -> Maybe c
lookupWithId e cId w = do
  (EntityRecord archId tableId) <- Map.lookup e (entities w)
  cState <- Map.lookup cId (componentStates w)
  colId <- Map.lookup archId (componentColumnIds cState)
  let Archetype _ table = (archetypes w) Map.! archId
  Table.lookup table tableId colId

-- | Despawn an `Entity`.
despawn :: Entity -> World -> World
despawn e w =
  let res = do
        record <- Map.lookup e (entities w)
        let arch = archetypes w Map.! (recordArchetypeId record)
        return $ despawnRecord arch record w
   in fromMaybe w res

despawnRecord :: Archetype -> EntityRecord -> World -> World
despawnRecord (Archetype (ComponentIDSet cs) table) record w =
  let archId = recordArchetypeId record
      table' = foldr (removeWithId' archId record w) table (Set.toList cs)
      archetypes' = Map.insert archId (Archetype (ComponentIDSet cs) table') (archetypes w)
   in w {archetypes = archetypes'}

-- | Remove a component from an `Entity`.
remove :: forall c. (Typeable c) => Entity -> World -> World
remove e w = case CS.lookup @c (components w) of
  Just cId -> removeWithId e cId w
  Nothing -> w

-- | Remove a component from an `Entity` with its `ComponentID`.
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
