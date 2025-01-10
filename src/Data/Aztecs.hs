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
    insertId,
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

data Archetype = Archetype ComponentIDSet Table deriving (Show)

newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

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

data World = World
  { archetypes :: Map ArchetypeID Archetype,
    archetypeIds :: Map ComponentIDSet ArchetypeID,
    nextArchetypeId :: ArchetypeID,
    componentIds :: Map TypeRep ComponentID,
    componentStates :: Map ComponentID ComponentState,
    nextComponentId :: ComponentID,
    entities :: Map Entity EntityRecord,
    nextEntity :: Entity
  }
  deriving (Show)

empty :: World
empty =
  World
    { archetypes = Map.empty,
      archetypeIds = Map.empty,
      nextArchetypeId = ArchetypeID 0,
      componentIds = Map.empty,
      componentStates = Map.empty,
      nextComponentId = ComponentID 0,
      entities = Map.empty,
      nextEntity = Entity 0
    }

insertId :: forall c. (Typeable c) => World -> (ComponentID, World)
insertId w = case Map.lookup (typeOf (Proxy @c)) (componentIds w) of
  Just cId -> (cId, w)
  Nothing ->
    let cId = nextComponentId w
        w' =
          w
            { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds w),
              nextComponentId = ComponentID (unComponentId cId + 1)
            }
     in (cId, w')

spawn :: forall c. (Typeable c) => c -> World -> (Entity, World)
spawn c w = case Map.lookup (typeOf (Proxy @c)) (componentIds w) of
  Just cId -> spawnWithId cId c w
  Nothing ->
    let cId = nextComponentId w
        w' =
          w
            { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds w),
              nextComponentId = ComponentID (unComponentId cId + 1)
            }
        e = nextEntity w'
     in (e, insertNewComponent e cId c (w' {nextEntity = Entity (unEntity e + 1)}))

spawnWithId :: (Typeable c) => ComponentID -> c -> World -> (Entity, World)
spawnWithId cId c w = do
  let e = nextEntity w
      w' = insertNew e cId c (w {nextEntity = Entity (unEntity e + 1)})
   in (e, w')

insert :: forall c. (Typeable c) => Entity -> c -> World -> World
insert e c w = case Map.lookup (typeOf (Proxy @c)) (componentIds w) of
  Just cId -> insertWithId e cId c w
  Nothing -> case Map.lookup e (entities w) of
    Just record ->
      let arch@(Archetype (ComponentIDSet idSet) table) = archetypes w Map.! (recordArchetypeId record)
          w' = despawnRecord arch record w
          cId = nextComponentId w'
          idSet' = ComponentIDSet $ Set.insert cId idSet
       in case Map.lookup idSet' (archetypeIds w') of
            Just archId -> error "TODO"
            Nothing ->
              let archId = nextArchetypeId w'
                  table' = Table.cons c table
                  f tId colId t = fromMaybe t $ snd <$> Table.remove @c tId colId t
                  g (i, idx) acc = Map.insert i (ComponentState (Map.singleton archId (ColumnID idx)) f) acc
               in w'
                    { archetypes = Map.insert archId (Archetype idSet' table') (archetypes w'),
                      archetypeIds = Map.insert idSet' archId (archetypeIds w'),
                      nextArchetypeId = ArchetypeID (unArchetypeId archId + 1),
                      entities = Map.insert e (EntityRecord archId (TableID $ Table.length table' - 1)) (entities w'),
                      componentStates =
                        foldr g (componentStates w) (zip (reverse $ Set.toList $ unComponentIdSet idSet') [0..])
                    }
    Nothing -> insertNewComponent e (nextComponentId w) c (w {nextComponentId = ComponentID (unComponentId (nextComponentId w) + 1)})

insertWithId :: (Typeable c) => Entity -> ComponentID -> c -> World -> World
insertWithId e cId c w = case Map.lookup e (entities w) of
  Just record -> error "TODO"
  Nothing -> insertNew e cId c w

insertNew :: forall c. (Typeable c) => Entity -> ComponentID -> c -> World -> World
insertNew e cId c w = case Map.lookup cId (componentStates w) of
  Just colIds -> error "TODO"
  Nothing -> insertNewComponent e cId c w

insertNewComponent :: forall c. (Typeable c) => Entity -> ComponentID -> c -> World -> World
insertNewComponent e cId c w =
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
          archetypeIds = Map.insert (ComponentIDSet (Set.singleton cId)) archId (archetypeIds w),
          componentStates = componentStates',
          entities = entities',
          nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
        }

lookup :: forall c. (Typeable c) => Entity -> World -> Maybe c
lookup e w = case Map.lookup (typeOf (Proxy @c)) (componentIds w) of
  Just cId -> lookupWithId e cId w
  Nothing -> Nothing

lookupWithId :: (Typeable c) => Entity -> ComponentID -> World -> Maybe c
lookupWithId e cId w = case Map.lookup e (entities w) of
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
        let arch = archetypes w Map.! (recordArchetypeId record)
        return $ despawnRecord arch record w
   in fromMaybe w res

despawnRecord :: Archetype -> EntityRecord -> World -> World
despawnRecord (Archetype (ComponentIDSet cs) table) record w =
  let archId = recordArchetypeId record
      table' = foldr (removeWithId' archId record w) table (Set.toList cs)
      archetypes' = Map.insert archId (Archetype (ComponentIDSet cs) table') (archetypes w)
   in w {archetypes = archetypes'}

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
