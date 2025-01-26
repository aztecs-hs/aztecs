{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World
  ( ArchetypeID (..),
    World (..),
    empty,
    spawn,
    spawnWithId,
    spawnWithArchetypeId,
    spawnEmpty,
    insert,
    insertWithId,
    insertArchetype,
    despawn,
    Node (..),
    lookupNode,
    lookupArchetypes,
    mapArchetypes,
  )
where

import Data.Aztecs.Component
  ( Component (..),
    ComponentID,
  )
import Data.Aztecs.Entity (EntityID (..))
import Data.Aztecs.World.Archetype (Archetype (..))
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Components (Components (..))
import qualified Data.Aztecs.World.Components as CS
import Data.Dynamic (Dynamic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Proxy (..), Typeable, typeOf)

newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

data Node = Node
  { nodeArchetype :: Archetype,
    nodeAdd :: Map ComponentID ArchetypeID,
    nodeRemove :: Map ComponentID ArchetypeID
  }
  deriving (Show)

-- | World of entities and their components.
data World = World
  { archetypes :: Map ArchetypeID Node,
    archetypeIds :: Map (Set ComponentID) ArchetypeID,
    archetypeComponents :: Map ArchetypeID (Set ComponentID),
    nextArchetypeId :: ArchetypeID,
    components :: Components,
    entities :: Map EntityID ArchetypeID,
    nextEntityId :: EntityID
  }
  deriving (Show)

-- | Empty `World`.
empty :: World
empty =
  World
    { archetypes = mempty,
      archetypeIds = mempty,
      archetypeComponents = mempty,
      nextArchetypeId = ArchetypeID 0,
      components = CS.empty,
      entities = mempty,
      nextEntityId = EntityID 0
    }

-- | Spawn an entity with a component.
spawn :: forall a. (Component a, Typeable (StorageT a)) => a -> World -> (EntityID, World)
spawn c w = case Map.lookup (typeOf (Proxy @a)) (componentIds (components w)) of
  Just cId -> spawnWithId cId c w
  Nothing ->
    let (cId, cs) = CS.insert @a (components w)
     in spawnWithId cId c w {components = cs}

-- | Spawn an empty entity.
spawnEmpty :: World -> (EntityID, World)
spawnEmpty w = let e = nextEntityId w in (e, w {nextEntityId = EntityID (unEntityId e + 1)})

-- | Spawn an entity with a component and its `ComponentID`.
spawnWithId ::
  forall a.
  (Component a, Typeable (StorageT a)) =>
  ComponentID ->
  a ->
  World ->
  (EntityID, World)
spawnWithId cId c w =
  let (e, w') = spawnEmpty w
   in case Map.lookup (Set.singleton cId) (archetypeIds w) of
        Just aId -> (e, spawnWithArchetypeId' e aId cId c w')
        Nothing ->
          ( e,
            snd $
              insertArchetype
                (Set.singleton cId)
                ( Node
                    { nodeArchetype = A.insert e cId c A.empty,
                      nodeAdd = Map.empty,
                      nodeRemove = Map.empty
                    }
                )
                w' {entities = Map.insert e (nextArchetypeId w) (entities w)}
          )

-- | Spawn an entity with a component and its `ComponentID` directly into an archetype.
spawnWithArchetypeId ::
  forall a.
  (Component a, Typeable (StorageT a)) =>
  a ->
  ComponentID ->
  ArchetypeID ->
  World ->
  (EntityID, World)
spawnWithArchetypeId c cId aId w =
  let (e, w') = spawnEmpty w
   in (e, spawnWithArchetypeId' e aId cId c w')

spawnWithArchetypeId' ::
  forall a.
  (Component a, Typeable (StorageT a)) =>
  EntityID ->
  ArchetypeID ->
  ComponentID ->
  a ->
  World ->
  World
spawnWithArchetypeId' e aId cId c w =
  let f n = n {nodeArchetype = A.insert e cId c (nodeArchetype n)}
   in w
        { archetypes = Map.adjust f aId (archetypes w),
          entities = Map.insert e aId (entities w)
        }

-- | Insert an archetype by its set of `ComponentID`s.
insertArchetype :: Set ComponentID -> Node -> World -> (ArchetypeID, World)
insertArchetype cIds n w =
  let aId = nextArchetypeId w
   in ( aId,
        w
          { archetypes = Map.insert aId n (archetypes w),
            archetypeIds = Map.insert cIds aId (archetypeIds w),
            archetypeComponents = Map.insert aId cIds (archetypeComponents w),
            nextArchetypeId = ArchetypeID (unArchetypeId aId + 1)
          }
      )

-- | Insert a component into an entity.
insert :: forall a. (Component a, Typeable (StorageT a)) => EntityID -> a -> World -> World
insert e c w =
  let (cId, components') = CS.insert @a (components w)
   in insertWithId e cId c w {components = components'}

-- | Insert a component into an entity with its `ComponentID`.
insertWithId :: (Component a, Typeable (StorageT a)) => EntityID -> ComponentID -> a -> World -> World
insertWithId e cId c w = case Map.lookup e (entities w) of
  Just aId -> case Map.lookup aId (archetypeComponents w) of
    Just cIds ->
      if Set.member cId cIds
        then w {archetypes = Map.adjust (\n -> n {nodeArchetype = A.insert e cId c (nodeArchetype n)}) aId (archetypes w)}
        else
          let node = archetypes w Map.! aId
           in case Map.lookup (Set.insert cId cIds) (archetypeIds w) of
                Just nextAId ->
                  let (cs, arch') = A.remove e (nodeArchetype node)
                      w' = w {archetypes = Map.insert aId node {nodeArchetype = arch'} (archetypes w)}
                      f (itemCId, dyn) archAcc =
                        archAcc
                          { A.storages =
                              Map.adjust
                                (\s -> s {A.storageDyn = A.insertDyn s (unEntityId e) dyn (A.storageDyn s)})
                                itemCId
                                (A.storages archAcc)
                          }
                   in w'
                        { archetypes =
                            Map.adjust
                              ( \nextNode ->
                                  nextNode
                                    { nodeArchetype =
                                        A.insert e cId c $
                                          foldr
                                            f
                                            (nodeArchetype nextNode)
                                            (Map.toList cs)
                                    }
                              )
                              nextAId
                              (archetypes w')
                        }
                Nothing ->
                  let (s, arch') = A.removeStorages e (nodeArchetype node)
                      n =
                        Node
                          { nodeArchetype = A.insert e cId c (Archetype {A.storages = s}),
                            nodeAdd = Map.empty,
                            nodeRemove = Map.singleton cId aId
                          }
                      (nextAId, w') = insertArchetype (Set.insert cId cIds) n w
                   in w'
                        { archetypes =
                            Map.insert
                              aId
                              node
                                { nodeArchetype = arch',
                                  nodeAdd = Map.insert cId nextAId (nodeAdd node)
                                }
                              (archetypes w')
                        }
    Nothing -> w
  Nothing -> w

-- | Despawn an entity, returning its components.
despawn :: EntityID -> World -> (Map ComponentID Dynamic, World)
despawn e w =
  let res = do
        aId <- Map.lookup e (entities w)
        arch <- Map.lookup aId (archetypes w)
        return (aId, arch)
   in case res of
        Just (aId, node) ->
          let (dynAcc, arch') = A.remove e (nodeArchetype node)
           in ( dynAcc,
                w
                  { archetypes = Map.insert aId node {nodeArchetype = arch'} (archetypes w),
                    entities = Map.delete e (entities w)
                  }
              )
        Nothing -> (Map.empty, w)

lookupNode :: ArchetypeID -> World -> Maybe Node
lookupNode aId w = Map.lookup aId (archetypes w)

lookupArchetypes :: ArchetypeID -> World -> [Archetype]
lookupArchetypes aId w = case lookupNode aId w of
  Just n -> nodeArchetype n : concatMap (`lookupArchetypes` w) (Map.elems (nodeAdd n))
  Nothing -> []

mapArchetypes :: ArchetypeID -> (Archetype -> (a, Archetype)) -> World -> ([a], World)
mapArchetypes aId f w = fromMaybe ([], w) $ do
  node <- lookupNode aId w
  let next = Map.elems (nodeAdd node)
      (a, arch) = f (nodeArchetype node)
      node' = node {nodeArchetype = arch}
      w' = w {archetypes = Map.insert aId node' (archetypes w)}
  return $
    foldr
      ( \aId' (acc, wAcc) ->
          let (as, wAcc') = mapArchetypes aId' f wAcc in (as ++ acc, wAcc')
      )
      ([a], w')
      next
