{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Access
  ( Access (..),
    runAccess,
    spawn,
    spawn_,
    insert,
    insertWithId,
    despawn,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState (..), StateT (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Aztecs.Component (Component (..), ComponentID)
import Data.Aztecs.Entity (EntityID (..))
import Data.Aztecs.View (View (..))
import Data.Aztecs.World (World (..), spawnEmpty)
import Data.Aztecs.World.Archetype (Archetype (..), Bundle (..), DynamicBundle (..))
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Node (..))
import qualified Data.Aztecs.World.Archetypes as AS
import qualified Data.Aztecs.World.Components as CS
import Data.Data (Typeable)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Prelude hiding (all, lookup, map)

-- | Access into the `World`.
newtype Access m a = Access {unAccess :: WriterT View (StateT World m) a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccess :: (Functor m) => Access m a -> World -> m (a, View, World)
runAccess a w = fmap (\((x, v), w') -> (x, v, w')) (runStateT (runWriterT $ unAccess a) w)

-- | Spawn an entity with a component.
spawn ::
  (Monad m) =>
  Bundle ->
  Access m EntityID
spawn b = Access $ do
  w <- get
  let (eId, w') = spawnEmpty w
      (cIds, components', dynB) = unBundle b (components w)
      (w'', v) = case AS.lookupArchetypeId cIds (archetypes w) of
        Just aId -> (fromMaybe (w', mempty)) $ do
          node <- AS.lookupNode aId (archetypes w)
          let arch' = runDynamicBundle dynB eId (nodeArchetype node)
              node' = node {nodeArchetype = arch'}
          return
            ( w
                { archetypes = (archetypes w) {AS.nodes = Map.insert aId node' {AS.nodeArchetype = arch'} (AS.nodes $ archetypes w)},
                  components = components',
                  entities = Map.insert eId aId (entities w)
                },
              View $ Map.singleton aId node'
            )
        Nothing ->
          let arch' = runDynamicBundle dynB eId A.empty
              node =
                Node
                  { nodeComponentIds = cIds,
                    nodeArchetype = arch',
                    nodeAdd = Map.empty,
                    nodeRemove = Map.empty
                  }
              (aId, arches) = AS.insertArchetype cIds node (archetypes w')
           in ( w'
                  { archetypes = arches,
                    entities = Map.insert eId aId (entities w),
                    components = components'
                  },
                View $ Map.singleton aId node
              )
  put w''
  tell v
  return eId

spawn_ :: (Monad m) => Bundle -> Access m ()
spawn_ c = do
  _ <- spawn c
  return ()

insert :: forall m a. (Monad m, Component a, Typeable (StorageT a)) => EntityID -> a -> Access m ()
insert eId a = do
  cId <- Access $ do
    w <- get
    let (cId, cs) = CS.insert @a (components w)
    put w {components = cs}
    return cId
  insertWithId eId cId a

-- | Insert a component into an entity with its `ComponentID`.
insertWithId :: (Monad m, Component a, Typeable (StorageT a)) => EntityID -> ComponentID -> a -> Access m ()
insertWithId e cId c = Access $ do
  w <- get
  let (w', v) = case Map.lookup e (entities w) of
        Just aId -> case AS.lookupNode aId (archetypes w) of
          Just node ->
            if Set.member cId (nodeComponentIds node)
              then
                let arch = A.insertComponent e cId c (nodeArchetype node)
                    node' = node {nodeArchetype = arch}
                 in ( w
                        { archetypes =
                            (archetypes w)
                              { AS.nodes = Map.insert aId node' (AS.nodes $ archetypes w)
                              }
                        },
                      View $ Map.singleton aId node'
                    )
              else case AS.lookupArchetypeId (Set.insert cId (nodeComponentIds node)) (archetypes w) of
                Just nextAId ->
                  let (cs, arch') = A.remove e (nodeArchetype node)
                      w'' = w {archetypes = (archetypes w) {AS.nodes = Map.insert aId node {nodeArchetype = arch'} (AS.nodes $ archetypes w)}}
                      f (itemCId, dyn) archAcc =
                        archAcc
                          { A.storages =
                              Map.adjust
                                (\s -> s {A.storageDyn = A.insertDyn s (unEntityId e) dyn (A.storageDyn s)})
                                itemCId
                                (A.storages archAcc)
                          }
                      nextNode = (AS.nodes $ archetypes w'') Map.! nextAId
                      nextNode' =
                        nextNode
                          { nodeArchetype =
                              A.insertComponent e cId c $
                                foldr
                                  f
                                  (nodeArchetype nextNode)
                                  (Map.toList cs)
                          }
                   in ( w''
                          { archetypes =
                              (archetypes w'')
                                { AS.nodes =
                                    Map.insert nextAId nextNode' (AS.nodes $ archetypes w'')
                                },
                            entities = Map.insert e nextAId (entities w)
                          },
                        View $ Map.singleton nextAId nextNode'
                      )
                Nothing ->
                  let (s, arch') = A.removeStorages e (nodeArchetype node)
                      n =
                        Node
                          { nodeComponentIds = Set.insert cId (nodeComponentIds node),
                            nodeArchetype = A.insertComponent e cId c (Archetype {A.storages = s}),
                            nodeAdd = Map.empty,
                            nodeRemove = Map.singleton cId aId
                          }
                      (nextAId, arches) = AS.insertArchetype (Set.insert cId (nodeComponentIds node)) n (archetypes w)
                      node' =
                        node
                          { nodeArchetype = arch',
                            nodeAdd = Map.insert cId nextAId (nodeAdd node)
                          }
                   in ( w
                          { archetypes =
                              arches
                                { AS.nodes = Map.insert aId node' (AS.nodes arches)
                                },
                            entities = Map.insert e nextAId (entities w)
                          },
                        View $ Map.singleton nextAId (node')
                      )
          Nothing -> (w, mempty)
        Nothing -> (w, mempty)
  tell v
  put w'

despawn :: (Monad m) => EntityID -> Access m ()
despawn e = Access $ do
  w <- get
  let res = do
        aId <- Map.lookup e (entities w)
        node <- AS.lookupNode aId (archetypes w)
        return (aId, node)
      (w', v) = case res of
        Just (aId, node) ->
          let (_, arch') = A.remove e (nodeArchetype node)
              node' = node {nodeArchetype = arch'}
           in ( w
                  { archetypes = (archetypes w) {AS.nodes = Map.insert aId node' (AS.nodes $ archetypes w)},
                    entities = Map.delete e (entities w)
                  },
                View $ Map.singleton aId node'
              )
        Nothing -> (w, mempty)
  put w'
  tell v
  return ()