{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.Asset.AssetServer
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Hierarchical relationships.
-- A `Children` component forms a one-to-many relationship with `Parent` components.
module Aztecs.Hierarchy
  ( Parent (..),
    Children (..),
    update,
    Hierarchy (..),
    toList,
    foldWithKey,
    mapWithKey,
    mapWithAccum,
    hierarchy,
    hierarchies,
    ParentState (..),
    ChildState (..),
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Monad
import Control.Monad.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics

-- | Parent component.
newtype Parent = Parent
  { -- | Parent entity ID.
    unParent :: EntityID
  }
  deriving (Eq, Ord, Show, Generic)

instance Component Identity Parent

-- | Parent internal state component.
newtype ParentState = ParentState {unParentState :: EntityID}
  deriving (Show, Generic)

instance Component Identity ParentState

-- | Children component.
newtype Children = Children {unChildren :: Set EntityID}
  deriving (Eq, Ord, Show, Semigroup, Monoid, Generic)

instance Component Identity Children

-- | Child internal state component.
newtype ChildState = ChildState {unChildState :: Set EntityID}
  deriving (Show, Generic)

instance Component Identity ChildState

-- | Update the parent-child relationships.
update :: Access ()
update = do
  parents <- A.system . S.readQuery $ do
    entity <- Q.entity
    parent <- Q.fetch
    maybeParentState <- Q.fetchMaybe @_ @ParentState
    return (entity, unParent parent, maybeParentState)

  children <- A.system . S.readQuery $ do
    entity <- Q.entity
    cs <- Q.fetch
    maybeChildState <- Q.fetchMaybe @_ @ChildState
    return (entity, unChildren cs, maybeChildState)

  let go = do
        mapM_
          ( \(entity, parent, maybeParentState) -> case maybeParentState of
              Just (ParentState parentState) -> do
                when (parent /= parentState) $ do
                  A.insert parent . bundle $ ParentState parent

                  -- Remove this entity from the previous parent's children.
                  maybeLastChildren <- A.lookup parentState
                  let lastChildren = maybe mempty unChildren maybeLastChildren
                  let lastChildren' = Set.filter (/= entity) lastChildren
                  A.insert parentState . bundle . Children $ lastChildren'

                  -- Add this entity to the new parent's children.
                  maybeChildren <- A.lookup parent
                  let parentChildren = maybe mempty unChildren maybeChildren
                  A.insert parent . bundle . Children $ Set.insert entity parentChildren
              Nothing -> do
                A.spawn_ . bundle $ ParentState parent
                maybeChildren <- A.lookup parent
                let parentChildren = maybe mempty unChildren maybeChildren
                A.insert parent . bundle . Children $ Set.insert entity parentChildren
          )
          parents
        mapM_
          ( \(entity, children', maybeChildState) -> case maybeChildState of
              Just (ChildState childState) -> do
                when (children' /= childState) $ do
                  A.insert entity . bundle $ ChildState children'
                  let added = Set.difference children' childState
                      removed = Set.difference childState children'
                  mapM_ (\e -> A.insert e . bundle . Parent $ entity) added
                  mapM_ (A.remove @_ @Parent) removed
              Nothing -> do
                A.insert entity . bundle $ ChildState children'
                mapM_ (\e -> A.insert e . bundle . Parent $ entity) children'
          )
          children
  go

-- | Hierarchy of entities.
data Hierarchy a = Node
  { -- | Entity ID.
    nodeEntityId :: EntityID,
    -- | Entity components.
    nodeEntity :: a,
    -- | Child nodes.
    nodeChildren :: [Hierarchy a]
  }
  deriving (Functor)

instance Foldable Hierarchy where
  foldMap f n = f (nodeEntity n) <> foldMap (foldMap f) (nodeChildren n)

instance Traversable Hierarchy where
  traverse f n =
    Node (nodeEntityId n) <$> f (nodeEntity n) <*> traverse (traverse f) (nodeChildren n)

-- | Convert a hierarchy to a vector of entity IDs and components.
toList :: Hierarchy a -> Vector (EntityID, a)
toList n = V.singleton (nodeEntityId n, nodeEntity n) <> V.concatMap toList (V.fromList $ nodeChildren n)

-- | Fold a hierarchy with a function that takes the entity ID, entity, and accumulator.
foldWithKey :: (EntityID -> a -> b -> b) -> Hierarchy a -> b -> b
foldWithKey f n b = f (nodeEntityId n) (nodeEntity n) (foldr (foldWithKey f) b (nodeChildren n))

-- | Map a hierarchy with a function that takes the entity ID and entity.
mapWithKey :: (EntityID -> a -> b) -> Hierarchy a -> Hierarchy b
mapWithKey f n =
  Node (nodeEntityId n) (f (nodeEntityId n) (nodeEntity n)) (map (mapWithKey f) (nodeChildren n))

-- | Map a hierarchy with a function that takes the entity ID, entity, and accumulator.
mapWithAccum :: (EntityID -> a -> b -> (c, b)) -> b -> Hierarchy a -> Hierarchy c
mapWithAccum f b n = case f (nodeEntityId n) (nodeEntity n) b of
  (c, b') -> Node (nodeEntityId n) c (map (mapWithAccum f b') (nodeChildren n))

-- | System to read a hierarchy of parents to children with the given query.
hierarchy ::
  EntityID ->
  Query a ->
  Access (Maybe (Hierarchy a))
hierarchy e q = do
  children <- A.system . S.readQuery $ do
    entity <- Q.entity
    cs <- Q.fetch
    a <- q
    return (entity, (unChildren cs, a))

  let childMap = Map.fromList $ V.toList children
  return $ hierarchy' e childMap

-- | Build all hierarchies of parents to children, joined with the given query.
hierarchies ::
  Query a ->
  Access (Vector (Hierarchy a))
hierarchies q = do
  children <-
    A.system . S.readQuery $ do
      entity <- Q.entity
      cs <- Q.fetch
      a <- q
      return (entity, (unChildren cs, a))

  let childMap = Map.fromList $ V.toList children
  roots <- A.system $ S.readQueryFiltered Q.entity (with @Identity @Children <> without @Identity @Parent)
  return $ V.mapMaybe (`hierarchy'` childMap) roots

-- | Build a hierarchy of parents to children.
hierarchy' :: EntityID -> Map EntityID (Set EntityID, a) -> Maybe (Hierarchy a)
hierarchy' e childMap = case Map.lookup e childMap of
  Just (cs, a) ->
    let bs = mapMaybe (`hierarchy'` childMap) (Set.toList cs)
     in Just
          Node
            { nodeEntityId = e,
              nodeEntity = a,
              nodeChildren = bs
            }
  Nothing -> Nothing
