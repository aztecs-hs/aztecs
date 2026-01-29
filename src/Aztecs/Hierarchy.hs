{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    Hierarchy (..),
    toList,
    foldWithKey,
    mapWithKey,
    mapWithAccum,
    hierarchy,
    hierarchies,
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Applicative (liftA3)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import GHC.Generics

-- | Parent component.
newtype Parent = Parent
  { -- | Parent entity ID.
    unParent :: EntityID
  }
  deriving (Eq, Ord, Show, Generic)

instance (Monad m) => Component m Parent where
  componentOnInsert e (Parent parent) = do
    -- Add this entity to the parent's children.
    maybeChildren <- A.lookup parent
    let parentChildren = maybe mempty unChildren maybeChildren
    A.insertUntracked parent . bundle . Children $ Set.insert e parentChildren

  componentOnChange e (Parent oldParent) (Parent newParent) = do
    -- Remove this entity from the old parent's children.
    maybeOldChildren <- A.lookup oldParent
    let oldChildren = maybe mempty unChildren maybeOldChildren
    let oldChildren' = Set.filter (/= e) oldChildren
    A.insertUntracked oldParent . bundle . Children $ oldChildren'

    -- Add this entity to the new parent's children.
    maybeNewChildren <- A.lookup newParent
    let newChildren = maybe mempty unChildren maybeNewChildren
    A.insertUntracked newParent . bundle . Children $ Set.insert e newChildren

  componentOnRemove e (Parent parent) = do
    -- Remove this entity from the parent's children.
    maybeChildren <- A.lookup parent
    let parentChildren = maybe mempty unChildren maybeChildren
    let parentChildren' = Set.filter (/= e) parentChildren
    A.insertUntracked parent . bundle . Children $ parentChildren'

-- | Children component.
newtype Children = Children {unChildren :: Set EntityID}
  deriving (Eq, Ord, Show, Semigroup, Monoid, Generic)

instance (Monad m) => Component m Children where
  componentOnInsert e (Children cs) = do
    -- Set parent on all children.
    mapM_ (\child -> A.insertUntracked child . bundle $ Parent e) cs

  componentOnChange e (Children oldCs) (Children newCs) = do
    let added = Set.difference newCs oldCs
        removed = Set.difference oldCs newCs
    -- Set parent on added children.
    mapM_ (\child -> A.insertUntracked child . bundle $ Parent e) added
    -- Remove parent from removed children.
    mapM_ (\child -> A.remove @_ @Parent child) removed

  componentOnRemove _ (Children cs) = do
    -- Remove parent from all children.
    mapM_ (\child -> A.remove @_ @Parent child) cs

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
  forall m a.
  (Monad m) =>
  EntityID ->
  (forall f. (Applicative f) => Query f m (f a)) ->
  Access m (Maybe (Hierarchy a))
hierarchy e q = do
  let mkQuery :: forall f. (Applicative f) => Query f m (f (EntityID, (Set EntityID, a)))
      mkQuery = do
        e' <- Q.entity
        cs <- Q.query @_ @_ @Children
        a' <- q
        return $ liftA3 (\eid c a'' -> (eid, (unChildren c, a''))) e' cs a'
  children <- A.system $ S.readQuery mkQuery
  let childMap = Map.fromList $ V.toList children
  return $ hierarchy' e childMap

-- | Build all hierarchies of parents to children, joined with the given query.
hierarchies ::
  forall m a.
  (Monad m) =>
  (forall f. (Applicative f) => Query f m (f a)) ->
  Access m (Vector (Hierarchy a))
hierarchies q = do
  let mkQuery :: forall f. (Applicative f) => Query f m (f (EntityID, (Set EntityID, a)))
      mkQuery = do
        e' <- Q.entity
        cs <- Q.query @_ @_ @Children
        a' <- q
        return $ liftA3 (\eid c a -> (eid, (unChildren c, a))) e' cs a'
  children <- A.system $ S.readQuery mkQuery
  let childMap = Map.fromList $ V.toList children
  roots <- A.system $ S.readQueryFiltered (Q.entity) (with @m @Children <> without @m @Parent)
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
