{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Aztecs.ECS.Access
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Access
  ( Access,
    AccessT (..),
    MonadAccess (..),
    runAccessT,
    runAccessT_,
    all,
    allM,
    filter,
    filterM,
    map,
    mapM,
    mapSingleMaybe,
    mapSingleMaybeM,
    filterMap,
    filterMapM,
    allDyn,
    allDynM,
    filterDyn,
    filterDynM,
    mapDyn,
    mapDynM,
    mapSingleMaybeDyn,
    mapSingleMaybeDynM,
    filterMapDyn,
    filterMapDynM,
  )
where

import Aztecs.ECS.Access.Class
import Aztecs.ECS.Component
import Aztecs.ECS.Query (Query, QueryFilter (..), QueryT (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQuery, DynamicQueryFilter (..), DynamicQueryT)
import qualified Aztecs.ECS.Query.Dynamic as Q
import Aztecs.ECS.World (World (..))
import qualified Aztecs.ECS.World as W
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Bundle
import Aztecs.ECS.World.Entities (Entities)
import qualified Aztecs.ECS.World.Entities as E
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Foldable as F
import Data.Set (Set)
import Data.Vector (Vector)
import Prelude hiding (all, filter, map, mapM)

type Access = AccessT Identity

-- | Access into the `World`.
newtype AccessT m a = AccessT {unAccessT :: StateT World m a}
  deriving (Functor, Applicative, MonadFix, MonadIO, MonadTrans)

instance (Monad m) => Monad (AccessT m) where
  a >>= f = AccessT $ do
    !w <- get
    (a', w') <- lift $ runAccessT a w
    put w'
    unAccessT $ f a'

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccessT :: (Functor m) => AccessT m a -> World -> m (a, World)
runAccessT a = runStateT $ unAccessT a

-- | Run an `Access` on an empty `World`.
runAccessT_ :: (Functor m) => AccessT m a -> m a
runAccessT_ a = fmap fst . runAccessT a $ W.empty

instance (Monad m) => MonadAccess Bundle (AccessT m) where
  spawn b = AccessT $ do
    !w <- get
    let !(e, w') = W.spawn b w
    put w'
    return e
  insert e c = AccessT $ do
    !w <- get
    let !w' = W.insert e c w
    put w'
  lookup e = AccessT $ do
    !w <- get
    return $ W.lookup e w
  remove e = AccessT $ do
    !w <- get
    let !(a, w') = W.remove e w
    put w'
    return a
  despawn e = AccessT $ do
    !w <- get
    let !(_, w') = W.despawn e w
    put w'

all :: (Monad m) => Query a -> AccessT m (Vector a)
all q = AccessT $ do
  w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
  put w {entities = (entities w) {E.components = cs}}
  unAccessT $ allDyn (Q.reads rws <> Q.writes rws) dynQ

allM :: (Monad m) => QueryT m a -> AccessT m (Vector a)
allM q = AccessT $ do
  w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
  put w {entities = (entities w) {E.components = cs}}
  unAccessT $ allDynM (Q.reads rws <> Q.writes rws) dynQ

filter :: (Monad m) => Query a -> QueryFilter -> AccessT m (Vector a)
filter q f = AccessT $ do
  !w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
      (dynF, _) = runQueryFilter f cs
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  return . Q.filterDyn (Q.reads rws <> Q.writes rws) dynQ f' $ entities w

filterM :: (Monad m) => QueryT m a -> QueryFilter -> AccessT m (Vector a)
filterM q f = AccessT $ do
  w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
      (dynF, cs') = runQueryFilter f cs
  put w {entities = (entities w) {E.components = cs'}}
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  unAccessT $ filterDynM (Q.reads rws <> Q.writes rws) dynQ f'

map :: (Monad m) => Query a -> AccessT m (Vector a)
map q = AccessT $ do
  !w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
      (as, es') = Q.mapDyn (Q.reads rws <> Q.writes rws) dynQ $ entities w
  put w {entities = es' {E.components = cs}}
  return as

mapM :: (Monad m) => QueryT m a -> AccessT m (Vector a)
mapM q = AccessT $ do
  !w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
  put w {entities = (entities w) {E.components = cs}}
  unAccessT $ mapDynM (Q.reads rws <> Q.writes rws) dynQ

mapSingleMaybe :: (Monad m) => QueryT m a -> AccessT m (Maybe a)
mapSingleMaybe q = AccessT $ do
  !w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
  put w {entities = (entities w) {E.components = cs}}
  unAccessT $ mapSingleMaybeDynM (Q.reads rws <> Q.writes rws) dynQ

mapSingleMaybeM :: (Monad m) => QueryT m a -> AccessT m (Maybe a)
mapSingleMaybeM q = AccessT $ do
  !w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
  put w {entities = (entities w) {E.components = cs}}
  unAccessT $ mapSingleMaybeDynM (Q.reads rws <> Q.writes rws) dynQ

filterMap :: (Monad m) => Query a -> QueryFilter -> AccessT m (Vector a)
filterMap q f = AccessT $ do
  !w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
      (dynF, cs') = runQueryFilter f cs
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
      (as, es') = Q.filterMapDyn (Q.reads rws <> Q.writes rws) f' dynQ $ entities w
  put w {entities = es' {E.components = cs'}}
  return as

filterMapM :: (Monad m) => QueryT m a -> QueryFilter -> AccessT m (Vector a)
filterMapM q f = AccessT $ do
  !w <- get
  let (rws, cs, dynQ) = runQuery q . E.components $ entities w
      (dynF, cs') = runQueryFilter f cs
  put w {entities = (entities w) {E.components = cs'}}
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  unAccessT $ filterMapDynM (Q.reads rws <> Q.writes rws) f' dynQ

allDyn :: (Monad m) => Set ComponentID -> DynamicQuery a -> AccessT m (Vector a)
allDyn cIds q = AccessT $ do
  !w <- get
  return $ Q.allDyn cIds q $ entities w

allDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> AccessT m (Vector a)
allDynM cIds q = AccessT $ do
  !w <- get
  lift . Q.allDynM cIds q $ entities w

filterDyn :: (Monad m) => Set ComponentID -> DynamicQuery a -> (Node -> Bool) -> AccessT m (Vector a)
filterDyn cIds q f = AccessT $ do
  !w <- get
  return $ Q.filterDyn cIds q f $ entities w

filterDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> (Node -> Bool) -> AccessT m (Vector a)
filterDynM cIds q f = AccessT $ do
  !w <- get
  lift . Q.filterDynM cIds f q $ entities w

mapDyn :: (Monad m) => Set ComponentID -> DynamicQuery a -> AccessT m (Vector a, Entities)
mapDyn cIds q = AccessT $ do
  !w <- get
  return $ Q.mapDyn cIds q $ entities w

mapDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> AccessT m (Vector a)
mapDynM cIds q = AccessT $ do
  !w <- get
  (as, es) <- lift . Q.mapDynM cIds q $ entities w
  put w {entities = es}
  return as

mapSingleMaybeDyn :: (Monad m) => Set ComponentID -> DynamicQuery a -> AccessT m (Maybe a)
mapSingleMaybeDyn cIds q = AccessT $ do
  !w <- get
  let (res, es) = Q.mapSingleMaybeDyn cIds q $ entities w
  put w {entities = es}
  return res

mapSingleMaybeDynM :: (Monad m) => Set ComponentID -> DynamicQueryT m a -> AccessT m (Maybe a)
mapSingleMaybeDynM cIds q = AccessT $ do
  !w <- get
  (res, es) <- lift . Q.mapSingleMaybeDynM cIds q $ entities w
  put w {entities = es}
  return res

filterMapDyn :: (Monad m) => Set ComponentID -> (Node -> Bool) -> DynamicQuery a -> AccessT m (Vector a, Entities)
filterMapDyn cIds f q = AccessT $ do
  !w <- get
  return $ Q.filterMapDyn cIds f q $ entities w

filterMapDynM :: (Monad m) => Set ComponentID -> (Node -> Bool) -> DynamicQueryT m a -> AccessT m (Vector a)
filterMapDynM cIds f q = AccessT $ do
  !w <- get
  (as, es) <- lift . Q.filterMapDynM cIds f q $ entities w
  put w {entities = es}
  return as
