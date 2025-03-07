{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Aztecs.ECS.System
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Systems to process queries in parallel.
module Aztecs.ECS.System
  ( System,
    SystemT (..),
    all,
    filter,
    map,
    filterMap,
    mapSingleMaybe,
    mapDyn,
    mapSingleMaybeDyn,
    filterMapDyn,
  )
where

import Aztecs.ECS.Query (QueryFilter (..), QueryT (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQueryFilter (..), DynamicQueryT, readDynQuery, readsWrites)
import qualified Aztecs.ECS.View as V
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Monad.Identity
import Control.Monad.Trans
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Prelude hiding (all, filter, map)

-- | @since 0.9
type System = SystemT Identity

-- | System to process queries in parallel.
--
-- @since 0.9
newtype SystemT m a = SystemT
  { -- | Run a system on a collection of `Entities`.
    --
    -- @since 0.9
    runSystemT :: forall t. (MonadTrans t, Monad (t m)) => ((Entities -> Entities) -> t m Entities) -> t m a
  }
  deriving (Functor)

instance (Monad m) => Applicative (SystemT m) where
  pure a = SystemT $ \_ -> pure a
  (<*>) = ap

instance (Monad m) => Monad (SystemT m) where
  SystemT f >>= g = SystemT $ \h -> do
    a <- f h
    runSystemT (g a) h

-- | @since 0.9
mapDyn :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
mapDyn q = SystemT $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

mapSingleMaybeDyn :: (Monad m) => DynamicQueryT m a -> SystemT m (Maybe a)
mapSingleMaybeDyn q = SystemT $ \f -> do
  w <- f id
  let rws = readsWrites q
  case V.viewSingle (Q.reads rws <> Q.writes rws) $ archetypes w of
    Just v -> do
      (o, v') <- lift $ V.mapSingleDyn q v
      _ <- f $ V.unview v'
      return o
    Nothing -> return Nothing

filterMapDyn :: (Monad m) => (Node -> Bool) -> DynamicQueryT m a -> SystemT m [a]
filterMapDyn qf q = SystemT $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) qf $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

-- | @since 0.9
map :: (Monad m) => QueryT m a -> SystemT m [a]
map q = SystemT $ \f -> do
  dynQ <- runSystemT (fromQuery q) f
  runSystemT (mapDyn dynQ) f

mapSingleMaybe :: (Monad m) => QueryT m a -> SystemT m (Maybe a)
mapSingleMaybe q = SystemT $ \f -> do
  dynQ <- runSystemT (fromQuery q) f
  runSystemT (mapSingleMaybeDyn dynQ) f

filterMap :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m [a]
filterMap q qf = SystemT $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
      (dynF, _) = runQueryFilter qf cs'
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  runSystemT (filterMapDyn f' dynQ) f

all :: (Monad m) => QueryT m a -> SystemT m [a]
all q = SystemT $ \f -> do
  w <- f id
  let (_, dynQ) = runQuery q $ components w
  runSystemT (allDyn dynQ) f

filter :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m [a]
filter q qf = SystemT $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
      (dynF, _) = runQueryFilter qf cs'
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  runSystemT (filterDyn dynQ f') f

allDyn :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
allDyn q = SystemT $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  lift $
    if V.null v
      then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities w}
      else V.allDyn q v

filterDyn :: (Monad m) => DynamicQueryT m a -> (Node -> Bool) -> SystemT m [a]
filterDyn q qf = SystemT $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) qf $ archetypes w
  lift $ V.allDyn q v

-- | Convert a `QueryT` to a `System`.
--
-- @since 0.9
fromQuery :: (Monad m) => QueryT m a -> SystemT m (DynamicQueryT m a)
fromQuery q = SystemT $ \f -> do
  -- TODO race?
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
  _ <- f $ const w {components = cs'}
  return dynQ
