{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
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
  ( -- * Systems
    System,
    SystemT (..),

    -- ** Reading
    all,
    filter,
    allDyn,
    filterDyn,

    -- ** Writing
    map,
    filterMap,
    mapSingleMaybe,
    mapDyn,
    filterMapDyn,
    mapSingleMaybeDyn,

    -- ** Running
    runSystemT,
    concurrently,
  )
where

import Aztecs.ECS.Query (QueryFilter (..), QueryT (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQueryFilter (..), DynamicQueryT, readDynQuery, readsWrites)
import qualified Aztecs.ECS.View as V
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad.Identity
import Control.Monad.Trans
import qualified Data.Foldable as F
import Data.Kind
import qualified Data.Map as Map
import Prelude hiding (all, filter, map)

-- | System to process queries in parallel.
--
-- @since 0.9
newtype Task t (m :: Type -> Type) a = Task
  { -- | Run a system on a collection of `Entities`.
    --
    -- @since 0.9
    runTask :: ((Entities -> Entities) -> t m Entities) -> t m a
  }
  deriving (Functor)

type System = SystemT Identity

data SystemKind t m a where
  Pure :: a -> SystemKind t m a
  Map :: (a -> b) -> SystemKind t m a -> SystemKind t m b
  Ap :: SystemKind t m (a -> b) -> SystemKind t m a -> SystemKind t m b
  Bind :: SystemKind t m a -> (a -> SystemKind t m b) -> SystemKind t m b
  Once :: Task t m a -> SystemKind t m a

newtype SystemT m a
  = System {unSystem :: forall t. (MonadTrans t, Monad (t m)) => SystemKind t m a}

instance Functor (SystemT m) where
  fmap f (System s) = System $ Map f s

instance (Monad m) => Applicative (SystemT m) where
  pure a = System $ Pure a
  (System f) <*> (System g) = System $ Ap f g

instance (Monad m) => Monad (SystemT m) where
  (System a) >>= f = System $ Bind a (unSystem . f)

-- | @since 0.9
mapDyn :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
mapDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

mapSingleMaybeDyn :: (Monad m) => DynamicQueryT m a -> SystemT m (Maybe a)
mapSingleMaybeDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
  case V.viewSingle (Q.reads rws <> Q.writes rws) $ archetypes w of
    Just v -> do
      (o, v') <- lift $ V.mapSingleDyn q v
      _ <- f $ V.unview v'
      return o
    Nothing -> return Nothing

filterMapDyn :: (Monad m) => (Node -> Bool) -> DynamicQueryT m a -> SystemT m [a]
filterMapDyn qf q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) qf $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

filterMapDyn' :: (MonadTrans t, Monad (t m), Monad m) => (Node -> Bool) -> DynamicQueryT m a -> Task t m [a]
filterMapDyn' qf q = Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) qf $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

-- | @since 0.9
map :: (Monad m) => QueryT m a -> SystemT m [a]
map q = do
  dynQ <- fromQuery q
  mapDyn dynQ

mapSingleMaybe :: (Monad m) => QueryT m a -> SystemT m (Maybe a)
mapSingleMaybe q = do
  dynQ <- fromQuery q
  mapSingleMaybeDyn dynQ

filterMap :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m [a]
filterMap q qf = System $ Once . Task $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
      (dynF, _) = runQueryFilter qf cs'
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  runTask (filterMapDyn' f' dynQ) f

all :: (Monad m) => QueryT m a -> SystemT m [a]
all q = do
  dynQ <- fromQuery q
  allDyn dynQ

filter :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m [a]
filter q qf = System $ Once . Task $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
      (dynF, _) = runQueryFilter qf cs'
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  runTask (filterDyn' dynQ f') f

allDyn :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
allDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  lift $
    if V.null v
      then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities w}
      else V.allDyn q v

filterDyn :: (Monad m) => DynamicQueryT m a -> (Node -> Bool) -> SystemT m [a]
filterDyn q qf = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) qf $ archetypes w
  lift $ V.allDyn q v

filterDyn' :: (MonadTrans t, Monad (t m), Monad m) => DynamicQueryT m a -> (Node -> Bool) -> Task t m [a]
filterDyn' q qf = Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) qf $ archetypes w
  lift $ V.allDyn q v

-- | Convert a `QueryT` to a `System`.
--
-- @since 0.9
fromQuery :: (Monad m) => QueryT m a -> SystemT m (DynamicQueryT m a)
fromQuery q = System $ Once . Task $ \f -> do
  -- TODO race?
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
  _ <- f $ const w {components = cs'}
  return dynQ

runSystemT :: (MonadTrans t, Monad (t m), Monad m) => SystemT m a -> ((Entities -> Entities) -> t m Entities) -> t m a
runSystemT (System s) = runSystemKind s

runSystemKind :: (MonadTrans t, Monad (t m), Monad m) => SystemKind t m a -> ((Entities -> Entities) -> t m Entities) -> t m a
runSystemKind (Pure a) _ = return a
runSystemKind (Map f' s') f = f' <$> runSystemKind s' f
runSystemKind (Ap f' a) f = runSystemKind f' f <*> runSystemKind a f
runSystemKind (Bind a f') f = runSystemKind a f >>= \a' -> runSystemKind (f' a') f
runSystemKind (Once (Task t)) f = t f

concurrently :: SystemT IO a -> ((Entities -> Entities) -> IO Entities) -> IO a
concurrently (System s) f = runSystemKindConcurrently s $ lift . f

runSystemKindConcurrently :: SystemKind IdentityT IO a -> ((Entities -> Entities) -> IdentityT IO Entities) -> IO a
runSystemKindConcurrently (Pure a) _ = return a
runSystemKindConcurrently (Map f' s') f = f' <$> runSystemKindConcurrently s' f
runSystemKindConcurrently (Ap f' a) f = do
  aVar <- newEmptyMVar
  fVar <- newEmptyMVar
  _ <- forkIO $ do
    f'' <- runSystemKindConcurrently f' f
    putMVar fVar f''
  _ <- forkIO $ do
    a' <- runSystemKindConcurrently a f
    putMVar aVar a'
  a' <- takeMVar aVar
  f'' <- takeMVar fVar
  return $ f'' a'
runSystemKindConcurrently (Bind a f') f = runSystemKindConcurrently a f >>= \a' -> runSystemKindConcurrently (f' a') f
runSystemKindConcurrently (Once (Task t)) f = runIdentityT $ t f
