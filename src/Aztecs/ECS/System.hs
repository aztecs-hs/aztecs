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
-- Systems to process entities.
module Aztecs.ECS.System
  ( -- * Systems
    System,
    SystemT (..),

    -- ** Reading
    readQuery,
    readFilterQuery,
    allDyn,
    filterDyn,
    readQueryEntities,

    -- ** Writing
    query,
    filterQuery,
    querySingleMaybe,
    queryDyn,
    filterQueryDyn,
    querySingleMaybeDyn,

    -- ** Running
    runSystemT,
    concurrently,

    -- * Internal
    Job (..),
    Task (..),
  )
where

import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.Query (QueryFilter (..), QueryT (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQueryFilter (..), DynamicQueryT, readDynQuery, readDynQueryEntities, readsWrites)
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

-- | System task.
--
-- @since 0.11
newtype Task t (m :: Type -> Type) a
  = Task {runTask :: ((Entities -> Entities) -> t m Entities) -> t m a}
  deriving (Functor)

-- | Job to be interpreted.
--
-- @since 0.11
data Job t m a where
  Pure :: a -> Job t m a
  Map :: (a -> b) -> Job t m a -> Job t m b
  Ap :: Job t m (a -> b) -> Job t m a -> Job t m b
  Bind :: Job t m a -> (a -> Job t m b) -> Job t m b
  Once :: Task t m a -> Job t m a

type System = SystemT Identity

-- | System to process entities.
--
-- @since 0.11
newtype SystemT m a
  = System {unSystem :: forall t. (MonadTrans t, Monad (t m)) => Job t m a}

-- | @since 0.11
instance Functor (SystemT m) where
  fmap f (System s) = System $ Map f s

-- | @since 0.11
instance (Monad m) => Applicative (SystemT m) where
  pure a = System $ Pure a
  (System f) <*> (System g) = System $ Ap f g

-- | @since 0.11
instance (Monad m) => Monad (SystemT m) where
  (System a) >>= f = System $ Bind a (unSystem . f)

-- | @since 0.11
instance (MonadIO m) => MonadIO (SystemT m) where
  liftIO m = System $ Once . Task . const . lift $ liftIO m

-- | Map all entities with a `DynamicQueryT`.
--
-- @since 0.11
queryDyn :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
queryDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

-- | Map a single entity with a `DynamicQueryT`.
--
-- @since 0.11
querySingleMaybeDyn :: (Monad m) => DynamicQueryT m a -> SystemT m (Maybe a)
querySingleMaybeDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
  case V.viewSingle (Q.reads rws <> Q.writes rws) $ archetypes w of
    Just v -> do
      (o, v') <- lift $ V.mapSingleDyn q v
      _ <- f $ V.unview v'
      return o
    Nothing -> return Nothing

-- | Filter and map all entities with a `DynamicQueryT`.
--
-- @since 0.11
filterQueryDyn :: (Monad m) => (Node -> Bool) -> DynamicQueryT m a -> SystemT m [a]
filterQueryDyn qf q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) qf $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

filterMapDyn' ::
  (MonadTrans t, Monad (t m), Monad m) => (Node -> Bool) -> DynamicQueryT m a -> Task t m [a]
filterMapDyn' qf q = Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) qf $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

-- | Match and update all entities with a `QueryT`.
--
-- @since 0.11
query :: (Monad m) => QueryT m a -> SystemT m [a]
query q = do
  dynQ <- fromQuery q
  queryDyn dynQ

-- | Match and update a single entity with a `QueryT`, or @Nothing@.
--
-- @since 0.11
querySingleMaybe :: (Monad m) => QueryT m a -> SystemT m (Maybe a)
querySingleMaybe q = do
  dynQ <- fromQuery q
  querySingleMaybeDyn dynQ

-- | Filter and map all entities with a `QueryT`.
--
-- @since 0.11
filterQuery :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m [a]
filterQuery q qf = System $ Once . Task $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
      (dynF, _) = runQueryFilter qf cs'
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  runTask (filterMapDyn' f' dynQ) f

-- | Match all entities with a `QueryT`.
--
-- @since 0.11
readQuery :: (Monad m) => QueryT m a -> SystemT m [a]
readQuery q = do
  dynQ <- fromQuery q
  allDyn dynQ

readFilterQuery :: (Monad m) => QueryT m a -> QueryFilter -> SystemT m [a]
readFilterQuery q qf = System $ Once . Task $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
      (dynF, _) = runQueryFilter qf cs'
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  runTask (filterDyn' dynQ f') f

-- | Match entities with a `QueryT`.
--
-- @since 0.11
readQueryEntities :: (Monad m) => [EntityID] -> QueryT m a -> SystemT m [a]
readQueryEntities es q = fromQuery q >>= queryEntitiesDyn es

allDyn :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
allDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  lift $
    if V.null v
      then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities w}
      else V.allDyn q v

queryEntitiesDyn :: (Monad m) => [EntityID] -> DynamicQueryT m a -> SystemT m [a]
queryEntitiesDyn es q = System $ Once . Task $ \f -> do
  w <- f id
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  lift $
    if V.null v
      then readDynQueryEntities es q $ A.empty {A.entities = Map.keysSet $ entities w}
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
-- @since 0.11
fromQuery :: (Monad m) => QueryT m a -> SystemT m (DynamicQueryT m a)
fromQuery q = System $ Once . Task $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
  _ <- f $ const w {components = cs'}
  return dynQ

runSystemT :: (MonadTrans t, Monad (t m), Monad m) => SystemT m a -> ((Entities -> Entities) -> t m Entities) -> t m a
runSystemT (System s) = runJob s

runJob :: (MonadTrans t, Monad (t m), Monad m) => Job t m a -> ((Entities -> Entities) -> t m Entities) -> t m a
runJob (Pure a) _ = return a
runJob (Map f' s') f = f' <$> runJob s' f
runJob (Ap f' a) f = runJob f' f <*> runJob a f
runJob (Bind a f') f = runJob a f >>= \a' -> runJob (f' a') f
runJob (Once (Task t)) f = t f

concurrently :: SystemT IO a -> ((Entities -> Entities) -> IO Entities) -> IO a
concurrently (System s) f = runJobConcurrently s $ lift . f

runJobConcurrently :: Job IdentityT IO a -> ((Entities -> Entities) -> IdentityT IO Entities) -> IO a
runJobConcurrently (Pure a) _ = return a
runJobConcurrently (Map f' s') f = f' <$> runJobConcurrently s' f
runJobConcurrently (Ap f' a) f = do
  aVar <- newEmptyMVar
  fVar <- newEmptyMVar
  _ <- forkIO $ do
    f'' <- runJobConcurrently f' f
    putMVar fVar f''
  _ <- forkIO $ do
    a' <- runJobConcurrently a f
    putMVar aVar a'
  a' <- takeMVar aVar
  f'' <- takeMVar fVar
  return $ f'' a'
runJobConcurrently (Bind a f') f = runJobConcurrently a f >>= \a' -> runJobConcurrently (f' a') f
runJobConcurrently (Once (Task t)) f = runIdentityT $ t f
