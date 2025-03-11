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

    -- ** Queries

    -- *** Reading
    readQuery,
    readQueryT,
    readQueryEntities,
    readQueryEntitiesT,

    -- *** Writing
    query,
    queryT,
    querySingleMaybe,
    querySingleMaybeT,

    -- *** Conversion
    fromQuery,
    fromQueryT,

    -- ** Dynamic Queries

    -- *** Reading
    readQueryDyn,
    readQueryDynT,
    readQueryEntitiesDyn,
    readQueryEntitiesDynT,

    -- *** Writing
    queryDyn,
    queryDynT,
    querySingleMaybeDyn,
    querySingleMaybeDynT,

    -- * Internal
    Job (..),
    Task (..),

    -- ** Running
    runSystemT,
    concurrently,
  )
where

import Aztecs.ECS.Entity
import Aztecs.ECS.Query (Query, QueryT (..))
import Aztecs.ECS.Query.Dynamic
  ( DynamicQuery,
    DynamicQueryT,
    queryFilter,
    readDynQuery,
    readDynQueryEntities,
  )
import qualified Aztecs.ECS.View as V
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Concurrent
import Control.Monad.Identity
import Control.Monad.Trans
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

-- | Match all entities with a `Query`.
--
-- @since 0.11
readQuery :: (Monad m) => Query a -> SystemT m [a]
readQuery q = do
  dynQ <- fromQuery q
  readQueryDyn dynQ

-- | Match all entities with a `QueryT`.
--
-- @since 0.11
readQueryT :: (Monad m) => QueryT m a -> SystemT m [a]
readQueryT q = do
  dynQ <- fromQueryT q
  readQueryDynT dynQ

-- | Match and update all entities with a `QueryT`.
--
-- @since 0.11
query :: (Monad m) => Query a -> SystemT m [a]
query q = do
  dynQ <- fromQuery q
  queryDyn dynQ

-- | Match and update all entities with a `QueryT`.
--
-- @since 0.11
queryT :: (Monad m) => QueryT m a -> SystemT m [a]
queryT q = do
  dynQ <- fromQueryT q
  queryDynT dynQ

-- | Match and update a single entity with a `Query`, or @Nothing@.
--
-- @since 0.11
querySingleMaybe :: (Monad m) => Query a -> SystemT m (Maybe a)
querySingleMaybe q = do
  dynQ <- fromQuery q
  querySingleMaybeDyn dynQ

-- | Match and update a single entity with a `QueryT`, or @Nothing@.
--
-- @since 0.11
querySingleMaybeT :: (Monad m) => QueryT m a -> SystemT m (Maybe a)
querySingleMaybeT q = do
  dynQ <- fromQueryT q
  querySingleMaybeDynT dynQ

-- | Map all entities with a `DynamicQuery`.
--
-- @since 0.11
queryDyn :: (Monad m) => DynamicQuery a -> SystemT m [a]
queryDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let qf = queryFilter q
      !v = V.view qf $ archetypes w
  let (o, v') = runIdentity $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

-- | Map all entities with a `DynamicQueryT`.
--
-- @since 0.11
queryDynT :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
queryDynT q = System $ Once . Task $ \f -> do
  w <- f id
  let qf = queryFilter q
      !v = V.view qf $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  _ <- f $ V.unview v'
  return o

-- | Map a single entity with a `DynamicQuery`.
--
-- @since 0.11
querySingleMaybeDyn :: (Monad m) => DynamicQuery a -> SystemT m (Maybe a)
querySingleMaybeDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let qf = queryFilter q
  case V.viewSingle qf $ archetypes w of
    Just v -> do
      let (o, v') = runIdentity $ V.mapSingleDyn q v
      _ <- f $ V.unview v'
      return o
    Nothing -> return Nothing

-- | Map a single entity with a `DynamicQueryT`.
--
-- @since 0.11
querySingleMaybeDynT :: (Monad m) => DynamicQueryT m a -> SystemT m (Maybe a)
querySingleMaybeDynT q = System $ Once . Task $ \f -> do
  w <- f id
  let qf = queryFilter q
  case V.viewSingle qf $ archetypes w of
    Just v -> do
      (o, v') <- lift $ V.mapSingleDyn q v
      _ <- f $ V.unview v'
      return o
    Nothing -> return Nothing

readQueryDyn :: (Monad m) => DynamicQuery a -> SystemT m [a]
readQueryDyn q = System $ Once . Task $ \f -> do
  w <- f id
  let qf = queryFilter q
      !v = V.view qf $ archetypes w
  return $
    runIdentity $
      if V.null v
        then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities w}
        else V.allDyn q v

readQueryDynT :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
readQueryDynT q = System $ Once . Task $ \f -> do
  w <- f id
  let qf = queryFilter q
      !v = V.view qf $ archetypes w
  lift $
    if V.null v
      then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities w}
      else V.allDyn q v

-- | Match entities with a `QueryT`.
--
-- @since 0.11
readQueryEntities :: (Monad m) => [EntityID] -> Query a -> SystemT m [a]
readQueryEntities es q = fromQuery q >>= readQueryEntitiesDyn es

-- | Match entities with a `QueryT`.
--
-- @since 0.11
readQueryEntitiesT :: (Monad m) => [EntityID] -> QueryT m a -> SystemT m [a]
readQueryEntitiesT es q = fromQueryT q >>= readQueryEntitiesDynT es

readQueryEntitiesDyn :: (Monad m) => [EntityID] -> DynamicQuery a -> SystemT m [a]
readQueryEntitiesDyn es q = System $ Once . Task $ \f -> do
  w <- f id
  let qf = queryFilter q
      !v = V.view qf $ archetypes w
  return . runIdentity $
    if V.null v
      then readDynQueryEntities es q $ A.empty {A.entities = Map.keysSet $ entities w}
      else V.allDyn q v

readQueryEntitiesDynT :: (Monad m) => [EntityID] -> DynamicQueryT m a -> SystemT m [a]
readQueryEntitiesDynT es q = System $ Once . Task $ \f -> do
  w <- f id
  let qf = queryFilter q
      !v = V.view qf $ archetypes w
  lift $
    if V.null v
      then readDynQueryEntities es q $ A.empty {A.entities = Map.keysSet $ entities w}
      else V.allDyn q v

-- | Convert a `Query` to a `SystemT`.
--
-- @since 0.11
fromQuery :: Query a -> SystemT m (DynamicQuery a)
fromQuery q = System $ Once . Task $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
  _ <- f $ const w {components = cs'}
  return dynQ

-- | Convert a `QueryT` to a `SystemT`.
--
-- @since 0.11
fromQueryT :: (Monad m) => QueryT m a -> SystemT m (DynamicQueryT m a)
fromQueryT q = System $ Once . Task $ \f -> do
  w <- f id
  let (cs', dynQ) = runQuery q $ components w
  _ <- f $ const w {components = cs'}
  return dynQ

runSystemT ::
  (MonadTrans t, Monad (t m), Monad m) =>
  SystemT m a ->
  ((Entities -> Entities) -> t m Entities) ->
  t m a
runSystemT (System s) = runJob s

runJob ::
  (MonadTrans t, Monad (t m), Monad m) =>
  Job t m a ->
  ((Entities -> Entities) -> t m Entities) ->
  t m a
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
