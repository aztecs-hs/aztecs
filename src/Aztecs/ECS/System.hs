{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

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
    readQueryEntities,

    -- *** Writing
    query,
    querySingleMaybe,

    -- *** Conversion
    fromQuery,
    mapSystem,
    fromSystem,

    -- ** Dynamic Queries

    -- *** Reading
    readQueryDyn,
    readQueryEntitiesDyn,

    -- *** Writing
    queryDyn,
    querySingleMaybeDyn,
  )
where

import Aztecs.ECS.Entity
import Aztecs.ECS.Query (QueryT (..))
import Aztecs.ECS.Query.Dynamic
  ( DynamicQueryT,
    queryFilter,
    readDynQuery,
    readDynQueryEntities,
  )
import Aztecs.ECS.View (View)
import qualified Aztecs.ECS.View as V
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Monad.Identity
import Control.Monad.Trans
import qualified Data.Map as Map

type System = SystemT Identity

-- | System to process entities.
--
-- @since 0.13
newtype SystemT m a
  = System {runSystemT :: Entities -> m (a, View)}
  deriving (Functor)

instance (Monad m) => Applicative (SystemT m) where
  {-# INLINE pure #-}
  pure a = System . const $ return (a, mempty)

  {-# INLINE (<*>) #-}
  System f <*> System a = System $ \g -> do
    (f', v) <- f g
    (a', v') <- a g
    return (f' a', v' <> v)

instance (Monad m) => Monad (SystemT m) where
  {-# INLINE (>>=) #-}
  System a >>= f = System $ \g -> do
    (a', v) <- a g
    (b', v') <- runSystemT (f a') g
    return (b', v' <> v)

instance (MonadIO m) => MonadIO (SystemT m) where
  {-# INLINE liftIO #-}
  liftIO = lift . liftIO

instance MonadTrans SystemT where
  {-# INLINE lift #-}
  lift m = System $ const (fmap (,mempty) m)

-- | Match all entities with a `QueryT`.
--
-- @since 0.11
readQuery :: (Monad m) => QueryT m a -> SystemT m [a]
readQuery q = do
  dynQ <- fromQuery q
  readQueryDyn dynQ

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

-- | Map all entities with a `DynamicQueryT`.
--
-- @since 0.11
queryDyn :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
queryDyn q = System $ \es -> do
  let qf = queryFilter q
      !v = V.view qf $ archetypes es
  V.mapDyn q v

-- | Map a single entity with a `DynamicQueryT`.
--
-- @since 0.11
querySingleMaybeDyn :: (Monad m) => DynamicQueryT m a -> SystemT m (Maybe a)
querySingleMaybeDyn q = System $ \es -> do
  let qf = queryFilter q
  case V.viewSingle qf $ archetypes es of
    Just v -> do
      (o, v') <- V.mapSingleDyn q v

      return (o, v')
    Nothing -> return (Nothing, mempty)

readQueryDyn :: (Monad m) => DynamicQueryT m a -> SystemT m [a]
readQueryDyn q = System $ \es -> do
  let qf = queryFilter q
      !v = V.view qf $ archetypes es
  res <-
    if V.null v
      then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities es}
      else V.allDyn q v
  return (res, mempty)

-- | Match entities with a `QueryT`.
--
-- @since 0.11
readQueryEntities :: (Monad m) => [EntityID] -> QueryT m a -> SystemT m [a]
readQueryEntities es q = fromQuery q >>= readQueryEntitiesDyn es

readQueryEntitiesDyn :: (Monad m) => [EntityID] -> DynamicQueryT m a -> SystemT m [a]
readQueryEntitiesDyn eIds q = System $ \es -> do
  let qf = queryFilter q
      !v = V.view qf $ archetypes es
  res <-
    if V.null v
      then readDynQueryEntities eIds q $ A.empty {A.entities = Map.keysSet $ entities es}
      else V.allDyn q v
  return (res, mempty)

-- | Convert a `QueryT` to a `SystemT`.
--
-- @since 0.11
fromQuery :: (Monad m) => QueryT m a -> SystemT m (DynamicQueryT m a)
fromQuery q = System $ \es -> do
  let (_, dynQ) = runQuery q $ components es
  return (dynQ, mempty)

mapSystem :: (m (a, View) -> n (b, View)) -> SystemT m a -> SystemT n b
mapSystem f (System a) = System $ \es -> f $ a es

fromSystem :: (Applicative m) => System a -> SystemT m a
fromSystem (System a) = System $ \es -> pure . runIdentity $ a es
