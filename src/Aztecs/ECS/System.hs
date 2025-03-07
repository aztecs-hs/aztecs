{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Control.Category
import Control.Concurrent.STM
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Prelude hiding (all, filter, id, map, (.))

-- | @since 0.9
type System = SystemT STM

-- | System to process queries in parallel.
--
-- @since 0.9
newtype SystemT m a = SystemT
  { -- | Run a system on a collection of `Entities`.
    --
    -- @since 0.9
    runSystemT :: ReaderT (TVar Entities) m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.9
mapDyn :: DynamicQueryT STM a -> System [a]
mapDyn q = SystemT $ do
  wVar <- ask
  w <- lift $ readTVar wVar
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  lift . modifyTVar wVar $ V.unview v'
  return o

mapSingleMaybeDyn :: DynamicQueryT STM a -> System (Maybe a)
mapSingleMaybeDyn q = SystemT $ do
  wVar <- ask
  w <- lift $ readTVar wVar
  let rws = readsWrites q
  case V.viewSingle (Q.reads rws <> Q.writes rws) $ archetypes w of
    Just v -> do
      (o, v') <- lift $ V.mapSingleDyn q v
      lift . modifyTVar wVar $ V.unview v'
      return o
    Nothing -> return Nothing

filterMapDyn :: (Node -> Bool) -> DynamicQueryT STM a -> System [a]
filterMapDyn f q = SystemT $ do
  wVar <- ask
  w <- lift $ readTVar wVar
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) f $ archetypes w
  (o, v') <- lift $ V.mapDyn q v
  lift . modifyTVar wVar $ V.unview v'
  return o

-- | @since 0.9
map :: QueryT STM a -> System [a]
map q = SystemT $ do
  dynQ <- runSystemT $ fromQuery q
  runSystemT $ mapDyn dynQ

mapSingleMaybe :: QueryT STM a -> System (Maybe a)
mapSingleMaybe q = SystemT $ do
  dynQ <- runSystemT $ fromQuery q
  runSystemT $ mapSingleMaybeDyn dynQ

filterMap :: QueryT STM a -> QueryFilter -> System [a]
filterMap q qf = SystemT $ do
  wVar <- ask
  let go w =
        let (cs', dynQ) = runQuery q $ components w
            (dynF, cs'') = runQueryFilter qf cs'
         in ((dynQ, dynF), w {components = cs''})
  (dynQ, dynF) <- lift $ stateTVar wVar go
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  runSystemT $ filterMapDyn f' dynQ

all :: QueryT STM a -> System [a]
all q = SystemT $ do
  wVar <- ask
  let go w =
        let (cs', dynQ) = runQuery q $ components w
         in (dynQ, w {components = cs'})
  dynQ <- lift $ stateTVar wVar go
  runSystemT $ allDyn dynQ

filter :: QueryT STM a -> QueryFilter -> System [a]
filter q qf = SystemT $ do
  wVar <- ask
  let go w =
        let (cs', dynQ) = runQuery q $ components w
            (dynF, cs'') = runQueryFilter qf cs'
         in ((dynQ, dynF), w {components = cs''})
  (dynQ, dynF) <- lift $ stateTVar wVar go
  let f' n =
        F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
          && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
  runSystemT $ filterDyn dynQ f'

allDyn :: DynamicQueryT STM a -> System [a]
allDyn q = SystemT $ do
  wVar <- ask
  w <- lift $ readTVar wVar
  let rws = readsWrites q
      !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
  lift $
    if V.null v
      then readDynQuery q $ A.empty {A.entities = Map.keysSet $ entities w}
      else V.allDyn q v

filterDyn :: DynamicQueryT STM a -> (Node -> Bool) -> System [a]
filterDyn q f = SystemT $ do
  wVar <- ask
  w <- lift $ readTVar wVar
  let rws = readsWrites q
      !v = V.filterView (Q.reads rws <> Q.writes rws) f $ archetypes w
  lift $ V.allDyn q v

-- | Convert a `QueryT` to a `System`.
--
-- @since 0.9
fromQuery :: QueryT STM a -> System (DynamicQueryT STM a)
fromQuery q = SystemT $ do
  wVar <- ask
  let go w =
        let (cs', dynQ) = runQuery q $ components w
         in (dynQ, w {components = cs'})
  lift $ stateTVar wVar go
