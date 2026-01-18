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
    MonadReaderSystem (..),
    MonadSystem (..),
    MonadDynamicReaderSystem (..),
    MonadDynamicSystem (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQuery, DynamicQueryFilter (..), DynamicQueryT (..), runDynQuery)
import Aztecs.ECS.System.Class
import Aztecs.ECS.System.Dynamic.Class
import Aztecs.ECS.System.Dynamic.Reader.Class
import Aztecs.ECS.System.Reader.Class
import qualified Aztecs.ECS.View as V
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Category
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Set (Set)
import Prelude hiding (all, filter, id, map, (.))

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

instance MonadDynamicSystem (DynamicQueryT STM) System where
  mapDyn cIds q = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.view cIds $ archetypes w
    (o, v') <- lift $ V.mapDyn q v
    lift . modifyTVar wVar $ V.unview v'
    return o
  mapSingleMaybeDyn cIds q = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    case V.viewSingle cIds $ archetypes w of
      Just v -> do
        (o, v') <- lift $ V.mapSingleDyn q v
        lift . modifyTVar wVar $ V.unview v'
        return o
      Nothing -> return Nothing
  filterMapDyn cIds f q = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.filterView cIds f $ archetypes w
    (o, v') <- lift $ V.mapDyn q v
    lift . modifyTVar wVar $ V.unview v'
    return o

instance MonadSystem (QueryT STM) System where
  map q = SystemT $ do
    (rws, dynQ) <- runSystemT $ fromQuery q
    runSystemT $ mapDyn (Q.reads rws <> Q.writes rws) dynQ
  mapSingleMaybe q = SystemT $ do
    (rws, dynQ) <- runSystemT $ fromQuery q
    runSystemT $ mapSingleMaybeDyn (Q.reads rws <> Q.writes rws) dynQ
  filterMap q qf = SystemT $ do
    wVar <- ask
    let go w =
          let (rws, cs', dynQ) = runQuery q $ components w
              (dynF, cs'') = Q.runQueryFilter qf cs'
           in ((rws, dynQ, dynF), w {components = cs''})
    (rws, dynQ, dynF) <- lift $ stateTVar wVar go
    let f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
    runSystemT $ filterMapDyn (Q.reads rws <> Q.writes rws) f' dynQ

instance MonadReaderSystem Query System where
  all q = SystemT $ do
    (cIds, dynQ) <- runSystemT $ fromQueryReader q
    runSystemT $ allDyn cIds dynQ
  filter q qf = SystemT $ do
    wVar <- ask
    let go w =
          let (rws, cs', dynQ) = runQuery q $ components w
              (dynF, cs'') = Aztecs.ECS.Query.runQueryFilter qf cs'
           in ((Q.reads rws <> Q.writes rws, dynQ, dynF), w {components = cs''})
    (cIds, dynQ, dynF) <- lift $ stateTVar wVar go
    let f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
    runSystemT $ filterDyn cIds dynQ f'

instance MonadDynamicReaderSystem DynamicQuery System where
  allDyn cIds q = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.view cIds $ archetypes w
    return $
      if V.null v
        then fst $ runDynQuery q A.empty {A.entities = Map.keysSet $ entities w}
        else V.allDyn q v
  filterDyn cIds q f = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.filterView cIds f $ archetypes w
    return $ V.allDyn q v

-- | Convert a `QueryReaderT` to a `System`.
--
-- @since 0.9
fromQueryReader :: Query a -> System (Set ComponentID, DynamicQuery a)
fromQueryReader q = SystemT $ do
  wVar <- ask
  let go w =
        let (rws, cs', dynQ) = runQuery q $ components w
         in ((Q.reads rws <> Q.writes rws, dynQ), w {components = cs'})
  lift $ stateTVar wVar go

-- | Convert a `QueryT` to a `System`.
--
-- @since 0.9
fromQuery :: QueryT STM a -> System (ReadsWrites, DynamicQueryT STM a)
fromQuery q = SystemT $ do
  wVar <- ask
  let go w =
        let (rws, cs', dynQ) = runQuery q $ components w
         in ((rws, dynQ), w {components = cs'})
  lift $ stateTVar wVar go
