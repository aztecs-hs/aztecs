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

import Aztecs.ECS.Query
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQueryT (..))
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader (..))
import Aztecs.ECS.Query.Reader
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
instance MonadDynamicSystem (DynamicQueryT STM) System where
  mapDyn (DynamicQuery (rws, q)) = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.view (Q.reads rws <> Q.writes rws) $ archetypes w
    (o, v') <- lift $ V.mapDyn (DynamicQuery (rws, q)) v
    lift . modifyTVar wVar $ V.unview v'
    return o
  mapSingleMaybeDyn (DynamicQuery (rws, q)) = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    case V.viewSingle (Q.reads rws <> Q.writes rws) $ archetypes w of
      Just v -> do
        (o, v') <- lift $ V.mapSingleDyn (DynamicQuery (rws, q)) v
        lift . modifyTVar wVar $ V.unview v'
        return o
      Nothing -> return Nothing
  filterMapDyn f (DynamicQuery (rws, q)) = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.filterView (Q.reads rws <> Q.writes rws) f $ archetypes w
    (o, v') <- lift $ V.mapDyn (DynamicQuery (rws, q)) v
    lift . modifyTVar wVar $ V.unview v'
    return o

-- | @since 0.9
instance MonadSystem (QueryT STM) System where
  map q = SystemT $ do
    dynQ <- runSystemT $ fromQuery q
    runSystemT $ mapDyn dynQ
  mapSingleMaybe q = SystemT $ do
    dynQ <- runSystemT $ fromQuery q
    runSystemT $ mapSingleMaybeDyn dynQ
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

-- | @since 0.9
instance MonadReaderSystem QueryReader System where
  all q = SystemT $ do
    dynQ <- runSystemT $ fromQueryReader q
    runSystemT $ allDyn dynQ
  filter q qf = SystemT $ do
    wVar <- ask
    let go w =
          let (cs', dynQ) = runQueryReader q $ components w
              (dynF, cs'') = runQueryFilter qf cs'
           in ((dynQ, dynF), w {components = cs''})
    (dynQ, dynF) <- lift $ stateTVar wVar go
    let f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
    runSystemT $ filterDyn dynQ f'

-- | @since 0.9
instance MonadDynamicReaderSystem DynamicQueryReader System where
  allDyn (DynamicQueryReader (cIds, q)) = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.view cIds $ archetypes w
    return $
      if V.null v
        then q A.empty {A.entities = Map.keysSet $ entities w}
        else V.allDyn (DynamicQueryReader (cIds, q)) v
  filterDyn (DynamicQueryReader (cIds, q)) f = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.filterView cIds f $ archetypes w
    return $ V.allDyn (DynamicQueryReader (cIds, q)) v

-- | Convert a `QueryReaderT` to a `System`.
--
-- @since 0.9
fromQueryReader :: QueryReader a -> System (DynamicQueryReader a)
fromQueryReader q = SystemT $ do
  wVar <- ask
  let go w =
        let (cs', dynQ) = runQueryReader q $ components w
         in (dynQ, w {components = cs'})
  lift $ stateTVar wVar go

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
