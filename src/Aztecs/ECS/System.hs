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
import Aztecs.ECS.Query.Dynamic (DynamicQueryT)
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReaderT, runDynQueryReaderT)
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
import Data.Set (Set)
import Prelude hiding (all, filter, id, map, (.))

-- | @since 9.0
type System = SystemT STM

-- | System to process queries in parallel.
--
-- @since 9.0
newtype SystemT m a = SystemT
  { -- | Run a system on a collection of `Entities`.
    --
    -- @since 9.0
    runSystemT :: ReaderT (TVar Entities) m a
  }
  deriving (Functor, Applicative, Monad, MonadFix)

-- | @since 9.0
instance MonadDynamicSystem (DynamicQueryT STM) System where
  mapDyn i cIds q = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.view cIds $ archetypes w
    (o, v') <- lift $ V.mapDyn i q v
    lift . modifyTVar wVar $ V.unview v'
    return o
  mapSingleMaybeDyn i cIds q = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    case V.viewSingle cIds $ archetypes w of
      Just v -> do
        (o, v') <- lift $ V.mapSingleDyn i q v
        lift . modifyTVar wVar $ V.unview v'
        return o
      Nothing -> return Nothing
  filterMapDyn i cIds f q = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.filterView cIds f $ archetypes w
    (o, v') <- lift $ V.mapDyn i q v
    lift . modifyTVar wVar $ V.unview v'
    return o

-- | @since 9.0
instance MonadSystem (QueryT STM) System where
  map i q = SystemT $ do
    (rws, dynQ) <- runSystemT $ fromQuery q
    runSystemT $ mapDyn i (Q.reads rws <> Q.writes rws) dynQ
  mapSingleMaybe i q = SystemT $ do
    (rws, dynQ) <- runSystemT $ fromQuery q
    runSystemT $ mapSingleMaybeDyn i (Q.reads rws <> Q.writes rws) dynQ
  filterMap i q qf = SystemT $ do
    wVar <- ask
    let go w =
          let (rws, cs', dynQ) = runQuery q $ components w
              (dynF, cs'') = runQueryFilter qf cs'
           in ((rws, dynQ, dynF), w {components = cs''})
    (rws, dynQ, dynF) <- lift $ stateTVar wVar go
    let f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
    runSystemT $ filterMapDyn i (Q.reads rws <> Q.writes rws) f' dynQ

-- | @since 9.0
instance MonadReaderSystem (QueryReaderT STM) System where
  all i q = SystemT $ do
    (cIds, dynQ) <- runSystemT $ fromQueryReader q
    runSystemT $ allDyn i cIds dynQ
  filter i q qf = SystemT $ do
    wVar <- ask
    let go w =
          let (cIds, cs', dynQ) = runQueryReader q $ components w
              (dynF, cs'') = runQueryFilter qf cs'
           in ((cIds, dynQ, dynF), w {components = cs''})
    (cIds, dynQ, dynF) <- lift $ stateTVar wVar go
    let f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
    runSystemT $ filterDyn i cIds dynQ f'

-- | @since 9.0
instance MonadDynamicReaderSystem (DynamicQueryReaderT STM) System where
  allDyn i cIds q = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.view cIds $ archetypes w
    lift $
      if V.null v
        then runDynQueryReaderT i q A.empty {A.entities = Map.keysSet $ entities w}
        else V.allDyn i q v
  filterDyn i cIds q f = SystemT $ do
    wVar <- ask
    w <- lift $ readTVar wVar
    let !v = V.filterView cIds f $ archetypes w
    lift $ V.allDyn i q v

-- | Convert a `QueryReaderT` to a `System`.
--
-- @since 9.0
fromQueryReader :: QueryReaderT STM i o -> System (Set ComponentID, DynamicQueryReaderT STM i o)
fromQueryReader q = SystemT $ do
  wVar <- ask
  let go w =
        let (cIds, cs', dynQ) = runQueryReader q $ components w
         in ((cIds, dynQ), w {components = cs'})
  lift $ stateTVar wVar go

-- | Convert a `QueryT` to a `System`.
--
-- @since 9.0
fromQuery :: QueryT STM i o -> System (ReadsWrites, DynamicQueryT STM i o)
fromQuery q = SystemT $ do
  wVar <- ask
  let go w =
        let (rws, cs', dynQ) = runQuery q $ components w
         in ((rws, dynQ), w {components = cs'})
  lift $ stateTVar wVar go
