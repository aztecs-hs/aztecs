{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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
    system,
  )
where

import Aztecs.ECS.Access.Class
import Aztecs.ECS.Query (QueryT (..))
import Aztecs.ECS.Query.Dynamic (DynamicQueryT)
import qualified Aztecs.ECS.Query.Dynamic as Q
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader)
import qualified Aztecs.ECS.Query.Dynamic.Reader as Q
import Aztecs.ECS.Query.Reader
import Aztecs.ECS.System
import Aztecs.ECS.World (World (..))
import qualified Aztecs.ECS.World as W
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Bundle
import qualified Aztecs.ECS.World.Entities as E
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Foldable as F

-- | @since 0.9
type Access = AccessT Identity

-- | Access into the `World`.
--
-- @since 0.9
newtype AccessT m a = AccessT {unAccessT :: StateT World m a}
  deriving (Functor, Applicative, MonadFix, MonadIO)

-- | @since 0.9
instance (Monad m) => Monad (AccessT m) where
  a >>= f = AccessT $ do
    !w <- get
    (a', w') <- lift $ runAccessT a w
    put (rnf w' `seq` w')
    unAccessT $ f a'

-- | Run an `Access` on a `World`, returning the output and updated `World`.
--
-- @since 0.9
runAccessT :: (Functor m) => AccessT m a -> World -> m (a, World)
runAccessT a = runStateT $ unAccessT a

-- | Run an `Access` on an empty `World`.
--
-- @since 0.9
runAccessT_ :: (Functor m) => AccessT m a -> m a
runAccessT_ a = fmap fst . runAccessT a $ W.empty

-- | @since 0.9
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

-- | @since 0.9
instance (Monad m) => MonadReaderSystem QueryReader (AccessT m) where
  all q = AccessT $ do
    w <- get
    let (cs, dynQ) = runQueryReader q . E.components $ entities w
    put w {entities = (entities w) {E.components = cs}}
    unAccessT $ allDyn dynQ
  filter q f = AccessT $ do
    w <- get
    let (cs, dynQ) = runQueryReader q . E.components $ entities w
        (dynF, cs') = runQueryFilter f cs
    put w {entities = (entities w) {E.components = cs'}}
    let f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
    unAccessT $ filterDyn dynQ f'

-- | @since 0.9
instance (Monad m) => MonadSystem (QueryT m) (AccessT m) where
  map q = AccessT $ do
    !w <- get
    let (cs, dynQ) = runQuery q . E.components $ entities w
    put w {entities = (entities w) {E.components = cs}}
    unAccessT $ mapDyn dynQ
  mapSingleMaybe q = AccessT $ do
    !w <- get
    let (cs, dynQ) = runQuery q . E.components $ entities w
    put w {entities = (entities w) {E.components = cs}}
    unAccessT $ mapSingleMaybeDyn dynQ
  filterMap q f = AccessT $ do
    !w <- get
    let (cs, dynQ) = runQuery q . E.components $ entities w
        (dynF, cs') = runQueryFilter f cs
    put w {entities = (entities w) {E.components = cs'}}
    let f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynF)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynF)
    unAccessT $ filterMapDyn f' dynQ

-- | @since 0.9
instance (Monad m) => MonadDynamicReaderSystem DynamicQueryReader (AccessT m) where
  allDyn q = AccessT $ do
    !w <- get
    return . Q.allDyn q $ entities w
  filterDyn q f = AccessT $ do
    !w <- get
    return . Q.filterDyn f q $ entities w

-- | @since 0.9
instance (Monad m) => MonadDynamicSystem (DynamicQueryT m) (AccessT m) where
  mapDyn q = AccessT $ do
    !w <- get
    (as, es) <- lift . Q.mapDyn q $ entities w
    put w {entities = es}
    return as
  mapSingleMaybeDyn q = AccessT $ do
    !w <- get
    (res, es) <- lift . Q.mapSingleMaybeDyn q $ entities w
    put w {entities = es}
    return res
  filterMapDyn f q = AccessT $ do
    !w <- get
    (as, es) <- lift . Q.filterMapDyn f q $ entities w
    put w {entities = es}
    return as

-- | Run a `System`.
--
-- @since 0.9
system :: System a -> AccessT IO a
system s = AccessT $ do
  !w <- get
  esVar <- lift . newTVarIO $ entities w
  a <- lift . atomically $ runReaderT (runSystemT s) esVar
  es <- lift $ readTVarIO esVar
  put w {entities = es}
  return a
