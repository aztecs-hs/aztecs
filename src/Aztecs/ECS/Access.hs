{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
    runAccess,
    runAccessT,
    runAccessT_,
    spawn,
    spawn_,
    insert,
    lookup,
    remove,
    despawn,
    system,
    systemM,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.System (SystemT (..))
import qualified Aztecs.ECS.System as S
import Aztecs.ECS.World (World)
import qualified Aztecs.ECS.World as W
import Aztecs.ECS.World.Bundle
import qualified Aztecs.ECS.World.Entities as E
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.State.Strict
import Prelude hiding (all, filter, lookup, map, mapM)

type Access = AccessT Identity

-- | Access into the `World`.
newtype AccessT m a = AccessT {unAccessT :: StateT (World m) m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

runAccess :: Access a -> World Identity -> (a, World Identity)
runAccess a = runIdentity . runAccessT a

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccessT :: (Functor m) => AccessT m a -> World m -> m (a, World m)
runAccessT a = runStateT $ unAccessT a

-- | Run an `Access` on an empty `World`.
runAccessT_ :: (Monad m) => AccessT m a -> m a
runAccessT_ a = fmap fst . runAccessT a $ W.empty

spawn :: (Monad m) => BundleT m -> AccessT m EntityID
spawn b = AccessT $ do
  !w <- get
  (e, w') <- lift $ W.spawn b w
  put w'
  return e

spawn_ :: (Monad m) => BundleT m -> AccessT m ()
spawn_ = void . spawn

insert :: (Monad m) => EntityID -> BundleT m -> AccessT m ()
insert e c = AccessT $ do
  !w <- get
  w' <- lift $ W.insert e c w
  put w'

lookup :: forall m a. (Monad m, Component m a) => EntityID -> AccessT m (Maybe a)
lookup e = AccessT $ do
  !w <- get
  return $ W.lookup @m e w

remove :: forall m a. (Monad m, Component m a) => EntityID -> AccessT m (Maybe a)
remove e = AccessT $ do
  !w <- get
  (a, w') <- lift $ W.remove @m e w
  put w'
  return a

despawn :: (Monad m) => EntityID -> AccessT m ()
despawn e = AccessT $ do
  !w <- get
  let !(_, w') = W.despawn e w
  put w'

-- | Run a `SystemT` on the `World`.
system :: (Monad m) => SystemT m a -> AccessT m a
system sys = AccessT $ do
  !w <- get
  let !es = W.entities w
      !(cs', dynSys) = S.runSystemT sys $ E.components es
  (a, es') <- lift $ S.runDynamicSystemT dynSys es
  put w {W.entities = es' {E.components = cs'}}
  return a
{-# INLINE system #-}

-- | Run a `SystemT` on the `World`.
systemM :: (Monad m) => SystemT m a -> AccessT m a
systemM = system
{-# INLINE systemM #-}
