{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
newtype AccessT m a = AccessT {unAccessT :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans)

runAccess :: Access a -> World -> (a, World)
runAccess a = runIdentity . runAccessT a

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccessT :: (Functor m) => AccessT m a -> World -> m (a, World)
runAccessT a = runStateT $ unAccessT a

-- | Run an `Access` on an empty `World`.
runAccessT_ :: (Functor m) => AccessT m a -> m a
runAccessT_ a = fmap fst . runAccessT a $ W.empty

spawn :: (Monad m) => Bundle -> AccessT m EntityID
spawn b = AccessT $ do
  !w <- get
  let !(e, w') = W.spawn b w
  put w'
  return e

spawn_ :: (Monad m) => Bundle -> AccessT m ()
spawn_ = void . spawn

insert :: (Monad m) => EntityID -> Bundle -> AccessT m ()
insert e c = AccessT $ do
  !w <- get
  let !w' = W.insert e c w
  put w'

lookup :: (Monad m, Component a) => EntityID -> AccessT m (Maybe a)
lookup e = AccessT $ do
  !w <- get
  return $ W.lookup e w

remove :: (Monad m, Component a) => EntityID -> AccessT m (Maybe a)
remove e = AccessT $ do
  !w <- get
  let !(a, w') = W.remove e w
  put w'
  return a

despawn :: (Monad m) => EntityID -> AccessT m ()
despawn e = AccessT $ do
  !w <- get
  let !(_, w') = W.despawn e w
  put w'

-- | Run a pure `System` on the `World`.
system :: (Monad m) => S.System a -> AccessT m a
system sys = AccessT $ do
  !w <- get
  let !es = W.entities w
      !(cs', dynSys) = S.runSystemT sys $ E.components es
      !(a, es') = runIdentity $ S.runDynamicSystemT dynSys es
  put w {W.entities = es' {E.components = cs'}}
  return a
{-# INLINE system #-}

-- | Run a `SystemT` on the `World`.
systemM :: (Monad m) => SystemT m a -> AccessT m a
systemM sys = AccessT $ do
  !w <- get
  let !es = W.entities w
      !(cs', dynSys) = S.runSystemT sys $ E.components es
  (a, es') <- lift $ S.runDynamicSystemT dynSys es
  put w {W.entities = es' {E.components = cs'}}
  return a
{-# INLINE systemM #-}
