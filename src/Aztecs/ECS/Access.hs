{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
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
    spawn,
    insert,
    lookup,
    remove,
    despawn,
    runAccessT,
    runAccessT_,
    system,

    -- ** Conversion
    mapAccessT,
    fromAccess,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.System (SystemT (..), runSystemT)
import Aztecs.ECS.View
import Aztecs.ECS.World (World (..))
import qualified Aztecs.ECS.World as W
import Aztecs.ECS.World.Bundle
import Control.DeepSeq
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Prelude hiding (lookup)

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

-- | Spawn an entity with a `Bundle`.
--
-- @since 0.11
spawn :: (Monad m) => Bundle -> AccessT m EntityID
spawn b = AccessT $ do
  !w <- get
  let !(e, w') = W.spawn b w
  put w'
  return e

-- | Insert a `Bundle` into an entity.
--
-- @since 0.11
insert :: (Monad m) => EntityID -> Bundle -> AccessT m ()
insert e c = AccessT $ do
  !w <- get
  let !w' = W.insert e c w
  put w'

-- | Lookup a component by `EntityID`.
--
-- @since 0.11
lookup :: (Monad m, Component a) => EntityID -> AccessT m (Maybe a)
lookup e = AccessT $ do
  !w <- get
  return $ W.lookup e w

-- | Remove a component by `EntityID`.
--
-- @since 0.11
remove :: (Monad m, Component a) => EntityID -> AccessT m (Maybe a)
remove e = AccessT $ do
  !w <- get
  let !(a, w') = W.remove e w
  put w'
  return a

-- | Despawn an entity by `EntityID`.
--
-- @since 0.11
despawn :: (Monad m) => EntityID -> AccessT m ()
despawn e = AccessT $ do
  !w <- get
  let !(_, w') = W.despawn e w
  put w'

-- | Run a `System`.
--
-- @since 0.11
system :: (Monad m) => SystemT m a -> AccessT m a
system s = AccessT $ do
  !w <- get
  (a, v) <- lift . runSystemT s $ entities w
  put w {entities = unview v $ entities w}
  return a

mapAccessT :: (Functor m, Functor n) => (m (a, World) -> n (b, World)) -> AccessT m a -> AccessT n b
mapAccessT f =
  AccessT . mapStateT (fmap (\(a, w) -> (a, w)) . f . fmap (\(a, w) -> (a, w))) . unAccessT

fromAccess :: (Applicative m) => Access a -> AccessT m a
fromAccess = mapAccessT (pure . runIdentity)
