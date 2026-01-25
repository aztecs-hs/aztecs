{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( Access (..),
    runAccess,
    runAccess_,
    spawn,
    spawn_,
    insert,
    insertUntracked,
    lookup,
    remove,
    despawn,
    system,
    triggerEvent,
    triggerEntityEvent,
  )
where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.System (System (..))
import qualified Aztecs.ECS.System as S
import Aztecs.ECS.World (World)
import qualified Aztecs.ECS.World as W
import Aztecs.ECS.World.Bundle
import qualified Aztecs.ECS.World.Entities as E
import Control.Monad
import Control.Monad.State
import Prelude hiding (all, filter, lookup, map, mapM)

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccess :: (Functor m) => Access m a -> World m -> m (a, World m)
runAccess a = runStateT $ unAccess a

-- | Run an `Access` on an empty `World`.
runAccess_ :: (Monad m) => Access m a -> m a
runAccess_ a = fmap fst . runAccess a $ W.empty

spawn :: (Monad m) => BundleT m -> Access m EntityID
spawn b = Access $ do
  !w <- get
  let (e, w', hook) = W.spawn b w
  put w'
  unAccess hook
  return e

spawn_ :: (Monad m) => BundleT m -> Access m ()
spawn_ = void . spawn

insert :: (Monad m) => EntityID -> BundleT m -> Access m ()
insert e c = Access $ do
  !w <- get
  let (w', hook) = W.insert e c w
  put w'
  unAccess hook

-- | Insert a component into an entity without running lifecycle hooks.
insertUntracked :: (Monad m) => EntityID -> BundleT m -> Access m ()
insertUntracked e c = Access $ do
  !w <- get
  let w' = W.insertUntracked e c w
  put w'

lookup :: forall m a. (Monad m, Component m a) => EntityID -> Access m (Maybe a)
lookup e = Access $ do
  !w <- get
  return $ W.lookup @m e w

remove :: forall m a. (Monad m, Component m a) => EntityID -> Access m (Maybe a)
remove e = Access $ do
  !w <- get
  let (a, w', hook) = W.remove @m e w
  put w'
  unAccess hook
  return a

despawn :: (Monad m) => EntityID -> Access m ()
despawn e = Access $ do
  !w <- get
  let !(_, w') = W.despawn e w
  put w'

-- | Run a `System` on the `World`.
system :: (Monad m) => System m a -> Access m a
system sys = Access $ do
  !w <- get
  let !es = W.entities w
      !(cs', dynSys) = S.runSystem sys $ E.components es
  (a, es', hook) <- lift $ S.runDynamicSystem dynSys es
  put w {W.entities = es' {E.components = cs'}}
  unAccess hook
  return a
{-# INLINE system #-}
