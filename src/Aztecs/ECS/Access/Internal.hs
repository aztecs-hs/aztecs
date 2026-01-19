{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Aztecs.ECS.Access.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Access.Internal (AccessT (..), runAccessTWith, evalAccessT) where

import Aztecs.ECS.World.Internal (World)
import Control.Monad.Fix
import Control.Monad.State

-- | Access into a `World`.
newtype AccessT m a = AccessT {unAccessT :: StateT (World m) m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

-- | Run an `AccessT` with a given `World`, returning the result and updated world.
runAccessTWith :: AccessT m a -> World m -> m (a, World m)
runAccessTWith a = runStateT (unAccessT a)

-- | Run an `AccessT` with a given `World`, returning only the result.
evalAccessT :: (Monad m) => AccessT m a -> World m -> m a
evalAccessT a = evalStateT (unAccessT a)
