{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Aztecs.ECS.Access.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Access.Internal (Access (..), runAccessWith, evalAccess) where

import Aztecs.ECS.World.Internal (World)
import Control.Monad.Fix
import Control.Monad.State

-- | Access into a `World`.
newtype Access m a = Access {unAccess :: StateT (World m) m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

-- | Run an `Access` with a given `World`, returning the result and updated world.
runAccessWith :: Access m a -> World m -> m (a, World m)
runAccessWith a = runStateT (unAccess a)

-- | Run an `Access` with a given `World`, returning only the result.
evalAccess :: (Monad m) => Access m a -> World m -> m a
evalAccess a = evalStateT (unAccess a)
