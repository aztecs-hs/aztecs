{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aztecs.Access (Access (..), runAccess, spawn, insert) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.Aztecs.Core (Component (..), EntityID)
import Data.Aztecs.World (World)
import qualified Data.Aztecs.World as W
import Data.Data (Typeable)

-- | Access into the `World`.
newtype Access m a = Access {unAccess :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccess :: Access m a -> World -> m (a, World)
runAccess a = runStateT (unAccess a)

-- | Spawn an entity with a component.
spawn :: (Monad m, Component a, Typeable (StorageT a)) => a -> Access m EntityID
spawn c = Access $ do
  w <- get
  let (e, w') = W.spawn c w
  put w'
  return e

-- | Insert a component into an entity.
insert :: (Monad m, Component a, Typeable (StorageT a)) => EntityID -> a -> Access m ()
insert e c = Access $ do
  w <- get
  let w' = W.insert e c w
  put w'
