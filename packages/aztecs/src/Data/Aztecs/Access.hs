{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Access
  ( Access (..),
    runAccess,
    spawn,
    spawn_,
    insert,
    despawn,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (EntityID (..))
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Archetype (Bundle (..))
import Data.Data (Typeable)
import Prelude hiding (all, lookup, map)

-- | Access into the `World`.
newtype Access m a = Access {unAccess :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccess :: (Functor m) => Access m a -> World -> m (a, World)
runAccess a = runStateT $ unAccess a

-- | Spawn an entity with a component.
spawn ::
  (Monad m) =>
  Bundle ->
  Access m EntityID
spawn c = Access $ do
  w <- get
  let (e, w') = W.spawn c w
  put w'
  return e

spawn_ :: (Monad m) => Bundle -> Access m ()
spawn_ c = do
  _ <- spawn c
  return ()

-- | Insert a component into an entity.
insert :: (Monad m, Component a, Typeable (StorageT a)) => EntityID -> a -> Access m ()
insert e c = Access $ do
  w <- get
  let w' = W.insert e c w
  put w'

despawn :: (Monad m) => EntityID -> Access m ()
despawn e = Access $ do
  w <- get
  let (_, w') = W.despawn e w
  put w'
