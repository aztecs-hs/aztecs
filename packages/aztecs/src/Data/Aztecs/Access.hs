{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Access
  ( Access,
    AccessT (..),
    runAccessT,
    spawn,
    spawn_,
    insert,
    lookup,
    despawn,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (EntityID (..))
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Archetype (Bundle (..))
import Data.Data (Typeable)
import Prelude hiding (all, lookup, map)

type Access = AccessT Identity

-- | AccessT into the `World`.
newtype AccessT m a = AccessT {unAccessT :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccessT :: (Functor m) => AccessT m a -> World -> m (a, World)
runAccessT a = runStateT $ unAccessT a

-- | Spawn an entity with a component.
spawn ::
  (Monad m) =>
  Bundle ->
  AccessT m EntityID
spawn c = AccessT $ do
  !w <- get
  let !(e, w') = W.spawn c w
  put w'
  return e

spawn_ :: (Monad m) => Bundle -> AccessT m ()
spawn_ c = do
  _ <- spawn c
  return ()

-- | Insert a component into an entity.
insert :: (Monad m, Component a, Typeable (StorageT a)) => EntityID -> a -> AccessT m ()
insert e c = AccessT $ do
  !w <- get
  let !w' = W.insert e c w
  put w'

lookup :: (Monad m, Component a) => EntityID -> AccessT m (Maybe a)
lookup e = AccessT $ do
  !w <- get
  return $ W.lookup e w

despawn :: (Monad m) => EntityID -> AccessT m ()
despawn e = AccessT $ do
  !w <- get
  let !(_, w') = W.despawn e w
  put w'
