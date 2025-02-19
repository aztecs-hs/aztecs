{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Access
  ( Access,
    AccessT (..),
    MonadAccess (..),
    runAccessT,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Data.Aztecs.Access.Class (MonadAccess (..))
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Prelude hiding (all, lookup, map)

type Access = AccessT Identity

-- | Access into the `World`.
newtype AccessT m a = AccessT {unAccessT :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccessT :: (Functor m) => AccessT m a -> World -> m (a, World)
runAccessT a = runStateT $ unAccessT a

instance (Monad m) => MonadAccess (AccessT m) where
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
  despawn e = AccessT $ do
    !w <- get
    let !(_, w') = W.despawn e w
    put w'
