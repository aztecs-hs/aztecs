{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Access
  ( Access,
    AccessT (..),
    MonadAccess (..),
    runAccessT,
    runSystem,
    runSystemOnce,
    runDynSystem,
    buildSystem,
    runDynSystemOnce,
  )
where

import Aztecs.ECS.Access.Class
import Aztecs.ECS.System (SystemT)
import qualified Aztecs.ECS.System as S
import Aztecs.ECS.System.Dynamic
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import qualified Aztecs.ECS.World as W
import Aztecs.ECS.World.Bundle
import qualified Aztecs.ECS.World.Entities as E
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.State.Strict

type Access = AccessT Identity

-- | Access into the `World`.
newtype AccessT m a = AccessT {unAccessT :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

-- | Run an `Access` on a `World`, returning the output and updated `World`.
runAccessT :: (Functor m) => AccessT m a -> World -> m (a, World)
runAccessT a = runStateT $ unAccessT a

instance (Monad m) => MonadAccess Bundle (AccessT m) where
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
  remove e = AccessT $ do
    !w <- get
    let !(a, w') = W.remove e w
    put w'
    return a
  despawn e = AccessT $ do
    !w <- get
    let !(_, w') = W.despawn e w
    put w'

buildSystem :: (Monad m) => SystemT m i o -> AccessT m (DynamicSystemT m i o)
buildSystem s = AccessT $ do
  w <- get
  let (dynS, _, cs) = S.runSystem s . E.components $ W.entities w
  put w {W.entities = (W.entities w) {E.components = cs}}
  return dynS

runDynSystem :: (Monad m) => (o -> m ()) -> DynamicSystemT m i o -> i -> AccessT m ()
runDynSystem f s i = do
  let go s' = do
        (o, s'') <- runDynSystemOnce s' i
        AccessT . lift $ f o
        go s''
  go s

runDynSystemOnce :: (Monad m) => DynamicSystemT m i o -> i -> AccessT m (o, DynamicSystemT m i o)
runDynSystemOnce s i = AccessT $ do
  w <- get
  (o, v, s') <- lift $ runSystemDyn s (W.entities w) i
  put w {W.entities = V.unview v $ W.entities w}
  return (o, s')

runSystem :: (Monad m) => (o -> m ()) -> i -> SystemT m i o -> AccessT m ()
runSystem f i s = do
  dynS <- buildSystem s
  runDynSystem f dynS i

runSystemOnce :: (Monad m) => i -> SystemT m i o -> AccessT m o
runSystemOnce i s = do
  dynS <- buildSystem s
  (o, _) <- runDynSystemOnce dynS i
  return o
