{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS
  ( module Aztecs.ECS.Queryable,
    module Aztecs.ECS.Queryable.R,
    module Aztecs.ECS.Queryable.W,
    PrimMonad (..),
    bundle,
    Query(..),
    ECS (..),
    AztecsT (..),
    runAztecsT_,
  )
where

import qualified Aztecs.ECS.Access.Internal as A
import Aztecs.ECS.Class
import qualified Aztecs.ECS.Entities as E
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.R
import Aztecs.ECS.Queryable.W
import Aztecs.ECS.World (World, bundle)
import qualified Aztecs.ECS.World as W
import Control.Monad.Primitive
import Control.Monad.State.Strict

newtype AztecsT cs m a = AztecsT {unAztecsT :: StateT (World m cs) m a}
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance MonadTrans (AztecsT cs) where
  lift = AztecsT . lift

instance (PrimMonad m) => ECS (AztecsT cs m) where
  type Entity (AztecsT cs m) = E.Entity
  type Bundle (AztecsT cs m) = W.Bundle cs m
  type Components (AztecsT cs m) = cs
  type Task (AztecsT cs m) = m

  spawn b = AztecsT $ do
    w <- get
    (e, w') <- lift $ W.spawn b w
    put w'
    return e
  insert e b = AztecsT $ do
    w <- get
    w' <- lift $ W.insert e b w
    put w'
  remove e = AztecsT $ do
    w <- get
    w' <- lift $ W.remove e w
    put w'
  query = AztecsT $ do
    w <- get
    return $ W.query w
  task = AztecsT . lift
  access = AztecsT $ do
    w <- get
    return $ A.access w

runAztecsT_ :: (Monad m) => AztecsT cs m a -> World m cs -> m a
runAztecsT_ (AztecsT m) = evalStateT m
