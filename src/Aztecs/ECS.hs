{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS
  ( module Aztecs.ECS.Queryable,
    module Aztecs.ECS.Queryable.R,
    module Aztecs.ECS.Queryable.W,
    module Aztecs.ECS.Schedule,
    PrimMonad (..),
    bundle,
    Query (..),
    runQuery,
    System (..),
    runSystemWithWorld,
    ECS (..),
    AztecsT (..),
    runAztecsT_,
    runSchedule,
  )
where

import Aztecs.ECS.Access.Internal
import qualified Aztecs.ECS.Access.Internal as A
import Aztecs.ECS.Class
import qualified Aztecs.ECS.Entities as E
import Aztecs.ECS.Executor
import Aztecs.ECS.HSet (HSet (..))
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.Queryable.R
import Aztecs.ECS.Queryable.W
import Aztecs.ECS.Schedule
import Aztecs.ECS.Scheduler hiding (runSchedule)
import qualified Aztecs.ECS.Scheduler as Scheduler
import Aztecs.ECS.System
import Aztecs.ECS.World (World, bundle)
import qualified Aztecs.ECS.World as W
import Control.Monad.Identity
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
  {-# INLINE spawn #-}
  insert e b = AztecsT $ do
    w <- get
    w' <- lift $ W.insert e b w
    put w'
  {-# INLINE insert #-}
  remove e = AztecsT $ do
    w <- get
    w' <- lift $ W.remove e w
    put w'
  {-# INLINE remove #-}
  query = AztecsT $ do
    w <- get
    return $ W.query w
  {-# INLINE query #-}
  task = AztecsT . lift
  {-# INLINE task #-}
  access = AztecsT $ do
    w <- get
    return $ A.access w
  {-# INLINE access #-}

runAztecsT_ :: (Monad m) => AztecsT cs m a -> World m cs -> m a
runAztecsT_ (AztecsT m) = evalStateT m
{-# INLINE runAztecsT_ #-}

runSchedule ::
  forall m cs s.
  ( Scheduler m s,
    Execute (World m cs) m (SchedulerOutput m s),
    s ~ HSet Identity (SchedulerInput m s),
    Monad m,
    AllSystems m (SchedulerInput m s),
    ScheduleLevelsBuilder
      m
      (TopologicalSort (BuildSystemGraph (SchedulerInput m s)))
      (SchedulerInput m s)
  ) =>
  s ->
  AztecsT cs m ()
runSchedule s = AztecsT $ do
  w <- get
  lift $ Scheduler.runSchedule @m @cs s w
