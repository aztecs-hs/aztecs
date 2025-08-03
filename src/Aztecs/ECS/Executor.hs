{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Executor where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.HSet (HSet (..), Subset)
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.System
import Aztecs.ECS.World
import Control.Monad.Identity

newtype ExecutorT m a = ExecutorT {runSystems :: ([m ()] -> m ()) -> m a}
  deriving (Functor)

instance (Applicative m) => Applicative (ExecutorT m) where
  pure x = ExecutorT $ \_ -> pure x
  ExecutorT f <*> ExecutorT g = ExecutorT $ \run -> f run <*> g run

instance (Monad m) => Monad (ExecutorT m) where
  ExecutorT f >>= g = ExecutorT $ \run -> f run >>= \x -> runSystems (g x) run

class Execute' m s where
  execute' :: s -> [m ()]

instance Execute' m (HSet Identity '[]) where
  execute' _ = []

instance
  ( Monad m,
    System m sys,
    Access m (SystemInputs m sys),
    ValidAccessInput (AccessType (SystemInputs m sys))
  ) =>
  Execute' m (Identity sys)
  where
  execute' (Identity system) =
    [ do
        inputs <- access
        runSystem system inputs
    ]

instance
  ( Monad m,
    System m sys,
    Access m (SystemInputs m sys),
    ValidAccessInput (AccessType (SystemInputs m sys)),
    Execute' m (HSet Identity systems)
  ) =>
  Execute' m (HSet Identity (sys ': systems))
  where
  execute' (HCons (Identity system) rest) =
    [ do
        inputs <- access
        runSystem system inputs
    ]
      ++ execute' rest

class Execute m s where
  execute :: s -> ExecutorT m ()

instance (Applicative m) => Execute m (HSet Identity '[]) where
  execute _ = pure ()

instance
  ( Monad m,
    Execute' m (Identity systems),
    Execute m (HSet Identity schedule)
  ) =>
  Execute m (HSet Identity (systems ': schedule))
  where
  execute (HCons system rest) = do
    ExecutorT $ \run -> run $ execute' system
    execute rest
