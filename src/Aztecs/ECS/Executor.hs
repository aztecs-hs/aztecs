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

newtype ExecutorT i m a = ExecutorT {runSystems :: ([i -> m ()] -> m ()) -> m a}
  deriving (Functor)

instance (Applicative m) => Applicative (ExecutorT i m) where
  pure x = ExecutorT $ \_ -> pure x
  ExecutorT f <*> ExecutorT g = ExecutorT $ \run -> f run <*> g run

instance (Monad m) => Monad (ExecutorT i m) where
  ExecutorT f >>= g = ExecutorT $ \run -> f run >>= \x -> runSystems (g x) run

class Execute' i m s where
  execute' :: s -> [i -> m ()]

instance Execute' (World m cs) m (HSet Identity '[]) where
  execute' _ = []

instance
  ( Monad m,
    System m sys,
    Access cs m (SystemInputs m sys),
    Subset (AccessToComponents (AccessType (SystemInputs m sys))) cs,
    ValidAccessInput (AccessType (SystemInputs m sys))
  ) =>
  Execute' (World m cs) m (Identity sys)
  where
  execute' (Identity system) =
    [ \world -> do
        let inputs = access world
        runSystem system inputs
    ]

instance
  ( Monad m,
    System m sys,
    Access cs m (SystemInputs m sys),
    Subset (AccessToComponents (AccessType (SystemInputs m sys))) cs,
    ValidAccessInput (AccessType (SystemInputs m sys)),
    Execute' (World m cs) m (HSet Identity systems)
  ) =>
  Execute' (World m cs) m (HSet Identity (sys ': systems))
  where
  execute' (HCons (Identity system) rest) =
    [ \world -> do
        let inputs = access world
        runSystem system inputs
    ]
      ++ execute' rest

class Execute i m s where
  execute :: s -> ExecutorT i m ()

instance (Applicative m) => Execute (World m cs) m (HSet Identity '[]) where
  execute _ = pure ()

instance
  ( Monad m,
    Execute' (World m cs) m (Identity systems),
    Execute (World m cs) m (HSet Identity schedule)
  ) =>
  Execute (World m cs) m (HSet Identity (systems ': schedule))
  where
  execute (HCons system rest) = do
    ExecutorT $ \run -> run $ execute' system
    execute rest
