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
import Aztecs.World

newtype ExecutorT m a = ExecutorT {runSystems :: ([m ()] -> m ()) -> m a}
  deriving (Functor)

instance (Applicative m) => Applicative (ExecutorT m) where
  pure x = ExecutorT $ \_ -> pure x
  {-# INLINE pure #-}
  ExecutorT f <*> ExecutorT g = ExecutorT $ \run -> f run <*> g run
  {-# INLINE (<*>) #-}

instance (Monad m) => Monad (ExecutorT m) where
  ExecutorT f >>= g = ExecutorT $ \run -> f run >>= \x -> runSystems (g x) run
  {-# INLINE (>>=) #-}

class Execute' m s where
  execute' :: s -> [m ()]

instance Execute' m (HSet '[]) where
  execute' _ = []
  {-# INLINE execute' #-}

instance 
  {-# OVERLAPS #-}
  ( Monad m,
    System m sys,
    Access m (SystemIn m sys),
    ValidAccessInput (AccessType (SystemIn m sys))
  ) =>
  Execute' m (HSet '[sys])
  where
  execute' (HCons system HEmpty) =
    [ do
        inputs <- access
        runSystem system inputs
    ]
  {-# INLINE execute' #-}

instance 
  {-# OVERLAPPABLE #-}
  ( Monad m,
    System m sys,
    Access m (SystemIn m sys),
    ValidAccessInput (AccessType (SystemIn m sys)),
    Execute' m (HSet systems)
  ) =>
  Execute' m (HSet (sys ': systems))
  where
  execute' (HCons system rest) =
    ( do
        inputs <- access
        runSystem system inputs
    )
      : execute' rest
  {-# INLINE execute' #-}

class Execute m s where
  execute :: s -> ExecutorT m ()

instance (Applicative m) => Execute m (HSet '[]) where
  execute _ = pure ()
  {-# INLINE execute #-}

instance
  {-# OVERLAPPING #-}
  ( Monad m,
    Execute' m systems,
    Execute m (HSet schedule)
  ) =>
  Execute m (HSet (systems ': schedule))
  where
  execute (HCons system rest) = do
    ExecutorT $ \run -> run $ execute' system
    execute rest
  {-# INLINE execute #-}
