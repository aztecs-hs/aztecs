{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Executor where

import Aztecs.ECS.HSet
import Aztecs.ECS.Query.Class (Queryable)
import Aztecs.ECS.System

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
  ( System m sys,
    Queryable m () (SystemIn m () sys)
  ) =>
  Execute' m (HSet '[sys])
  where
  execute' (HCons sys HEmpty) = [withSystemIn sys (runSystem sys)]
  {-# INLINE execute' #-}

instance
  {-# OVERLAPPABLE #-}
  ( System m sys,
    Queryable m () (SystemIn m () sys),
    Execute' m (HSet systems)
  ) =>
  Execute' m (HSet (sys ': systems))
  where
  execute' (HCons s rest) = withSystemIn s (runSystem s) : execute' rest
  {-# INLINE execute' #-}

class Execute m s where
  execute :: s -> ExecutorT m ()

instance (Applicative m) => Execute m (HSet '[]) where
  execute _ = pure ()
  {-# INLINE execute #-}

instance
  {-# OVERLAPPING #-}
  (Monad m, Execute' m systems, Execute m (HSet schedule)) =>
  Execute m (HSet (systems ': schedule))
  where
  execute (HCons system rest) = do
    ExecutorT $ \run -> run $ execute' system
    execute rest
  {-# INLINE execute #-}
