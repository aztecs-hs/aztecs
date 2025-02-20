{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.Schedule.Access (AcessSchedule (..), ArrowAccessSchedule (..)) where

import Aztecs.ECS.Access (AccessT (..))
import Aztecs.ECS.Schedule (ArrowAccessSchedule (..))
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))

newtype AcessSchedule m i o = AcessSchedule {runAcessSchedule :: i -> AccessT m o}
  deriving (Functor)

instance (Monad m) => Category (AcessSchedule m) where
  id = AcessSchedule return
  AcessSchedule f . AcessSchedule g = AcessSchedule $ (g Control.Monad.>=> f)

instance (Monad m) => Arrow (AcessSchedule m) where
  arr f = AcessSchedule $ \i -> return $ f i
  first (AcessSchedule f) = AcessSchedule $ \(b, d) -> do
    c <- f b
    return (c, d)

instance (Monad m) => ArrowAccessSchedule (AccessT m) (AcessSchedule m) where
  access = AcessSchedule
