{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.Schedule.Access.Class (ArrowAccessSchedule (..)) where

import Aztecs.ECS.Access (MonadAccess)
import Control.Arrow (Arrow (..))

class (MonadAccess b m, Arrow arr) => ArrowAccessSchedule b m arr | arr -> m where
  access :: (i -> m o) -> arr i o
