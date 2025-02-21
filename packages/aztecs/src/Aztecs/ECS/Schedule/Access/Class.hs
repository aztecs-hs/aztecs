{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.Schedule.Access.Class (ArrowAccessSchedule (..)) where

import Aztecs.ECS.Access (MonadAccess)
import Control.Arrow (Arrow (..))

-- | Schedule arrow that provides access to a `World`.
class (MonadAccess b m, Arrow arr) => ArrowAccessSchedule b m arr | arr -> m where
  -- | Access the `World`.
  access :: (i -> m o) -> arr i o
