{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.Schedule.Class (ArrowSchedule (..)) where

import Control.Arrow (Arrow (..))

-- | Schedule arrow that runs systems.
class (Arrow arr) => ArrowSchedule s arr | arr -> s where
  -- | Schedule a system.
  system :: s i o -> arr i o
