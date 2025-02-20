{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.Schedule.Class (ArrowSchedule (..)) where

import Control.Arrow (Arrow (..))

class (Arrow arr) => ArrowSchedule s arr | arr -> s where
  system :: s i o -> arr i o
