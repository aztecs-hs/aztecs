{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.Schedule.Reader.Class (ArrowReaderSchedule (..)) where

import Control.Arrow (Arrow (..))

-- | Schedule arrow that runs read-only systems.
class (Arrow arr) => ArrowReaderSchedule s arr | arr -> s where
  -- | Schedule a reader system.
  reader :: s i o -> arr i o
