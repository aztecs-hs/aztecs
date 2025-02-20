{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.Schedule.Reader.Class (ArrowReaderSchedule (..)) where

import Control.Arrow (Arrow (..))

class (Arrow arr) => ArrowReaderSchedule s arr | arr -> s where
  reader :: s i o -> arr i o
