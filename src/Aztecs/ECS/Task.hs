{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.Task where

import Control.Arrow

class (Arrow arr) => ArrowTask m arr | arr -> m where
  task :: (i -> m o) -> arr i o
