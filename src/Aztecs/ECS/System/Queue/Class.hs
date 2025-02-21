{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Queue.Class (ArrowQueueSystem (..)) where

import Aztecs.ECS.Access (MonadAccess)
import Control.Arrow (Arrow (..))

class (MonadAccess b m, Arrow arr) => ArrowQueueSystem b m arr | arr -> m where
  -- | Queue an `Access` to happen after this system schedule.
  queue :: (i -> m ()) -> arr i ()
