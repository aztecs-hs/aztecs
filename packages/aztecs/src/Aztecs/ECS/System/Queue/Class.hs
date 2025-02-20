module Aztecs.ECS.System.Queue.Class (ArrowQueueSystem (..)) where

import Aztecs.ECS.Access (Access)
import Control.Arrow (Arrow (..))

class (Arrow arr) => ArrowQueueSystem arr where
  -- | Queue an `Access` to happen after this system schedule.
  queue :: (i -> Access ()) -> arr i ()
