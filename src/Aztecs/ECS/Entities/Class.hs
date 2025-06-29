module Aztecs.ECS.Entities.Class where

import Aztecs.ECS
import Aztecs.ECS.Query

class (Monad m) => MonadEntities m where
  spawn :: m Entity
  entities :: Query m Entity
