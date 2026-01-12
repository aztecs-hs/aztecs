{-# LANGUAGE RoleAnnotations #-}

module Aztecs.ECS.W (W (..), Runner (..), runRunner, liftRunner) where

import Aztecs.ECS.Runner (Runner (..), liftRunner, runRunner)

-- | Read-write 'Queryable' component access.
data W s m c = W
  { readW :: !(Runner s m c),
    writeW :: !(c -> Runner s m ()),
    modifyW :: !((c -> c) -> Runner s m ())
  }

type role W phantom representational nominal
