module Aztecs.ECS.W (W (..)) where

-- | Read-write 'Queryable' component access.
data W m c = W
  { readW :: m c,
    writeW :: c -> m (),
    modifyW :: (c -> c) -> m ()
  }
