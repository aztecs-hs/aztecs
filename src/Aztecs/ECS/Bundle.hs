module Aztecs.ECS.Bundle where

-- | Bundle of components that can be stored in an entity.
newtype Bundle e m = Bundle {runBundle :: e -> m ()}

instance (Monad m) => Semigroup (Bundle e m) where
  Bundle f <> Bundle g = Bundle $ \entity -> f entity >> g entity
  {-# INLINE (<>) #-}

instance (Monad m) => Monoid (Bundle e m) where
  mempty = Bundle $ \_ -> return ()
  {-# INLINE mempty #-}
