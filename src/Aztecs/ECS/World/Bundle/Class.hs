{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Aztecs.ECS.World.Bundle.Class (MonoidBundle (..)) where

import Aztecs.ECS.Component (Component (..))

-- | Monoid bundle of components.
class (Monoid a) => MonoidBundle a where
  -- | Add a component to the bundle.
  bundle :: forall c. (Component c) => c -> a
