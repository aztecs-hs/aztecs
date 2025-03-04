{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Aztecs.ECS.World.Bundle.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Bundle.Class (MonoidBundle (..)) where

import Aztecs.ECS.Component

-- | Monoid bundle of components.
--
-- @since 9.0
class (Monoid a) => MonoidBundle a where
  -- | Add a component to the bundle.
  --
  -- @since 9.0
  bundle :: forall c. (Component c) => c -> a
