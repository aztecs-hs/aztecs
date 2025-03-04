-- |
-- Module      : Aztecs.ECS.World.Bundle.Dynamic.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Bundle.Dynamic.Class (MonoidDynamicBundle (..)) where

import Aztecs.ECS.Component

-- | Monoid bundle of dynamic components.
--
-- @since 0.9
class MonoidDynamicBundle a where
  -- | Add a component to the bundle by its `ComponentID`.
  --
  -- @since 0.9
  dynBundle :: (Component c) => ComponentID -> c -> a
