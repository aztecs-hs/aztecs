-- |
-- Module      : Aztecs.ECS.Query.Dynamic.Reader.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Control.Arrow

-- | Arrow dynamic query reader.
--
-- @since 9.0
class (Arrow arr) => ArrowDynamicQueryReader arr where
  -- | Fetch the currently matched `EntityID`.
  --
  -- @since 9.0
  entity :: arr () EntityID

  -- | Fetch a `Component` by its `ComponentID`.
  --
  -- @since 9.0
  fetchDyn :: (Component a) => ComponentID -> arr () a

  -- | Try to fetch a `Component` by its `ComponentID`.
  --
  -- @since 9.0
  fetchMaybeDyn :: (Component a) => ComponentID -> arr () (Maybe a)
  fetchMaybeDyn cId = fetchDyn cId >>> arr Just
