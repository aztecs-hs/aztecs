-- |
-- Module      : Aztecs.ECS.Query.Dynamic.Reader.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Dynamic.Reader.Class (DynamicQueryReaderF (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity

-- | Dynamic query reader functor.
class (Functor f) => DynamicQueryReaderF f where
  -- | Fetch the currently matched `EntityID`.
  --
  -- @since 0.10
  entity :: f EntityID

  -- | Fetch a `Component` by its `ComponentID`.
  --
  -- @since 0.10
  fetchDyn :: (Component a) => ComponentID -> f a

  -- | Try to fetch a `Component` by its `ComponentID`.
  --
  -- @since 0.10
  fetchMaybeDyn :: (Component a) => ComponentID -> f (Maybe a)
  fetchMaybeDyn cId = Just <$> fetchDyn cId
