-- |
-- Module      : Aztecs.ECS.Query.Reader.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Reader.Class (ArrowQueryReader (..)) where

import Aztecs.ECS.Component
import Control.Arrow

-- | Arrow for queries that can read from entities.
--
-- @since 9.0
class (Arrow arr) => ArrowQueryReader arr where
  -- | Fetch a `Component` by its type.
  --
  -- @since 9.0
  fetch :: (Component a) => arr () a

  -- | Fetch a `Component` by its type, returning `Nothing` if it doesn't exist.
  --
  -- @since 9.0
  fetchMaybe :: (Component a) => arr () (Maybe a)
  fetchMaybe = fetch >>> arr Just
