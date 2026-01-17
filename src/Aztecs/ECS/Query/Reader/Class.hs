-- |
-- Module      : Aztecs.ECS.Query.Reader.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Reader.Class (QueryReaderF (..)) where

import Aztecs.ECS.Component

-- | Query reader functor.
--
-- @since 0.10
class (Functor f) => QueryReaderF f where
  -- | Fetch a `Component` by its type.
  --
  -- @since 0.10
  fetch :: (Component a) => f a

  -- | Fetch a `Component` by its type, returning `Nothing` if it doesn't exist.
  --
  -- @since 0.10
  fetchMaybe :: (Component a) => f (Maybe a)
  fetchMaybe = Just <$> fetch
