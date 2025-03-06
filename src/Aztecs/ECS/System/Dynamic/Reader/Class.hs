{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Aztecs.ECS.System.Dynamic.Reader.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.System.Dynamic.Reader.Class (MonadDynamicReaderSystem (..)) where

import Aztecs.ECS.World.Archetypes (Node)
import GHC.Stack

-- | Monadic dynamic reader system.
--
-- @since 0.11
class (Monad m) => MonadDynamicReaderSystem q m | m -> q where
  -- | Match all entities with a query.
  --
  -- @since 0.11
  allDyn :: q a -> m [a]

  -- | Match a single entity with a query.
  --
  -- @since 0.11
  singleDyn :: (HasCallStack) => q a -> m a
  singleDyn q = do
    os <- allDyn q
    case os of
      [o] -> return o
      _ -> error "singleDyn: expected a single result, but got multiple"

  -- | Match a single entity with a query, or Nothing.
  --
  -- @since 0.11
  singleMaybeDyn :: q a -> m (Maybe a)
  singleMaybeDyn q = do
    os <- allDyn q
    return $ case os of
      [o] -> Just o
      _ -> Nothing

  -- | Match all entities with a query and filter.
  --
  -- @since 0.11
  filterDyn :: q a -> (Node -> Bool) -> m [a]
