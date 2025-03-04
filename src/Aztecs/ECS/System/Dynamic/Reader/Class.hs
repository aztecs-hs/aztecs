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

import Aztecs.ECS.Component
import Aztecs.ECS.World.Archetypes (Node)
import Data.Set (Set)
import GHC.Stack

-- | Monadic dynamic reader system.
--
-- @since 0.9
class (Monad m) => MonadDynamicReaderSystem q m | m -> q where
  -- | Match all entities with a query.
  --
  -- @since 0.9
  allDyn :: i -> Set ComponentID -> q i o -> m [o]

  -- | Match a single entity with a query.
  --
  -- @since 0.9
  singleDyn :: (HasCallStack) => i -> Set ComponentID -> q i o -> m o
  singleDyn i cIds q = do
    os <- allDyn i cIds q
    case os of
      [o] -> return o
      _ -> error "singleDyn: expected a single result, but got multiple"

  -- | Match a single entity with a query, or Nothing.
  --
  -- @since 0.9
  singleMaybeDyn :: i -> Set ComponentID -> q i o -> m (Maybe o)
  singleMaybeDyn i cIds q = do
    os <- allDyn i cIds q
    return $ case os of
      [o] -> Just o
      _ -> Nothing

  -- | Match all entities with a query and filter.
  --
  -- @since 0.9
  filterDyn :: i -> Set ComponentID -> q i a -> (Node -> Bool) -> m [a]
