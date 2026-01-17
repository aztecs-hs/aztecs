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
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack

-- | Monadic dynamic reader system.
--
-- @since 0.9
class (Monad m) => MonadDynamicReaderSystem q m | m -> q where
  -- | Match all entities with a query.
  --
  -- @since 0.9
  allDyn :: Set ComponentID -> q a -> m (Vector a)

  -- | Match a single entity with a query.
  --
  -- @since 0.9
  singleDyn :: (HasCallStack) => Set ComponentID -> q a -> m a
  singleDyn cIds q = do
    os <- allDyn cIds q
    case V.length os of
      1 -> return $ V.head os
      _ -> error "singleDyn: expected a single result, but got multiple"

  -- | Match a single entity with a query, or Nothing.
  --
  -- @since 0.9
  singleMaybeDyn :: Set ComponentID -> q a -> m (Maybe a)
  singleMaybeDyn cIds q = do
    os <- allDyn cIds q
    return $ case V.length os of
      1 -> Just (V.head os)
      _ -> Nothing

  -- | Match all entities with a query and filter.
  --
  -- @since 0.9
  filterDyn :: Set ComponentID -> q a -> (Node -> Bool) -> m (Vector a)
