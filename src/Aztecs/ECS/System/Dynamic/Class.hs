{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Aztecs.ECS.System.Dynamic.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.System.Dynamic.Class (MonadDynamicSystem (..)) where

import Aztecs.ECS.World.Archetypes (Node (..))
import GHC.Stack

-- | Monadic dynamic system.
--
-- @since 0.10
class (Monad m) => MonadDynamicSystem q m | m -> q where
  -- | Map all matching entities with a query.
  --
  -- @since 0.10
  mapDyn :: q a -> m [a]

  -- | Map a single matching entity with a query, or @Nothing@.
  --
  -- @since 0.10
  mapSingleMaybeDyn :: q a -> m (Maybe a)

  -- | Map a single matching entity with a query.
  mapSingleDyn :: (HasCallStack) => q a -> m a
  mapSingleDyn q = do
    res <- mapSingleMaybeDyn q
    case res of
      Just a -> return a
      Nothing -> error "Expected a single matching entity."

  -- | Map all matching entities with a query and filter.
  --
  -- @since 0.10
  filterMapDyn :: (Node -> Bool) -> q a -> m [a]
