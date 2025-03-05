{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Aztecs.ECS.System.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.System.Class (MonadSystem (..)) where

import Aztecs.ECS.Query.Reader (QueryFilter (..))
import GHC.Stack
import Prelude hiding (map)

-- | Monadic system.
--
-- @since 0.9
class (Monad m) => MonadSystem q m | m -> q where
  -- | Map all matching entities with a query.
  --
  -- @since 0.9
  map :: q a -> m [a]

  -- | Map a single matching entity with a query, or @Nothing@.
  --
  -- @since 0.9
  mapSingleMaybe :: q a -> m (Maybe a)

  -- | Map a single matching entity with a query.
  --
  -- @since 0.9
  mapSingle :: (HasCallStack) => q a -> m a
  mapSingle q = do
    res <- mapSingleMaybe q
    case res of
      Just a -> return a
      Nothing -> error "Expected a single matching entity."

  -- | Map all matching entities with a query and filter.
  --
  -- @since 0.9
  filterMap :: q a -> QueryFilter -> m [a]
