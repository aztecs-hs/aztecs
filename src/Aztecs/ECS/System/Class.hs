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
-- @since 9.0
class (Monad m) => MonadSystem q m | m -> q where
  -- | Map all matching entities with a query.
  --
  -- @since 9.0
  map :: i -> q i o -> m [o]

  -- | Map a single matching entity with a query, or @Nothing@.
  --
  -- @since 9.0
  mapSingleMaybe :: i -> q i o -> m (Maybe o)

  -- | Map a single matching entity with a query.
  --
  -- @since 9.0
  mapSingle :: (HasCallStack) => i -> q i o -> m o
  mapSingle i q = do
    res <- mapSingleMaybe i q
    case res of
      Just a -> return a
      Nothing -> error "Expected a single matching entity."

  -- | Map all matching entities with a query and filter.
  --
  -- @since 9.0
  filterMap :: i -> q i o -> QueryFilter -> m [o]
