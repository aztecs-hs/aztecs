{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Aztecs.ECS.System.Reader.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.System.Reader.Class (MonadReaderSystem (..)) where

import Aztecs.ECS.Query.Reader (QueryFilter (..))
import GHC.Stack
import Prelude hiding (all)

-- | Monadic reader system.
--
-- @since 9.0
class (Monad m) => MonadReaderSystem q m | m -> q where
  -- | Match all entities with a query.
  --
  -- @since 9.0
  all :: i -> q i o -> m [o]

  -- | Match a single entity with a query.
  --
  -- @since 9.0
  single :: (HasCallStack) => i -> q i o -> m o
  single i q = do
    os <- all i q
    case os of
      [o] -> return o
      _ -> error "single: expected a single result"

  -- | Match a single entity with a query, or @Nothing@.
  --
  -- @since 9.0
  singleMaybe :: i -> q i o -> m (Maybe o)
  singleMaybe i q = do
    os <- all i q
    return $ case os of
      [o] -> Just o
      _ -> Nothing

  -- | Match all entities with a query and filter.
  --
  -- @since 9.0
  filter :: i -> q i o -> QueryFilter -> m [o]
