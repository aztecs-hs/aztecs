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

import Aztecs.ECS.Query (QueryFilter (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack
import Prelude hiding (all)

-- | Monadic reader system.
class (Monad m) => MonadReaderSystem q m | m -> q where
  -- | Match all entities with a query.
  --
  -- @since 0.9
  all :: q a -> m (Vector a)

  -- | Match a single entity with a query.
  --
  -- @since 0.9
  single :: (HasCallStack) => q a -> m a
  single q = do
    os <- all q
    case V.length os of
      1 -> return $ V.head os
      _ -> error "single: expected a single result"

  -- | Match a single entity with a query, or @Nothing@.
  --
  -- @since 0.9
  singleMaybe :: q a -> m (Maybe a)
  singleMaybe q = do
    os <- all q
    return $ case V.length os of
      1 -> Just (V.head os)
      _ -> Nothing

  -- | Match all entities with a query and filter.
  --
  -- @since 0.9
  filter :: q a -> QueryFilter -> m (Vector a)
