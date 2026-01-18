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

import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.World.Archetypes (Node (..))
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.Stack

-- | Monadic dynamic system.
class (Monad m) => MonadDynamicSystem q m | m -> q where
  -- | Map all matching entities with a query.
  --
  -- @since 0.9
  mapDyn :: Set ComponentID -> q a -> m (Vector a)

  -- | Map a single matching entity with a query, or @Nothing@.
  --
  -- @since 0.9
  mapSingleMaybeDyn :: Set ComponentID -> q a -> m (Maybe a)

  -- | Map a single matching entity with a query.
  mapSingleDyn :: (HasCallStack) => Set ComponentID -> q a -> m a
  mapSingleDyn cIds q = do
    res <- mapSingleMaybeDyn cIds q
    case res of
      Just a -> return a
      Nothing -> error "Expected a single matching entity."

  -- | Map all matching entities with a query and filter.
  --
  -- @since 0.9
  filterMapDyn :: Set ComponentID -> (Node -> Bool) -> q a -> m (Vector a)
