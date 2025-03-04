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
import GHC.Stack

-- | Monadic dynamic system.
--
-- @since 9.0
class (Monad m) => MonadDynamicSystem q m | m -> q where
  -- | Map all matching entities with a query.
  --
  -- @since 9.0
  mapDyn :: i -> Set ComponentID -> q i o -> m [o]

  -- | Map a single matching entity with a query, or @Nothing@.
  --
  -- @since 9.0
  mapSingleMaybeDyn :: i -> Set ComponentID -> q i a -> m (Maybe a)

  -- | Map a single matching entity with a query.
  mapSingleDyn :: (HasCallStack) => i -> Set ComponentID -> q i o -> m o
  mapSingleDyn i cIds q = do
    res <- mapSingleMaybeDyn i cIds q
    case res of
      Just a -> return a
      Nothing -> error "Expected a single matching entity."

  -- | Map all matching entities with a query and filter.
  --
  -- @since 9.0
  filterMapDyn :: i -> Set ComponentID -> (Node -> Bool) -> q i a -> m [a]
