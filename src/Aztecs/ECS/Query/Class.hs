{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Aztecs.ECS.Query.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Class (QueryF (..)) where

import Aztecs.ECS.Component
import Control.Monad

-- | Query functor.
--
-- @since 0.10
class (Applicative g, Functor f) => QueryF g f | f -> g where
  -- | Adjust a `Component` by its type.
  --
  -- @since 0.10
  adjust :: (Component a) => (b -> a -> a) -> f b -> f a

  -- | Adjust a `Component` by its type, ignoring any output.
  --
  -- @since 0.10
  adjust_ :: (Component a) => (b -> a -> a) -> f b -> f ()
  adjust_ f = void . adjust f

  -- | Adjust a `Component` by its type with some `Monad` @m@.
  --
  -- @since 0.10
  adjustM :: (Component a) => (b -> a -> g a) -> f b -> f a

  -- | Set a `Component` by its type.
  --
  -- @since 0.10
  set :: (Component a) => f a -> f a
