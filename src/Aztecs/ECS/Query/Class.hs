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
class (Applicative g, Functor f) => QueryF g f | f -> g where
  -- | Fetch a `Component` by its type.
  fetch :: (Component a) => f a

  -- | Fetch a `Component` by its type, returning `Nothing` if it doesn't exist.
  fetchMaybe :: (Component a) => f (Maybe a)
  fetchMaybe = Just <$> fetch

  -- | Adjust a `Component` by its type.
  fetchMap :: (Component a) => (b -> a -> a) -> f b -> f a

  -- | Adjust a `Component` by its type, ignoring any output.
  fetchMap_ :: (Component a) => (b -> a -> a) -> f b -> f ()
  fetchMap_ f = void . fetchMap f

  -- | Adjust a `Component` by its type with some `Monad` @m@.
  fetchMapM :: (Component a) => (b -> a -> g a) -> f b -> f a
