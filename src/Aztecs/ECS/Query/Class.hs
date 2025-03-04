{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.ECS.Query.Reader.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Class (ArrowQuery (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader)
import Control.Arrow

-- | Arrow for queries that can update entities.
--
-- @since 0.9
class (ArrowQueryReader arr) => ArrowQuery m arr | arr -> m where
  -- | Adjust a `Component` by its type.
  --
  -- @since 0.9
  adjust :: (Component a) => (i -> a -> a) -> arr i a

  -- | Adjust a `Component` by its type, ignoring any output.
  --
  -- @since 0.9
  adjust_ :: (Component a) => (i -> a -> a) -> arr i ()
  adjust_ f = adjust @m f >>> arr (const ())

  -- | Adjust a `Component` by its type with some `Monad` @m@.
  --
  -- @since 0.9
  adjustM :: (Component a) => (i -> a -> m a) -> arr i a

  -- | Set a `Component` by its type.
  --
  -- @since 0.9
  set :: (Component a) => arr a a
