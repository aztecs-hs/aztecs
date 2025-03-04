{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.ECS.Query.Dynamic.Class
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Dynamic.Class (ArrowDynamicQuery (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Reader.Class
import Control.Arrow

-- | Arrow dynamic query.
--
-- @since 9.0
class (ArrowDynamicQueryReader arr) => ArrowDynamicQuery m arr | arr -> m where
  -- | Adjust a `Component` by its `ComponentID`.
  --
  -- @since 9.0
  adjustDyn :: (Component a) => (i -> a -> a) -> ComponentID -> arr i a

  -- | Adjust a `Component` by its `ComponentID`, ignoring any output.
  --
  -- @since 9.0
  adjustDyn_ :: (Component a) => (i -> a -> a) -> ComponentID -> arr i ()
  adjustDyn_ f cid = adjustDyn @m f cid >>> arr (const ())

  -- | Adjust a `Component` by its `ComponentID` with some `Monad` @m@.
  --
  -- @since 9.0
  adjustDynM :: (Component a) => (i -> a -> m a) -> ComponentID -> arr i a

  -- | Set a `Component` by its `ComponentID`.
  --
  -- @since 9.0
  setDyn :: (Component a) => ComponentID -> arr a a
