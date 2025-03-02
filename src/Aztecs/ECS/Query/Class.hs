{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.ECS.Query.Class (ArrowQuery (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader)
import Control.Arrow

-- | Arrow for queries that can update entities.
class (ArrowQueryReader arr) => ArrowQuery m arr | arr -> m where
  -- | Adjust a `Component` by its type.
  adjust :: (Component a) => (i -> a -> a) -> arr i a

  adjust_ :: (Component a) => (i -> a -> a) -> arr i ()
  adjust_ f = adjust @m f >>> arr (const ())

  adjustM :: (Component a) => (i -> a -> m a) -> arr i a

  -- | Set a `Component` by its type.
  set :: (Component a) => arr a a
