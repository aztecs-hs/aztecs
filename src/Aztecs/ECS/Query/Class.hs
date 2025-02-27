module Aztecs.ECS.Query.Class (ArrowQuery (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader)

-- | Arrow for queries that can update entities.
class (ArrowQueryReader arr) => ArrowQuery arr where
  -- | Adjust a `Component` by its type.
  adjust :: (Component a) => (i -> a -> a) -> arr i a

  -- | Set a `Component` by its type.
  set :: (Component a) => arr a a
