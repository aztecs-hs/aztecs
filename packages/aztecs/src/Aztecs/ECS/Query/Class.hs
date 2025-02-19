module Aztecs.ECS.Query.Class (ArrowQuery (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Reader.Class (ArrowQueryReader)

class (ArrowQueryReader arr) => ArrowQuery arr where
  -- | Set a `Component` by its type.
  set :: (Component a) => arr a a
