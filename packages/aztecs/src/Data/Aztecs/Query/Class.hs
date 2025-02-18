module Data.Aztecs.Query.Class (ArrowQuery (..)) where

import Data.Aztecs.Component
import Data.Aztecs.Query.Reader.Class (ArrowQueryReader)

class (ArrowQueryReader arr) => ArrowQuery arr where
  -- | Set a `Component` by its type.
  set :: (Component a) => arr a a
