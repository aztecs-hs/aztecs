module Aztecs.ECS.Query.Dynamic.Class (ArrowDynamicQuery (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader)

class (ArrowDynamicQueryReader arr) => ArrowDynamicQuery arr where
  adjustDyn :: (Component a) => (i -> a -> a) -> ComponentID -> arr i a

  setDyn :: (Component a) => ComponentID -> arr a a
