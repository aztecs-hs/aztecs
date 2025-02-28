module Aztecs.ECS.Query.Dynamic.Class (ArrowDynamicQuery (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader)
import Control.Arrow

class (ArrowDynamicQueryReader arr) => ArrowDynamicQuery arr where
  adjustDyn :: (Component a) => (i -> a -> a) -> ComponentID -> arr i a

  adjustDyn_ :: (Component a) => (i -> a -> a) -> ComponentID -> arr i ()
  adjustDyn_ f cid = adjustDyn f cid >>> arr (const ())

  setDyn :: (Component a) => ComponentID -> arr a a
