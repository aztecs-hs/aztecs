module Data.Aztecs.Query.Dynamic.Class (ArrowDynamicQuery (..)) where

import Data.Aztecs.Component
import Data.Aztecs.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader)

class (ArrowDynamicQueryReader arr) => ArrowDynamicQuery arr where
  setDyn :: (Component a) => ComponentID -> arr a a
