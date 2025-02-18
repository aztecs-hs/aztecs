module Data.Aztecs.Query.Dynamic.Class (ArrowDynamicQuery (..)) where

import Data.Aztecs.Component
import Data.Aztecs.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

class (ArrowDynamicQueryReader arr) => ArrowDynamicQuery arr where
  setDyn :: (Component a) => ComponentID -> arr a a
