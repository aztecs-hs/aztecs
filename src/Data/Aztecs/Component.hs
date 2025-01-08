module Data.Aztecs.Component (Component (..)) where

import Data.Aztecs.Storage (ComponentStorage, table)
import Data.Data (Typeable)

class (Typeable a) => Component a where
  storage :: ComponentStorage a
  storage = table
