module Data.Aztecs.Component (Component (..)) where

import Data.Aztecs.Storage (Storage, table)
import Data.Data (Typeable)

class (Typeable a) => Component a where
  storage :: Storage a
  storage = table
