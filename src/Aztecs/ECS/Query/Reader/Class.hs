module Aztecs.ECS.Query.Reader.Class (ArrowQueryReader (..)) where

import Aztecs.ECS.Component
import Control.Arrow (Arrow (..), (>>>))

-- | Arrow for queries that can read from entities.
class (Arrow arr) => ArrowQueryReader arr where
  -- | Fetch a `Component` by its type.
  fetch :: (Component a) => arr () a

  -- | Fetch a `Component` by its type, returning `Nothing` if it doesn't exist.
  fetchMaybe :: (Component a) => arr () (Maybe a)
  fetchMaybe = fetch >>> arr Just
