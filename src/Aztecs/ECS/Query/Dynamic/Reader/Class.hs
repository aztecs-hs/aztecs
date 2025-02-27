module Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Control.Arrow

class (Arrow arr) => ArrowDynamicQueryReader arr where
  -- | Fetch the currently matched `EntityID`.
  entity :: arr () EntityID

  -- | Fetch a `Component` by its `ComponentID`.
  fetchDyn :: (Component a) => ComponentID -> arr () a

  -- | Try to fetch a `Component` by its `ComponentID`.
  fetchMaybeDyn :: (Component a) => ComponentID -> arr () (Maybe a)
  fetchMaybeDyn cId = fetchDyn cId >>> arr Just
