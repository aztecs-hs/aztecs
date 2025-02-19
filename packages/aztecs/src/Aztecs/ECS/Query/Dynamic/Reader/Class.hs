module Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..)) where

import Control.Arrow (Arrow (..), (>>>))
import Aztecs.ECS.Component
import Aztecs.ECS.Entity (EntityID)

class (Arrow arr) => ArrowDynamicQueryReader arr where
  -- | Fetch the `EntityID` belonging to this entity.
  entityDyn :: arr () EntityID

  -- | Fetch a `Component` by its `ComponentID`.
  fetchDyn :: (Component a) => ComponentID -> arr () a

  -- | Try to fetch a `Component` by its `ComponentID`.
  fetchMaybeDyn :: (Component a) => ComponentID -> arr () (Maybe a)
  fetchMaybeDyn cId = fetchDyn cId >>> arr Just
