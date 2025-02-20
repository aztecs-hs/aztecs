module Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..)) where

import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader)
import Aztecs.ECS.World.Archetypes (Node)
import Control.Arrow (Arrow (..), (>>>))
import Data.Set (Set)

class (Arrow arr) => ArrowDynamicReaderSystem arr where
  allDyn :: Set ComponentID -> DynamicQueryReader i o -> arr i [o]

  filterDyn :: Set ComponentID -> DynamicQueryReader i o -> (Node -> Bool) -> arr i [o]

  singleDyn :: Set ComponentID -> DynamicQueryReader () a -> arr () a
  singleDyn cIds q =
    allDyn cIds q
      >>> arr
        ( \as -> case as of
            [a] -> a
            _ -> error "TODO"
        )
