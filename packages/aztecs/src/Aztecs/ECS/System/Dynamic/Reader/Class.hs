{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..)) where

import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.World.Archetypes (Node)
import Control.Arrow (Arrow (..), (>>>))
import Data.Set (Set)

class (Arrow arr) => ArrowDynamicReaderSystem q arr | arr -> q where
  allDyn :: Set ComponentID -> q i o -> arr i [o]

  filterDyn :: Set ComponentID -> q i o -> (Node -> Bool) -> arr i [o]

  singleDyn :: Set ComponentID -> q () a -> arr () a
  singleDyn cIds q =
    allDyn cIds q
      >>> arr
        ( \as -> case as of
            [a] -> a
            _ -> error "TODO"
        )
