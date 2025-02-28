{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.World.Archetypes (Node)
import Control.Arrow
import Data.Set (Set)
import GHC.Stack

class (Arrow arr) => ArrowDynamicReaderSystem q arr | arr -> q where
  allDyn :: Set ComponentID -> q i o -> arr i [o]

  filterDyn :: Set ComponentID -> q i o -> (Node -> Bool) -> arr i [o]

  singleDyn :: (HasCallStack) => Set ComponentID -> q i a -> arr i a
  singleDyn cIds q =
    allDyn cIds q
      >>> arr
        ( \case
            [a] -> a
            _ -> error "singleDyn: expected exactly one matching entity"
        )

  singleMaybeDyn :: Set ComponentID -> q i o -> arr i (Maybe o)
  singleMaybeDyn cIds q =
    allDyn cIds q
      >>> arr
        ( \case
            [a] -> Just a
            _ -> Nothing
        )
