{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Dynamic.Reader.Class (MonadDynamicReaderSystem (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.World.Archetypes (Node)
import Data.Set (Set)
import GHC.Stack

class (Monad m) => MonadDynamicReaderSystem q m | m -> q where
  allDyn :: i -> Set ComponentID -> q i o -> m [o]

  singleDyn :: (HasCallStack) => i -> Set ComponentID -> q i o -> m o
  singleDyn i cIds q = do
    os <- allDyn i cIds q
    case os of
      [o] -> return o
      _ -> error "singleDyn: expected a single result, but got multiple"

  singleMaybeDyn :: i -> Set ComponentID -> q i o -> m (Maybe o)
  singleMaybeDyn i cIds q = do
    os <- allDyn i cIds q
    return $ case os of
      [o] -> Just o
      _ -> Nothing

  filterDyn :: i -> Set ComponentID -> q i a -> (Node -> Bool) -> m [a]
