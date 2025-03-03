{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Dynamic.Reader.Class (MonadDynamicReaderSystem (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.World.Archetypes (Node)
import Data.Set (Set)

class (Monad m) => MonadDynamicReaderSystem q m | m -> q where
  allDyn :: i -> Set ComponentID -> q i o -> m [o]
  filterDyn :: i -> Set ComponentID -> q i a -> (Node -> Bool) -> m [a]
