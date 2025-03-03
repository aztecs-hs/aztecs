{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Dynamic.Class (MonadDynamicSystem (..)) where

import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.World.Archetypes (Node (..))
import Data.Set (Set)
import GHC.Stack

class (Monad m) => MonadDynamicSystem q m | m -> q where
  mapDyn :: i -> Set ComponentID -> q i o -> m [o]

  mapSingleMaybeDyn :: i -> Set ComponentID -> q i a -> m (Maybe a)

  mapSingleDyn :: (HasCallStack) => i -> Set ComponentID -> q i o -> m o
  mapSingleDyn i cIds q = do
    res <- mapSingleMaybeDyn i cIds q
    case res of
      Just a -> return a
      Nothing -> error "Expected a single matching entity."

  filterMapDyn :: i -> Set ComponentID -> (Node -> Bool) -> q i a -> m [a]
