{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Dynamic.Class
  ( ArrowDynamicSystem (..),
  )
where

import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.World.Archetypes (Node (..))
import Data.Set (Set)

class ArrowDynamicSystem q arr | arr -> q where
  -- | Map all matching entities, storing the updated entities.
  mapDyn :: Set ComponentID -> q i o -> arr i [o]

  mapSingleDyn :: Set ComponentID -> q i o -> arr i o

  mapSingleMaybeDyn :: Set ComponentID -> q i o -> arr i (Maybe o)

  filterMapDyn ::
    Set ComponentID ->
    q i o ->
    (Node -> Bool) ->
    arr i [o]
