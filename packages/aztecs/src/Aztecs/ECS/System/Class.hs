{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Class (ArrowSystem (..)) where

import Aztecs.ECS.Access (Access)
import Aztecs.ECS.Query.Reader (QueryFilter (..))
import Control.Arrow (Arrow (..), (>>>))
import Prelude hiding (map)

class (Arrow arr) => ArrowSystem q arr | arr -> q where
  -- | Query and update all matching entities.
  map :: q i a -> arr i [a]

  -- | Query and update all matching entities, ignoring the results.
  map_ :: q i o -> arr i ()
  map_ q = map q >>> arr (const ())

  -- | Map all matching entities with a `QueryFilter`, storing the updated entities.
  filterMap :: q i a -> QueryFilter -> arr i [a]

  -- | Map a single matching entity, storing the updated components.
  -- If there are zero or multiple matching entities, an error will be thrown.
  mapSingle :: q i a -> arr i a

  mapSingleMaybe :: q i a -> arr i (Maybe a)

  -- | Queue an `Access` to happen after this system schedule.
  queue :: (i -> Access ()) -> arr i ()
