{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..)) where

import Aztecs.ECS.Query.Reader (QueryFilter (..))
import Control.Arrow (Arrow (..), (>>>))
import Prelude hiding (all, any, filter, id, lookup, map, mapM, reads, (.))

class (Arrow arr) => ArrowReaderSystem q arr | arr -> q where
  -- | Query all matching entities.
  all :: q i a -> arr i [a]

  -- | Query all matching entities with a `QueryFilter`.
  filter :: q () a -> QueryFilter -> arr () [a]

  -- | Query a single matching entity.
  -- If there are zero or multiple matching entities, an error will be thrown.
  single :: q i a -> arr i a
  single q =
    all q
      >>> arr
        ( \as -> case as of
            [a] -> a
            _ -> error "TODO"
        )
