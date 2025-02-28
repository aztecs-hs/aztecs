{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..)) where

import Aztecs.ECS.Query.Reader (QueryFilter (..))
import Control.Arrow (Arrow (..), (>>>))
import GHC.Stack (HasCallStack)
import Prelude hiding (all, any, filter, id, lookup, map, mapM, reads, (.))

class (Arrow arr) => ArrowReaderSystem q arr | arr -> q where
  -- | Query all matching entities.
  all :: q i a -> arr i [a]

  -- | Query all matching entities with a `QueryFilter`.
  filter :: q i a -> QueryFilter -> arr i [a]

  -- | Query a single matching entity.
  -- If there are zero or multiple matching entities, an error will be thrown.
  single :: (HasCallStack) => q i a -> arr i a
  single q =
    all q
      >>> arr
        ( \case
            [a] -> a
            _ -> error "single: expected exactly one matching entity"
        )

  singleMaybe :: q i a -> arr i (Maybe a)
  singleMaybe q =
    all q
      >>> arr
        ( \case
            [a] -> Just a
            _ -> Nothing
        )
