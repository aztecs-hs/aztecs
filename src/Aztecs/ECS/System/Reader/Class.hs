{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Reader.Class (MonadReaderSystem (..)) where

import Aztecs.ECS.Query.Reader (QueryFilter (..))
import GHC.Stack
import Prelude hiding (all, any, filter, id, lookup, map, mapM, reads, (.))

class (Monad m) => MonadReaderSystem q m | m -> q where
  all :: i -> q i o -> m [o]

  single :: (HasCallStack) => i -> q i o -> m o
  single i q = do
    os <- all i q
    case os of
      [o] -> return o
      _ -> error "single: expected a single result"

  singleMaybe :: i -> q i o -> m (Maybe o)
  singleMaybe i q = do
    os <- all i q
    return $ case os of
      [o] -> Just o
      _ -> Nothing

  filter :: i -> q i o -> QueryFilter -> m [o]
