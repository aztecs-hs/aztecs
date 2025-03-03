{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Reader.Class (MonadReaderSystem (..)) where

import Aztecs.ECS.Query.Reader (QueryFilter (..))
import Prelude hiding (all, any, filter, id, lookup, map, mapM, reads, (.))

class (Monad m) => MonadReaderSystem q m | m -> q where
  all :: i -> q i o -> m [o]
  filter :: i -> q i o -> QueryFilter -> m [o]
