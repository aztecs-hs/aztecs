{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.ECS.System.Class (MonadSystem (..)) where

import Aztecs.ECS.Query.Reader (QueryFilter (..))
import GHC.Stack
import Prelude hiding (map)

class (Monad m) => MonadSystem q m | m -> q where
  map :: i -> q i o -> m [o]

  mapSingleMaybe :: i -> q i o -> m (Maybe o)

  mapSingle :: (HasCallStack) => i -> q i o -> m o
  mapSingle i q = do
    res <- mapSingleMaybe i q
    case res of
      Just a -> return a
      Nothing -> error "Expected a single matching entity."
  filterMap :: i -> q i o -> QueryFilter -> m [o]
