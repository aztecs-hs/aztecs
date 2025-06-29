{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Class where

import Aztecs.ECS.Query

class (Monad m) => MonadSystem c m | m -> c where
  query :: Query m c
