{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aztecs.ECS.World.Bundle.Class (MonoidBundle (..)) where

import Aztecs.ECS.Component (Component (..))
import Data.Data (Typeable)

class (Monoid a) => MonoidBundle a where
  bundle :: forall c. (Component c, Typeable (StorageT c)) => c -> a
