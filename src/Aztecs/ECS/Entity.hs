{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aztecs.ECS.Entity (EntityID (..)) where

import Control.DeepSeq
import GHC.Generics

-- | Entity ID.
newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)
