{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aztecs.ECS.Entity (EntityID (..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Entity ID.
newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show, Generic, NFData)
