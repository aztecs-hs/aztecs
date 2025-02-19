module Aztecs.ECS.Entity (EntityID (..)) where

-- | Entity ID.
newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show)
