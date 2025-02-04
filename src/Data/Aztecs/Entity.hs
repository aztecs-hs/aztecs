module Data.Aztecs.Entity (EntityID (..)) where

-- | Entity ID.
newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show)
