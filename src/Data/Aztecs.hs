module Data.Aztecs (EntityID(..)) where

-- | EntityIDID.
newtype EntityID= EntityID{unEntityID:: Int}
  deriving (Eq, Ord, Show)
