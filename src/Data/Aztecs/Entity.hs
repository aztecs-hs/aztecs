module Data.Aztecs.Entity where

newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show)
