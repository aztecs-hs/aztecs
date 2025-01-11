module Data.Aztecs (Entity (..)) where

-- | Entity ID.
newtype Entity = Entity {unEntity :: Int}
  deriving (Eq, Ord, Show)
