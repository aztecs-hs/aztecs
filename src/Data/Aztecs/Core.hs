module Data.Aztecs.Core (Entity (..), EntityComponent (..)) where

newtype Entity = Entity Int deriving (Eq, Ord, Show)

data EntityComponent a = EntityComponent Entity a deriving (Show)
