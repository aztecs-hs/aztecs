module Data.Aztecs.Core (Entity (..), EntityComponent (..)) where

newtype Entity = Entity Int deriving (Eq, Show)

data EntityComponent a = EntityComponent Entity a deriving (Show)
