module Data.Aztecs.Core
  ( Entity (..),
  )
where

newtype Entity = Entity {unEntity :: Int}
  deriving (Eq, Ord, Show)
