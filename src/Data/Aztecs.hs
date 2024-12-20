module Data.Aztecs
  ( Storage (..),
    table,
    Component (..),
    World,
    newWorld,
    spawn,
  )
where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map
import Data.Typeable

table' :: [a] -> Storage a
table' cs =
  Storage
    { empty' = table' [],
      spawn' = \a -> table' (a : cs)
    }

table :: Storage a
table = table' []

data Storage a = Storage
  { empty' :: Storage a,
    spawn' :: a -> Storage a
  }

class Component a where
  storage :: Storage a
  storage = table

data World = World (Map TypeRep Dynamic) deriving (Show)

newWorld :: World
newWorld = World empty

spawn :: (Component a, Typeable a) => a -> World -> World
spawn a (World w) =
  World
    ( alter
        ( \maybeRow -> case maybeRow of
            Just row -> fmap (\row' -> toDyn $ spawn' row' a) (fromDynamic row)
            Nothing -> Just $ toDyn $ spawn' storage a
        )
        (typeOf a)
        w
    )
