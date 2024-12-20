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

newtype Entity = Entity Int deriving (Show)

data EntityComponent a = EntityComponent Entity a

table' :: [EntityComponent a] -> Storage a
table' cs =
  Storage
    { empty' = table' [],
      spawn' = \e a -> table' (EntityComponent e a : cs)
    }

table :: Storage a
table = table' []

data Storage a = Storage
  { empty' :: Storage a,
    spawn' :: Entity -> a -> Storage a
  }

class Component a where
  storage :: Storage a
  storage = table

data World = World (Map TypeRep Dynamic) Entity deriving (Show)

newWorld :: World
newWorld = World empty (Entity 0)

spawn :: (Component a, Typeable a) => a -> World -> World
spawn a (World w (Entity e)) =
  World
    ( alter
        ( \maybeRow -> case maybeRow of
            Just row -> fmap (\row' -> toDyn $ spawn' row' (Entity e) a) (fromDynamic row)
            Nothing -> Just $ toDyn $ spawn' storage (Entity e) a
        )
        (typeOf a)
        w
    )
    (Entity (e + 1))
