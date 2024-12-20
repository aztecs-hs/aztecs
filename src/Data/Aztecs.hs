module Data.Aztecs
  ( Storage (..),
    table,
    Component (..),
    World,
    newWorld,
    spawn,
    get,
  )
where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map
import Data.Typeable

newtype Entity = Entity Int deriving (Eq, Show)

data EntityComponent a = EntityComponent Entity a

table' :: [EntityComponent a] -> Storage a
table' cs =
  Storage
    { empty' = table' [],
      spawn' = \e a -> table' (EntityComponent e a : cs),
      get' = \e ->
        find (\(EntityComponent e' _) -> e == e') cs
          <&> \(EntityComponent _ a) -> a
    }

table :: Storage a
table = table' []

data Storage a = Storage
  { empty' :: Storage a,
    spawn' :: Entity -> a -> Storage a,
    get' :: Entity -> Maybe a
  }

class Component a where
  storage :: Storage a
  storage = table

data World = World (Map TypeRep Dynamic) Entity deriving (Show)

newWorld :: World
newWorld = World empty (Entity 0)

spawn :: (Component c, Typeable c) => c -> World -> (Entity, World)
spawn = f (Proxy)
  where
    f :: (Component c, Typeable c) => Proxy c -> c -> World -> (Entity, World)
    f p c (World w (Entity e)) =
      ( Entity e,
        World
          ( alter
              ( \maybeRow -> case maybeRow of
                  Just row -> fmap (\row' -> toDyn $ spawn' row' (Entity e) c) (fromDynamic row)
                  Nothing -> Just $ toDyn $ spawn' storage (Entity e) c
              )
              (typeOf p)
              w
          )
          (Entity (e + 1))
      )

get :: (Typeable c) => Entity -> World -> Maybe c
get e (World w _) = f (Proxy)
  where
    f :: (Typeable c) => Proxy c -> Maybe c
    f p = Data.Map.lookup (typeOf p) w >>= fromDynamic >>= flip get' e
