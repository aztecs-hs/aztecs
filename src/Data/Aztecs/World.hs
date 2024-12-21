{-# LANGUAGE DeriveFunctor #-}

module Data.Aztecs.World
  ( Entity,
    EntityComponent (..),
    Component (..),
    Storage (..),
    World,
    union,
    spawn,
    insert,
    adjust,
    get,
    getRow,
    newWorld,
    setRow,
    table,
  )
where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map (Map, alter, empty, lookup)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Typeable
import Prelude hiding (read)

newtype Entity = Entity Int deriving (Eq, Show)

data EntityComponent a = EntityComponent Entity a deriving (Show)

table' :: [EntityComponent a] -> Storage a
table' cs =
  Storage
    { empty' = table' [],
      spawn' = \e a -> table' (EntityComponent e a : cs),
      insert' = \cs' -> table' (filter (\(EntityComponent e _) -> not . isJust $ find (\(EntityComponent e' _) -> e == e') cs') cs <> cs'),
      get' = \e ->
        find (\(EntityComponent e' _) -> e == e') cs
          <&> \(EntityComponent _ a) -> a,
      toList = cs
    }

table :: Storage a
table = table' []

data Storage a = Storage
  { empty' :: Storage a,
    spawn' :: Entity -> a -> Storage a,
    insert' :: [EntityComponent a] -> Storage a,
    get' :: Entity -> Maybe a,
    toList :: [EntityComponent a]
  }

class Component a where
  storage :: Storage a
  storage = table

data World = World (Map TypeRep Dynamic) Entity deriving (Show)

newWorld :: World
newWorld = World empty (Entity 0)

union :: World -> World -> World
union (World a e) (World b _) = World (Map.union a b) e

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

insert :: (Component c, Typeable c) => Entity -> c -> World -> World
insert = f Proxy
  where
    f :: (Component c, Typeable c) => Proxy c -> Entity -> c -> World -> World
    f p e c (World w e') =
      World
        ( alter
            ( \maybeRow -> Just $ toDyn $ g (maybeRow >>= fromDynamic) e c
            )
            (typeOf p)
            w
        )
        e'
    g :: (Component c, Typeable c) => Maybe (Storage c) -> Entity -> c -> Storage c
    g maybeRow e c = case maybeRow of
      Just row -> spawn' row e c
      Nothing -> spawn' storage e c

adjust :: (Component c, Typeable c) => c -> (c -> c) -> Entity -> World -> World
adjust a f w = insert w (f a)

getRow :: (Typeable c) => Proxy c -> World -> Maybe (Storage c)
getRow p (World w _) = Data.Map.lookup (typeOf p) w >>= fromDynamic

get :: (Typeable c) => Entity -> World -> Maybe c
get e (World w _) = f (Proxy)
  where
    f :: (Typeable c) => Proxy c -> Maybe c
    f p = Data.Map.lookup (typeOf p) w >>= fromDynamic >>= flip get' e

setRow :: (Component c, Typeable c) => Storage c -> World -> World
setRow = f Proxy
  where
    f :: (Component c, Typeable c) => Proxy c -> Storage c -> World -> World
    f p cs (World w e') =
      World
        ( Map.insert (typeOf p) (toDyn cs) w
        )
        e'
