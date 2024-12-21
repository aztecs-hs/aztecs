{-# LANGUAGE DeriveFunctor #-}

module Data.Aztecs
  ( Entity,
    EntityComponent (..),
    Storage (..),
    table,
    Component (..),
    World,
    newWorld,
    spawn,
    insert,
    get,
    Query,
    read,
    runQuery,
  )
where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map (Map, alter, empty, lookup)
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable
import Prelude hiding (read)

newtype Entity = Entity Int deriving (Eq, Show)

data EntityComponent a = EntityComponent Entity a deriving (Show)

table' :: [EntityComponent a] -> Storage a
table' cs =
  Storage
    { empty' = table' [],
      spawn' = \e a -> table' (EntityComponent e a : cs),
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
    get' :: Entity -> Maybe a,
    toList :: [EntityComponent a]
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

getRow :: (Typeable c) => Proxy c -> World -> Maybe (Storage c)
getRow p (World w _) = Data.Map.lookup (typeOf p) w >>= fromDynamic

get :: (Typeable c) => Entity -> World -> Maybe c
get e (World w _) = f (Proxy)
  where
    f :: (Typeable c) => Proxy c -> Maybe c
    f p = Data.Map.lookup (typeOf p) w >>= fromDynamic >>= flip get' e

data ReadWrites = ReadWrites [TypeRep] [TypeRep]

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites [] []

data Query a = Query ReadWrites (Maybe [Entity] -> World -> [a])
  deriving (Functor)

instance Applicative Query where
  pure a = Query mempty (\_ _ -> [a])
  Query rs f <*> Query rs' f' = Query (rs <> rs') (\es w -> f es w <*> f' es w)

read :: (Typeable a) => Query a
read = f Proxy
  where
    f :: (Typeable a) => Proxy a -> Query a
    f p =
      Query
        (ReadWrites [typeOf p] [])
        ( \es w ->
            let x = (fromMaybe [] (fmap toList (getRow p w)))
             in case es of
                  Just es' ->
                    map
                      (\(EntityComponent _ a) -> a)
                      (filter (\(EntityComponent e _) -> isJust $ find (== e) es') x)
                  Nothing -> map (\(EntityComponent _ a) -> a) x
        )

runQuery :: Query a -> World -> [a]
runQuery (Query _ f) w = f Nothing w
