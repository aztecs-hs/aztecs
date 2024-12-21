{-# LANGUAGE DeriveFunctor #-}

module Data.Aztecs.World
  ( Entity,
    EntityComponent (..),
    Component (..),
    Storage (..),
    World,
    spawn,
    insert,
    get,
    getRow,
    newWorld,
    setRow,
    table,
    ReadWrites (..),
    Write (..),
    Query (..),
    read,
    write,
    QueryResult (..),
    query,
    queryAll,
    adjust,
    adjustQuery,
  )
where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map (Map, alter, empty, lookup)
import qualified Data.Map as Map
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

setRow :: (Component c, Typeable c) => Storage c -> World -> World
setRow = f Proxy
  where
    f :: (Component c, Typeable c) => Proxy c -> Storage c -> World -> World
    f p cs (World w e') =
      World
        ( Map.insert (typeOf p) (toDyn cs) w
        )
        e'

data ReadWrites = ReadWrites [TypeRep] [TypeRep]

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites [] []

data Query a
  = Query
      ReadWrites
      (Maybe [Entity] -> World -> ([Entity], [a]))
      (Entity -> World -> Maybe a)
  deriving (Functor)

instance Applicative Query where
  pure a = Query mempty (\_ _ -> ([], [a])) (\_ _ -> Just a)
  Query rs f g <*> Query rs' f' g' =
    Query
      (rs <> rs')
      ( \es w ->
          let (es1, fs) = f es w
           in case es1 of
                [] -> ([], [])
                _ ->
                  let (es2, as) = f' es w
                   in (es1 <> es2, fs <*> as)
      )
      ( \e w ->
          case g e w of
            Just a -> case g' e w of
              Just a' -> Just $ a a'
              Nothing -> Nothing
            Nothing -> Nothing
      )

read :: (Typeable a) => Query a
read = f Proxy
  where
    f :: (Typeable a) => Proxy a -> Query a
    f p = Query (ReadWrites [typeOf p] []) (\es w -> readWrite es p w) (get)

newtype Write a = Write {unWrite :: a} deriving (Show)

adjust :: (Component c, Typeable c) => Write c -> (c -> c) -> Entity -> World -> World
adjust (Write a) f w = insert w (f a)

write :: (Component a, Typeable a) => Query (Write a)
write = f Proxy
  where
    f :: (Typeable a) => Proxy a -> Query (Write a)
    f p =
      Query
        (ReadWrites [] [typeOf p])
        (\es w -> let (a, b) = readWrite es p w in (a, Write <$> b))
        (\e w -> Write <$> get e w)

readWrite :: (Typeable a, Foldable t) => Maybe (t Entity) -> Proxy a -> World -> ([Entity], [a])
readWrite es p w =
  let row = (fromMaybe [] (fmap toList (getRow p w)))
      row' = case es of
        Just es' -> (filter (\(EntityComponent e _) -> isJust $ find (== e) es') row)
        Nothing -> row
   in foldr (\(EntityComponent e a) (es'', as) -> (e : es'', a : as)) ([], []) row'

query :: Entity -> Query a -> World -> Maybe a
query e (Query _ _ f) w = f e w

data QueryResult a = QueryResult [Entity] [a]
  deriving (Functor, Show)

queryAll :: Query a -> World -> QueryResult a
queryAll (Query _ f _) w = let (es, as) = f Nothing w in QueryResult es as

adjustQuery :: (Component a, Typeable a) => QueryResult (Write a) -> (a -> a) -> World -> World
adjustQuery (QueryResult es as) g w =
  let as' = map (\(Write wr) -> g wr) as
      s = getRow Proxy w
      s' = fmap (\s'' -> insert' s'' (map (\(e, a) -> EntityComponent e a) (zip es as'))) s
   in fromMaybe w (fmap (\y' -> setRow y' w) s')
