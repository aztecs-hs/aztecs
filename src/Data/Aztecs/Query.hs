{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( ReadWrites (..),
    Write (..),
    Query (..),
    read,
    write,
    has,
    QueryResult (..),
    query,
    all,
    adjust,
  )
where

import Data.Aztecs.World (Component, Entity, EntityComponent (..), Storage (..), World, get, getRow, setRow)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable
import Prelude hiding (all, read)

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

-- | Read a `Component`.
read :: (Typeable a) => Query a
read = f Proxy
  where
    f :: (Typeable a) => Proxy a -> Query a
    f p = Query (ReadWrites [typeOf p] []) (\es w -> readWrite es p w) get

newtype Write a = Write {unWrite :: a} deriving (Show)

-- | Get a writer to a `Component`.
write :: (Component a, Typeable a) => Query (Write a)
write = f Proxy
  where
    f :: (Typeable a) => Proxy a -> Query (Write a)
    f p =
      Query
        (ReadWrites [] [typeOf p])
        (\es w -> let (a, b) = readWrite es p w in (a, Write <$> b))
        (\e w -> Write <$> get e w)

has :: forall a. (Typeable a) => Query Bool
has = f @a Proxy
  where
    f :: forall b. (Typeable b) => Proxy b -> Query Bool
    f p =
      Query
        (ReadWrites [] [typeOf p])
        ( \_es w ->
            let row = (fromMaybe [] (fmap toList (getRow p w)))
             in foldr (\(EntityComponent e _) (eAcc, rowAcc) -> (e : eAcc, True : rowAcc)) ([], []) row
        )
        (\e w -> Just $ isJust $ get @a e w)

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

all :: Query a -> World -> QueryResult a
all (Query _ f _) w = let (es, as) = f Nothing w in QueryResult es as

adjust :: (Component a, Typeable a) => QueryResult (Write a) -> (a -> a) -> World -> World
adjust (QueryResult es as) g w =
  let as' = map (\(Write wr) -> g wr) as
      s = getRow Proxy w
      s' = fmap (\s'' -> insert' s'' (map (\(e, a) -> EntityComponent e a) (zip es as'))) s
   in fromMaybe w (fmap (\y' -> setRow y' w) s')
