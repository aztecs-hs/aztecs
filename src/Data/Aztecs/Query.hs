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
    alter,
  )
where

import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World (Component, Entity, EntityComponent (..), World, get, getRow, setRow)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable
import Prelude hiding (all, read)

-- | Component IDs to read and write.
data ReadWrites = ReadWrites [TypeRep] [TypeRep]

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites [] []

-- | Query to apply to the `World`.
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
      ( \es w -> case f es w of
          ([], _) -> ([], [])
          (es', fs) -> let (es'', as) = f' es w in (es' <> es'', fs <*> as)
      )
      ( \e w -> do
          a <- g e w
          a' <- g' e w
          return (a a')
      )

-- | Read a `Component`.
read :: forall a. (Typeable a) => Query a
read = Query (ReadWrites [typeOf (Proxy :: Proxy a)] []) (\es w -> readWrite es (Proxy :: Proxy a) w) get

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

-- | Check if an `Entity` has a `Component`, returning `True` if it's present.
has :: forall a. (Typeable a) => Query Bool
has =
  Query
    (ReadWrites [] [typeOf (Proxy :: Proxy a)])
    ( \_es w ->
        let row = (fromMaybe [] (fmap S.toList (getRow (Proxy :: Proxy a) w)))
         in foldr (\(EntityComponent e _) (eAcc, rowAcc) -> (e : eAcc, True : rowAcc)) ([], []) row
    )
    (\e w -> Just $ isJust $ get @a e w)

readWrite :: (Typeable a, Foldable t) => Maybe (t Entity) -> Proxy a -> World -> ([Entity], [a])
readWrite es p w =
  let row = (fromMaybe [] (fmap S.toList (getRow p w)))
      row' = case es of
        Just es' -> (filter (\(EntityComponent e _) -> isJust $ find (== e) es') row)
        Nothing -> row
   in foldr (\(EntityComponent e a) (es'', as) -> (e : es'', a : as)) ([], []) row'

-- | Query a single match from the `World`.
query :: Entity -> Query a -> World -> Maybe a
query e (Query _ _ f) w = f e w

data QueryResult a = QueryResult [Entity] [a]
  deriving (Functor, Show)

-- | Query all matches from the `World`.
all :: Query a -> World -> QueryResult a
all (Query _ f _) w = let (es, as) = f Nothing w in QueryResult es as

-- | Alter the components in a query.
alter :: (Component a, Typeable a) => QueryResult (Write a) -> (a -> a) -> World -> World
alter (QueryResult es as) g w =
  let as' = map (\(Write wr) -> g wr) as
      s = getRow Proxy w
      s' = fmap (\s'' -> S.insert s'' (map (\(e, a) -> EntityComponent e a) (zip es as'))) s
   in fromMaybe w (fmap (\y' -> setRow y' w) s')
