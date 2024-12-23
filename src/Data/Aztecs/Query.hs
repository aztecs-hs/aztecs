{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( ReadWrites (..),
    Write,
    unWrite,
    mapWrite,
    QueryBuilder (..),
    read,
    write,
    Query (..),
    query,
    all,
    alter,
  )
where

import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World (Component, Entity, EntityComponent (..), World (..), get, getRow, setRow)
import Data.Aztecs.World.Archetypes (Archetype, ArchetypeId)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Prelude hiding (all, read)

-- | Component IDs to read and write.
data ReadWrites = ReadWrites (Set TypeRep) (Set TypeRep)

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites mempty mempty

data QueryBuilder a = QueryBuilder ReadWrites Archetype (ArchetypeId -> World -> Query a)

instance Functor QueryBuilder where
  fmap f (QueryBuilder r a q) = QueryBuilder r a (\aid w -> fmap f (q aid w))

instance Applicative QueryBuilder where
  pure a = QueryBuilder mempty mempty (const $ pure (Query [] (const $ const [a]) (const $ const $ Just a)))
  QueryBuilder r a f <*> QueryBuilder r' a' x =
    QueryBuilder (r <> r') (a <> a') (\aid w -> f aid w <*> x aid w)

-- | Query to apply to the `World`.
data Query a
  = Query
      [ArchetypeId]
      ([Entity] -> World -> [a])
      (Entity -> World -> Maybe a)
  deriving (Functor)

instance Applicative Query where
  pure a = Query mempty (const $ const [a]) (const $ const $ Just a)
  Query aIds f g <*> Query aIds' f' g' =
    Query (aIds <> aIds') (\es w -> f es w <*> f' es w) (\e w -> g e w <*> g' e w)

-- | Read a `Component`.
read :: forall c. (Component c) => QueryBuilder c
read =
  QueryBuilder
    (ReadWrites Set.empty (Set.fromList [typeOf (Proxy :: Proxy c)]))
    (A.archetype @c)
    ( \aId _ ->
        Query
          [aId]
          ( \es w ->
              let es' = fromMaybe [] (fmap S.toList (getRow (Proxy :: Proxy c) w))
                  es'' = filter (\(EntityComponent e _) -> e `elem` es) es'
               in map (\(EntityComponent _ c) -> c) es''
          )
          get
    )

newtype Write c = Write c deriving (Show)

unWrite :: Write c -> c
unWrite (Write c) = c

mapWrite :: (c -> c) -> Write c -> Write c
mapWrite f (Write c) = Write (f c)

-- | Get a writer to a `Component`.
write :: forall c. (Component c) => QueryBuilder (Write c)
write =
  QueryBuilder
    (ReadWrites Set.empty (Set.fromList [typeOf (Proxy :: Proxy c)]))
    (A.archetype @c)
    ( \aId _ ->
        Query
          [aId]
          ( \es w ->
              let es' = fromMaybe [] (fmap S.toList (getRow (Proxy :: Proxy c) w))
                  es'' = filter (\(EntityComponent e _) -> e `elem` es) es'
               in map (\(EntityComponent _ c) -> Write c) es''
          )
          (\e w -> Write <$> get e w)
    )

-- | Query a single match from the `World`.
query :: Entity -> Query a -> World -> Maybe a
query e (Query _ _ f) w = f e w

-- | Query all matches from the `World`.
all :: Query a -> World -> [a]
all (Query aIds f _) (World cs as) = f (concat $ map (\aId -> A.getArchetype aId as) aIds) (World cs as)

-- | Alter the components in a query.
alter :: (Component c) => [(Entity, Write c)] -> (c -> c) -> World -> World
alter as g w =
  let s = getRow Proxy w
      s' = fmap (\s'' -> S.insert s'' (map (\(e, Write a) -> EntityComponent e (g a)) as)) s
   in fromMaybe w (fmap (\y' -> setRow y' w) s')
