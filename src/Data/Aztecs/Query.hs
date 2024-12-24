{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( ReadWrites (..),
    QueryBuilder (..),
    entity,
    read,
    buildQuery,
    Query (..),
    all,
    all',
    get,
    get',
    alter,
  )
where

import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World (Component, Entity, EntityComponent (..), World (..), getRow, setRow)
import Data.Aztecs.World.Archetypes (ArchetypeId)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Typeable
import Prelude hiding (all, read)

-- | Component IDs to read and write.
data ReadWrites = ReadWrites (Set TypeRep) (Set TypeRep)

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites mempty mempty

-- | Builder for a `Query`.
data QueryBuilder a where
  PureQB :: a -> QueryBuilder a
  MapQB :: (a -> b) -> QueryBuilder a -> QueryBuilder b
  AppQB :: QueryBuilder (a -> b) -> QueryBuilder a -> QueryBuilder b
  EntityQB :: QueryBuilder Entity
  ComponentQB :: (Component c) => Proxy c -> (World -> (ArchetypeId, World)) -> QueryBuilder c

instance Functor QueryBuilder where
  fmap = MapQB

instance Applicative QueryBuilder where
  pure = PureQB
  (<*>) = AppQB

entity :: QueryBuilder Entity
entity = EntityQB

-- | Read a `Component`.
read :: forall c. (Component c) => QueryBuilder c
read =
  ComponentQB
    (Proxy :: Proxy c)
    ( \(World cs as) ->
        let (aId, as') = A.insertArchetype (A.archetype @c) cs as
         in (aId, World cs as')
    )

buildQuery :: QueryBuilder a -> World -> (Query a, World)
buildQuery (PureQB a) w = (pure a, w)
buildQuery (MapQB f qb) w =
  let (a, w') = buildQuery qb w
   in (fmap f a, w')
buildQuery (AppQB fqb aqb) w =
  let (f, w') = buildQuery fqb w
      (a, w'') = buildQuery aqb w'
   in (f <*> a, w'')
buildQuery EntityQB w = (Query mempty EntityQB, w)
buildQuery (ComponentQB p f) w =
  let (aId, w') = f w
   in (Query [aId] (ComponentQB p f), w')

-- | Query to apply to the `World`.
data Query a = Query [ArchetypeId] (QueryBuilder a)
  deriving (Functor)

instance Applicative Query where
  pure a = Query mempty (pure a)
  Query aIds f <*> Query aIds' f' = Query (aIds <> aIds') (f <*> f')

all :: Query a -> World -> [a]
all (Query aIds qb) w@(World _ as) = all' (concat $ map (\aId -> A.getArchetype aId as) aIds) qb w

all' :: [Entity] -> QueryBuilder a -> World -> [a]
all' _ (PureQB a) _ = [a]
all' es (MapQB f qb) w = map f (all' es qb w)
all' es (AppQB fqb aqb) w = zipWith (\f a -> f a) (all' es fqb w) (all' es aqb w)
all' es EntityQB _ = es
all' es (ComponentQB p _) w =
  let es' = fromMaybe [] (fmap S.toList (getRow p w))
      es'' = filter (\(EntityComponent e _) -> e `elem` es) es'
   in map (\(EntityComponent _ c) -> c) es''

get :: Query a -> Entity -> World -> Maybe a
get (Query aIds qb) e w@(World _ as) = get' e (concat $ map (\aId -> A.getArchetype aId as) aIds) qb w

get' :: Entity -> [Entity] -> QueryBuilder a -> World -> Maybe a
get' _ _ (PureQB a) _ = Just a
get' e es (MapQB f qb) w = fmap f (get' e es qb w)
get' e es (AppQB fqb aqb) w = get' e es fqb w <*> get' e es aqb w
get' e es EntityQB _ = if e `elem` es then Just e else Nothing
get' e _ (ComponentQB p _) w =
  let es' = fromMaybe [] (fmap S.toList (getRow p w))
      es'' = filter (\(EntityComponent e' _) -> e == e') es'
   in fmap (\(EntityComponent _ c) -> c) (listToMaybe es'')

-- | Alter the components in a query.
alter :: (Component c) => [EntityComponent c] -> (Entity -> c -> c) -> World -> World
alter as g w =
  let s = getRow Proxy w
      s' = fmap (\s'' -> S.insert s'' (map (\(EntityComponent e a) -> EntityComponent e (g e a)) as)) s
   in fromMaybe w (fmap (\y' -> setRow y' w) s')
