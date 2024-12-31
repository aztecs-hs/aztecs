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
    write,
  )
where

import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World (Component, Entity, EntityComponent (..), World (..), getRow)
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
  ReadQB :: (Component c) => Proxy c -> (World -> IO (ArchetypeId, World)) -> QueryBuilder c
  WriteQB :: (Component c) => Proxy c -> c -> (World -> IO (ArchetypeId, World)) -> QueryBuilder ()

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
  ReadQB
    (Proxy :: Proxy c)
    ( \(World cs as) -> do
        (aId, as') <- A.insertArchetype (A.archetype @c) cs as
        return (aId, World cs as')
    )

-- | Alter a `Component`.
write :: forall c. (Component c) => c -> QueryBuilder ()
write c =
  WriteQB
    (Proxy :: Proxy c)
    c
    ( \(World cs as) -> do
        (aId, as') <- A.insertArchetype (A.archetype @c) cs as
        return (aId, World cs as')
    )

buildQuery :: QueryBuilder a -> World -> IO (Query a, World)
buildQuery (PureQB a) w = return (pure a, w)
buildQuery (MapQB f qb) w = do
  (a, w') <- buildQuery qb w
  return (fmap f a, w')
buildQuery (AppQB fqb aqb) w = do
  (f, w') <- buildQuery fqb w
  (a, w'') <- buildQuery aqb w'
  return (f <*> a, w'')
buildQuery EntityQB w = return (Query mempty EntityQB, w)
buildQuery (ReadQB p f) w = do
  (aId, w') <- f w
  return (Query [aId] (ReadQB p f), w')
buildQuery (WriteQB p c f) w = do
  (aId, w') <- f w
  return (Query [aId] (WriteQB p c f), w')

-- | Query to apply to the `World`.
data Query a = Query [ArchetypeId] (QueryBuilder a)
  deriving (Functor)

instance Applicative Query where
  pure a = Query mempty (pure a)
  Query aIds f <*> Query aIds' f' = Query (aIds <> aIds') (f <*> f')

all :: Query a -> World -> IO [a]
all (Query aIds qb) w@(World _ as) = all' (concat $ map (\aId -> A.getArchetype aId as) aIds) qb w

all' :: [Entity] -> QueryBuilder a -> World -> IO [a]
all' _ (PureQB a) _ = return [a]
all' es (MapQB f qb) w = do
  a <- all' es qb w
  return $ map f a
all' es (AppQB fqb aqb) w = do
  f <- all' es fqb w
  a <- all' es aqb w
  return $ zipWith (\f' a' -> f' a') f a
all' es EntityQB _ = return es
all' es (ReadQB p _) w = do
  es' <- fromMaybe (pure []) (fmap S.toList (getRow p w))
  let es'' = filter (\(EntityComponent e _) -> e `elem` es) es'
  return $ map (\(EntityComponent _ c) -> c) es''
all' es (WriteQB p c _) w = do
  es' <- fromMaybe (pure []) (fmap S.toList' (getRow p w))
  let es'' = filter (\(EntityComponent e _, _) -> e `elem` es) es'
  mapM_ (\(EntityComponent _ _, f) -> f c) es''
  return $ replicate (length es) ()

get :: Query a -> Entity -> World -> IO (Maybe a)
get (Query aIds qb) e w@(World _ as) = get' e (concat $ map (\aId -> A.getArchetype aId as) aIds) qb w

get' :: Entity -> [Entity] -> QueryBuilder a -> World -> IO (Maybe a)
get' _ _ (PureQB a) _ = return $ Just a
get' e es (MapQB f qb) w = do
  a <- get' e es qb w
  return $ fmap f a
get' e es (AppQB fqb aqb) w = do
  f <- get' e es fqb w
  a <- get' e es aqb w
  return $ f <*> a
get' e es EntityQB _ = return $ if e `elem` es then Just e else Nothing
get' e _ (ReadQB p _) w = do
  es' <- fromMaybe (pure []) (fmap S.toList (getRow p w))
  let es'' = filter (\(EntityComponent e' _) -> e == e') es'
  return $ fmap (\(EntityComponent _ c) -> c) (listToMaybe es'')
