{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( ReadWrites (..),
    Query (..),
    entity,
    read,
    buildQuery,
    all,
    all',
    get,
    get',
    write,
  )
where

import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World (Component, Entity, EntityComponent (..), World (..), getRow)
import Data.Aztecs.World.Archetypes (Archetype, ArchetypeId, archetype)
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
data Query a where
  PureQB :: a -> Query a
  MapQB :: (a -> b) -> Query a -> Query b
  AppQB :: Query (a -> b) -> Query a -> Query b
  EntityQB :: Query Entity
  ReadQB :: (Component c) => Proxy c -> Archetype -> Query c
  WriteQB :: (Component c) => Proxy c -> c -> Archetype -> Query ()

instance Functor Query where
  fmap = MapQB

instance Applicative Query where
  pure = PureQB
  (<*>) = AppQB

entity :: Query Entity
entity = EntityQB

-- | Read a `Component`.
read :: forall c. (Component c) => Query c
read = ReadQB (Proxy :: Proxy c) (archetype @c)

-- | Alter a `Component`.
write :: forall c. (Component c) => c -> Query ()
write c = WriteQB (Proxy :: Proxy c) c (archetype @c)

buildQuery :: Query a -> Archetype
buildQuery (PureQB _) = mempty
buildQuery (MapQB _ qb) = buildQuery qb
buildQuery (AppQB f a) = buildQuery f <> buildQuery a
buildQuery EntityQB = mempty
buildQuery (ReadQB _ a) = a
buildQuery (WriteQB _ _ a) = a

all :: ArchetypeId -> Query a -> World -> IO [a]
all a qb w@(World _ as) = all' (A.getArchetype a as) qb w

all' :: [Entity] -> Query a -> World -> IO [a]
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

get :: ArchetypeId -> Query a -> Entity -> World -> IO (Maybe a)
get a qb e w@(World _ as) = get' e (A.getArchetype a as) qb w

get' :: Entity -> [Entity] -> Query a -> World -> IO (Maybe a)
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
