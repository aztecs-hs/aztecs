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

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World (Component, Entity, EntityComponent (..), World (..), getRow)
import Data.Aztecs.World.Archetypes (Archetype, ArchetypeId, archetype)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Foldable (foldrM)
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
data Query m a where
  PureQ :: a -> Query m a
  MapQ :: (a -> b) -> Query m a -> Query m b
  AppQ :: Query m (a -> b) -> Query m a -> Query m b
  BindQ :: Query m a -> (a -> Query m b) -> Query m b
  EntityQ :: Query m Entity
  ReadQ :: (Component c) => Proxy c -> Archetype -> Query m c
  WriteQ :: (Component c) => Proxy c -> (c -> c) -> Archetype -> Query m c
  LiftQ :: m a -> Query m a

instance Functor (Query m) where
  fmap = MapQ

instance Applicative (Query m) where
  pure = PureQ
  (<*>) = AppQ

instance Monad (Query m) where
  (>>=) = BindQ

entity :: Query m Entity
entity = EntityQ

-- | Read a `Component`.
read :: forall m c. (Component c) => Query m c
read = ReadQ (Proxy :: Proxy c) (archetype @c)

-- | Alter a `Component`.
write :: forall m c. (Component c) => (c -> c) -> Query m c
write c = WriteQ (Proxy :: Proxy c) c (archetype @c)

buildQuery :: Query m a -> Archetype
buildQuery (PureQ _) = mempty
buildQuery (MapQ _ qb) = buildQuery qb
buildQuery (AppQ f a) = buildQuery f <> buildQuery a
buildQuery EntityQ = mempty
buildQuery (ReadQ _ a) = a
buildQuery (WriteQ _ _ a) = a
buildQuery (BindQ a _) = buildQuery a
buildQuery (LiftQ _) = mempty

all :: (MonadIO m) => ArchetypeId -> Query m a -> World -> m [a]
all a qb w@(World _ as) = all' (A.getArchetype a as) qb w

all' :: (MonadIO m) => [Entity] -> Query m a -> World -> m [a]
all' es q w =
  foldrM
    ( \e acc -> do
        a <- get' e es q w
        return $ case a of
          Just a' -> (a' : acc)
          Nothing -> acc
    )
    []
    es

get :: (MonadIO m) => ArchetypeId -> Query m a -> Entity -> World -> m (Maybe a)
get a qb e w@(World _ as) = get' e (A.getArchetype a as) qb w

get' :: (MonadIO m) => Entity -> [Entity] -> Query m a -> World -> m (Maybe a)
get' _ _ (PureQ a) _ = return $ Just a
get' e es (MapQ f qb) w = do
  a <- get' e es qb w
  return $ fmap f a
get' e es (AppQ fqb aqb) w = do
  f <- get' e es fqb w
  a <- get' e es aqb w
  return $ f <*> a
get' e es EntityQ _ = return $ if e `elem` es then Just e else Nothing
get' e _ (ReadQ p _) w = do
  es' <- liftIO $ fromMaybe (pure []) (fmap S.toList (getRow p w))
  let es'' = filter (\(EntityComponent e' _) -> e == e') es'
  return $ fmap (\(EntityComponent _ c) -> c) (listToMaybe es'')
get' e _ (WriteQ p f _) w = do
  es' <- liftIO $ fromMaybe (pure []) (fmap S.toList' (getRow p w))
  let es'' = filter (\(EntityComponent e' _, _) -> e == e') es'
  liftIO $ mapM_ (\(EntityComponent _ c, g) -> g (f c)) es''
  return $ fmap (\(EntityComponent _ c, _) -> c) (listToMaybe es'')
get' e es (BindQ qb f) w = do
  a <- get' e es qb w
  case a of
    Just a' -> get' e es (f a') w
    Nothing -> return Nothing
get' _ _ (LiftQ a) _ = fmap Just a
