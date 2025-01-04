{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System
  ( Access (..),
    runAccess,
    runAccess',
    all,
    get,
    command,
    System (..),
    runSystem,
    runSystem',
    Cache (..),
  )
where

import Control.Monad.IO.Class
import Data.Aztecs.Command
import Data.Aztecs.Query (Query (..))
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.World (Entity, World (..))
import Data.Aztecs.World.Archetypes (Archetype, ArchetypeId)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Foldable (foldrM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Prelude hiding (all, read)

newtype Cache = Cache (Map Archetype ArchetypeId)
  deriving (Semigroup, Monoid)

data Access m a where
  PureA :: a -> Access m a
  MapA :: (a -> b) -> Access m a -> Access m b
  AppA :: Access m (a -> b) -> Access m a -> Access m b
  BindA :: Access m a -> (a -> Access m b) -> Access m b
  AllA :: Archetype -> Query m a -> Access m [(Entity, a)]
  GetA :: Archetype -> Query m a -> Entity -> Access m (Maybe a)
  CommandA :: Command m () -> Access m ()
  LiftA :: m a -> Access m a

instance Functor (Access m) where
  fmap = MapA

instance (Monad m) => Applicative (Access m) where
  pure = PureA
  (<*>) = AppA

instance (Monad m) => Monad (Access m) where
  (>>=) = BindA

instance (MonadIO m) => MonadIO (Access m) where
  liftIO io = LiftA (liftIO io)

buildQuery :: Query m a -> Archetype
buildQuery (PureQ _) = mempty
buildQuery (MapQ _ qb) = buildQuery qb
buildQuery (AppQ f a) = buildQuery f <> buildQuery a
buildQuery EntityQ = mempty
buildQuery (ReadQ a) = a
buildQuery (WriteQ _ a) = a
buildQuery (BindQ a _) = buildQuery a
buildQuery (LiftQ _) = mempty

runAccess :: Access IO a -> World -> Cache -> IO (Either (Access IO a) a, World, Cache, [Command IO ()])
runAccess (PureA a) w c = return (Right a, w, c, [])
runAccess (MapA f a) w cache = do
  (a', w', cache', cmds) <- runAccess a w cache
  return
    ( case a' of
        Left a'' -> Left (MapA f a'')
        Right a'' -> Right (f a''),
      w',
      cache',
      cmds
    )
runAccess (AppA f a) w cache = do
  (f', w', cache', cmds) <- runAccess f w cache
  (a', w'', cache'', cmds') <- runAccess a w' cache'
  return
    ( case (f', a') of
        (Right f'', Right a'') -> Right (f'' a'')
        (Left f'', _) -> Left (AppA f'' a)
        (_, Left a'') -> Left (AppA f a''),
      w'',
      cache'',
      cmds ++ cmds'
    )
runAccess (BindA a f) w cache = do
  (a', w', cache', cmds) <- runAccess a w cache
  case a' of
    Left a'' -> return (Left (BindA a'' f), w', cache', cmds)
    Right a'' -> runAccess (f a'') w' cache'
runAccess (AllA a qb) w (Cache cache) = case Map.lookup (buildQuery qb) cache of
  Just aId -> do
    es <- Q.all aId qb w
    return (Right es, w, Cache cache, [])
  Nothing -> return (Left (AllA a qb), w, Cache cache, [])
runAccess (GetA arch q e) w (Cache cache) = do
  case Map.lookup arch cache of
    Just aId -> do
      a <- Q.get aId q e w
      return (Right a, w, Cache cache, [])
    Nothing -> return (Left (GetA arch q e), w, Cache cache, [])
runAccess (CommandA cmd) w cache = return (Right (), w, cache, [cmd])
runAccess (LiftA io) w cache = do
  a <- liftIO io
  return (Right a, w, cache, [])

runAccess' :: Access IO a -> World -> Cache -> IO (a, World, Cache, [Command IO ()])
runAccess' (PureA a) w c = return (a, w, c, [])
runAccess' (MapA f a) w cache = do
  (a', w', cache', cmds) <- runAccess' a w cache
  return (f a', w', cache', cmds)
runAccess' (AppA f a) w cache = do
  (f', w', cache', cmds) <- runAccess' f w cache
  (a', w'', cache'', cmds') <- runAccess' a w' cache'
  return (f' a', w'', cache'', cmds ++ cmds')
runAccess' (BindA a f) w cache = do
  (a', w', cache', cmds) <- runAccess' a w cache
  (b, w'', cache'', cmds') <- runAccess' (f a') w' cache'
  return (b, w'', cache'', cmds ++ cmds')
runAccess' (AllA arch q) (World cs as) (Cache cache) = do
  (aId, w, cache') <- case Map.lookup arch cache of
    Just q' -> return (q', World cs as, cache)
    Nothing -> do
      (x, as') <- A.insertArchetype arch cs as
      return (x, World cs as', Map.insert arch x cache)
  es <- Q.all aId q w
  return (es, w, Cache cache', [])
runAccess' (GetA arch q e) (World cs as) (Cache cache) = do
  (aId, w, cache') <- case Map.lookup arch cache of
    Just q' -> return (q', World cs as, cache)
    Nothing -> do
      (x, as') <- A.insertArchetype arch cs as
      return (x, World cs as', Map.insert arch x cache)
  a <- Q.get aId q e w
  return (a, w, Cache cache', [])
runAccess' (CommandA cmd) w cache = return ((), w, cache, [cmd])
runAccess' (LiftA io) w cache = do
  a <- liftIO io
  return (a, w, cache, [])

-- | Query all matches.
all :: (Monad m) => Query m a -> Access m [a]
all q = do
  (as, _) <- all' mempty q
  return (map snd as)

all' :: (Monad m) => Archetype -> Query m a -> Access m ([(Entity, a)], Archetype)
all' arch (PureQ _) = pure ([], arch)
all' arch (MapQ f a) = all' arch (f <$> a)
all' arch (AppQ f a) = all' arch (f <*> a)
all' arch (BindQ a f) = do
  (as, arch') <- all' arch a
  foldrM
    ( \(e, a') (acc, archAcc) -> do
        (b, archAcc') <- get' archAcc e (f a')
        case b of
          Just b' -> return ((e, b') : acc, archAcc')
          Nothing -> return (acc, archAcc')
    )
    ([], arch')
    as
all' arch (LiftQ m) = do
  _ <- LiftA m
  return ([], arch)
all' arch (ReadQ arch') = do
  let arch'' = (arch <> arch')
  as <- AllA arch'' (ReadQ arch')
  return (as, arch'')
all' arch (WriteQ f arch') = do
  let arch'' = (arch <> arch')
  as <- AllA arch'' (WriteQ f arch')
  return (as, arch'')
all' arch EntityQ = do
  es <- AllA arch EntityQ
  return (es, arch)

get :: (Monad m) => Entity -> Query m a -> Access m (Maybe a)
get e q = fst <$> get' mempty e q

get' :: (Monad m) => Archetype -> Entity -> Query m a -> Access m (Maybe a, Archetype)
get' arch _ (PureQ a) = pure (Just a, arch)
get' arch e (MapQ f qb) = get' arch e (f <$> qb)
get' arch e (AppQ f a) = get' arch e (f <*> a)
get' arch e (BindQ a f) = do
  (a', arch') <- get' arch e a
  case fmap f a' of
    Just a'' -> get' arch' e a''
    Nothing -> return (Nothing, arch')
get' arch _ (LiftQ m) = do
  a <- LiftA m
  return (Just a, arch)
get' arch e (ReadQ arch') = do
  let arch'' = (arch <> arch')
  a <- GetA arch'' (ReadQ arch') e
  return (a, arch'')
get' arch e (WriteQ f arch') = do
  let arch'' = (arch <> arch')
  a <- GetA arch'' (WriteQ f arch') e
  return (a, arch'')
get' arch e EntityQ = return (Just e, arch)

command :: Command m () -> Access m ()
command = CommandA

class (Typeable a) => System m a where
  access :: Access m ()

runSystem :: forall a. (System IO a) => World -> IO (World)
runSystem w = snd <$> runSystem' @a (Cache mempty) w

runSystem' :: forall a. (System IO a) => Cache -> World -> IO (Cache, World)
runSystem' c w = do
  (_, w', c', _) <- runAccess' (access @_ @a) w c
  return (c', w')
