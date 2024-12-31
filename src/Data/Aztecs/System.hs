{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System
  ( Access (..),
    runAccess,
    runAccess',
    all,
    command,
    System (..),
    runSystem,
    Cache (..),
  )
where

import Control.Monad.IO.Class
import Data.Aztecs.Command
import Data.Aztecs.Query
  ( Query (..),
  )
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.World
  ( Component,
    Entity,
    EntityComponent (EntityComponent),
    World (..),
  )
import Data.Aztecs.World.Archetypes (Archetype)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable
import Prelude hiding (all, read)

-- TODO is TypeRep a stable ID for queries?
newtype Cache = Cache (Map Archetype Dynamic)
  deriving (Semigroup, Monoid)

data Access (m :: Type -> Type) a where
  PureA :: a -> Access m a
  MapA :: (a -> b) -> Access m a -> Access m b
  AppA :: Access m (a -> b) -> Access m a -> Access m b
  BindA :: Access m a -> (a -> Access m b) -> Access m b
  AllA :: (Typeable a) => Proxy a -> Query a -> Access m [a]
  CommandA :: Command m () -> Access m ()
  LiftIO :: IO a -> Access m a

instance Functor (Access m) where
  fmap = MapA

instance (Monad m) => Applicative (Access m) where
  pure = PureA
  (<*>) = AppA

instance (Monad m) => Monad (Access m) where
  (>>=) = BindA

instance (MonadIO m) => MonadIO (Access m) where
  liftIO = LiftIO

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
runAccess (AllA p qb) (World cs as) (Cache cache) = case Map.lookup (Q.buildQuery qb) cache of
  Just q' -> do
    let (q, w) = (fromMaybe (error "TODO") (fromDynamic q'), World cs as)
    es <- Q.all q qb w
    return (Right es, World cs as, Cache cache, [])
  Nothing -> return (Left (AllA p qb), World cs as, Cache cache, [])
runAccess (CommandA cmd) w cache = return (Right (), w, cache, [cmd])
runAccess (LiftIO io) w cache = do
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
runAccess' (AllA p qb) (World cs as) (Cache cache) = do
  (q, w) <- case Map.lookup (Q.buildQuery qb) cache of
    Just q' -> return (fromMaybe (error "TODO") (fromDynamic q'), World cs as)
    Nothing -> do
      (x, as') <- A.insertArchetype (Q.buildQuery qb) cs as
      return (x, World cs as')
  es <- Q.all q qb w
  return (es, w, Cache cache, [])
runAccess' (CommandA cmd) w cache = return ((), w, cache, [cmd])
runAccess' (LiftIO io) w cache = do
  a <- liftIO io
  return (a, w, cache, [])

-- | Query all matches.
all :: forall m a. (Typeable a, Monad m) => Query a -> Access m [a]
all = AllA (Proxy :: Proxy a)

command :: Command m () -> Access m ()
command = CommandA

class (Typeable a) => System m a where
  access :: Access m ()

runSystem :: forall a. (System IO a) => Cache -> World -> IO (Maybe (Access IO ()), Cache, [Command IO ()], World)
runSystem cache w = do
  (result, w', cache', cmds) <- runAccess (access @IO @a) w cache
  case result of
    Left a -> return (Just a, cache', cmds, w')
    Right _ -> return (Nothing, cache', cmds, w')
