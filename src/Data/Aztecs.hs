{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aztecs
  ( Entity,
    EntityComponent (..),
    Storage (..),
    table,
    Component (..),
    World,
    newWorld,
    get,
    Query,
    Write (..),
    QueryResult (..),
    Access (..),
    query,
    Task (..),
    spawn,
    insert,
    getQuery,
    queryAll,
    adjust,
    adjustQuery,
    System (..),
    runSystem,
    W.read,
    W.write,
  )
where

import Control.Monad.State (MonadIO (liftIO), StateT (runStateT))
import qualified Control.Monad.State as S
import Data.Aztecs.World
  ( Component (..),
    Entity,
    EntityComponent (..),
    Query (..),
    QueryResult,
    ReadWrites,
    Storage (..),
    World,
    Write,
    get,
    newWorld,
    table,
  )
import qualified Data.Aztecs.World as W
import Data.Functor ((<&>))
import Data.Typeable
import Prelude hiding (read)

newtype Access m a = Access {unAccess :: World -> m (ReadWrites, a)}
  deriving (Functor)

instance (Applicative m) => Applicative (Access m) where
  pure a = Access $ (\_ -> pure (mempty, a))
  Access f <*> Access a = Access $ (\w -> (\(rs, f') (rs', a') -> (rs <> rs', f' a')) <$> f w <*> a w)

query :: (Applicative m) => Query a -> Access m (Query a)
query (Query a f g) = Access $ (\_ -> pure (a, (Query a f g)))

data Task m s a = Task (StateT (s, World) m a)
  deriving (Functor)

instance (Monad m) => Applicative (Task m s) where
  pure a = Task $ pure a
  Task f <*> Task a = Task $ f <*> a

instance (Monad m) => Monad (Task m s) where
  Task a >>= f = Task $ a >>= (\a' -> case f a' of Task b -> b)

instance (MonadIO m) => MonadIO (Task m s) where
  liftIO a = Task $ liftIO a

spawn :: (Component a, Typeable a, Monad m) => a -> Task m s Entity
spawn a = Task $ do
  (s, w) <- S.get
  let (e, w') = W.spawn a w
  S.put $ (s, w')
  return e

insert :: (Component a, Typeable a, Monad m) => Entity -> a -> Task m s ()
insert e a = Task $ do
  (s, w) <- S.get
  S.put $ (s, W.insert e a w)
  return ()

adjust :: (Component a, Typeable a, Monad m) => Write a -> (a -> a) -> Entity -> Task m s ()
adjust w f e = Task $ do
  (s, w') <- S.get
  S.put $ (s, W.adjust w f e w')
  return ()

getQuery :: (Monad m) => Entity -> Query a -> Task m s (Maybe a)
getQuery e q = Task $ do
  (_, w) <- S.get
  return $ W.query e q w

queryAll :: (Monad m) => Query a -> Task m s (QueryResult a)
queryAll q = Task $ do
  (_, w) <- S.get
  return $ W.queryAll q w

adjustQuery :: (Component a, Typeable a, Monad m) => QueryResult (Write a) -> (a -> a) -> Task m s ()
adjustQuery q f = Task $ do
  (s, w) <- S.get
  S.put $ (s, W.adjustQuery q f w)
  return ()

class System m a where
  access :: Access m a
  run :: a -> Task m a ()

runSystem :: (Monad m, System m a) => World -> m (a, World)
runSystem w = do
  (_, i :: a) <- unAccess access w
  let (Task t) = run i
  runStateT t (i, w) <&> snd
