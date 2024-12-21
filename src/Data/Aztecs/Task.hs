{-# LANGUAGE DeriveFunctor #-}

module Data.Aztecs.Task
  ( Task (..),
    spawn,
    insert,
    get,
    all,
    update,
    alter,
  )
where

import Control.Monad.State (MonadIO (liftIO), StateT (..))
import qualified Control.Monad.State as S
import Data.Aztecs.Query (Query, QueryResult, Write (..))
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.World (Component, Entity, World)
import qualified Data.Aztecs.World as W
import Data.Dynamic (Typeable)
import Prelude hiding (all)

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

update :: (Component a, Typeable a, Monad m) => Write a -> (a -> a) -> Entity -> Task m s ()
update (Write wr) f e = Task $ do
  (s, w) <- S.get
  S.put $ (s, W.adjust wr f e w)
  return ()

get :: (Monad m) => Entity -> Query a -> Task m s (Maybe a)
get e q = Task $ do
  (_, w) <- S.get
  return $ Q.query e q w

all :: (Monad m) => Query a -> Task m s (QueryResult a)
all q = Task $ do
  (_, w) <- S.get
  return $ Q.all q w

alter ::
  (Component a, Typeable a, Monad m) =>
  QueryResult (Write a) ->
  (a -> a) ->
  Task m s ()
alter q f = Task $ do
  (s, w) <- S.get
  S.put $ (s, Q.adjust q f w)
  return ()
