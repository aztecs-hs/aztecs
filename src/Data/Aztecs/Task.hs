{-# LANGUAGE DeriveFunctor #-}

module Data.Aztecs.Task
  ( Task (..),
    get,
    all,
    update,
    alter,
    command,
  )
where

import Control.Monad.State (MonadIO (liftIO), StateT (..))
import qualified Control.Monad.State as S
import Data.Aztecs.Command
import Data.Aztecs.Query (Query, QueryResult, Write (..))
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.World (Component, Entity, World)
import qualified Data.Aztecs.World as W
import Data.Dynamic (Typeable)
import Prelude hiding (all)

-- | System task.
data Task m s a = Task (StateT (s, [Command m ()], World) m a)
  deriving (Functor)

instance (Monad m) => Applicative (Task m s) where
  pure a = Task $ pure a
  Task f <*> Task a = Task $ f <*> a

instance (Monad m) => Monad (Task m s) where
  Task a >>= f = Task $ a >>= (\a' -> case f a' of Task b -> b)

instance (MonadIO m) => MonadIO (Task m s) where
  liftIO a = Task $ liftIO a

-- | Update a single query match.
update :: (Component a, Typeable a, Monad m) => Write a -> (a -> a) -> Entity -> Task m s ()
update (Write wr) f e = Task $ do
  (s, cmds, w) <- S.get
  S.put $ (s, cmds, W.adjust wr f e w)
  return ()

-- | Query a single match.
get :: (Monad m) => Entity -> Query a -> Task m s (Maybe a)
get e q = Task $ do
  (_, _, w) <- S.get
  return $ Q.query e q w

-- | Query all matches.
all :: (Monad m) => Query a -> Task m s (QueryResult a)
all q = Task $ do
  (_, _, w) <- S.get
  return $ Q.all q w

-- | Alter the components in a query.
alter ::
  (Component a, Typeable a, Monad m) =>
  QueryResult (Write a) ->
  (a -> a) ->
  Task m s ()
alter q f = Task $ do
  (s, cmds, w) <- S.get
  S.put $ (s, cmds, Q.alter q f w)
  return ()

-- | Queue a `Command` to run after this system is complete.
command :: (Monad m) => Command m () -> Task m a ()
command cmd = Task $ do
  (s, cmds, w) <- S.get
  S.put $ (s, cmds <> [cmd], w)
  return ()
