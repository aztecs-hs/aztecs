{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aztecs.Task
  ( Task (..),
    get,
    all,
    update,
    command,
    runTask,
  )
where

import Control.Monad.IO.Class
import Control.Monad.State (StateT (..))
import qualified Control.Monad.State as S
import Data.Aztecs.Command
import Data.Aztecs.Query (Query, Write)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.World (Component, Entity, World)
import qualified Data.Aztecs.World as W
import Data.Dynamic (Typeable)
import Prelude hiding (all)

-- | System task.
newtype Task m s a = Task (StateT (s, [Command m ()], World) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

runTask :: (Functor m) => Task m s a -> s -> World -> m (a, s, [Command m ()], World)
runTask (Task t) s w = fmap (\(a, (s', cmds, w')) -> (a, s', cmds, w')) (S.runStateT t (s, [], w))

-- | Update a single query match.
update :: (Component a, Typeable a, Monad m) => Write a -> (a -> a) -> Entity -> Task m s ()
update wr f e = Task $ do
  (s, cmds, w) <- S.get
  S.put $ (s, cmds, W.adjust (snd $ Q.unWrite wr) f e w)

-- | Query a single match.
get :: (Monad m) => Entity -> Query a -> Task m s (Maybe a)
get e q = Task $ do
  (_, _, w) <- S.get
  return $ Q.query e q w

-- | Query all matches.
all :: (Monad m) => Query a -> Task m s [a]
all q = Task $ do
  (_, _, w) <- S.get
  return $ Q.all q w

-- | Queue a `Command` to run after this system is complete.
command :: (Monad m) => Command m () -> Task m a ()
command cmd = Task $ do
  (s, cmds, w) <- S.get
  S.put $ (s, cmds <> [cmd], w)
