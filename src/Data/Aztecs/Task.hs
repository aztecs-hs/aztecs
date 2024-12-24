{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aztecs.Task
  ( Task (..),
    command,
    runTask,
  )
where

import Control.Monad.IO.Class
import Control.Monad.State (StateT (..))
import qualified Control.Monad.State as S
import Data.Aztecs.Command
import Data.Aztecs.World (World)
import Prelude hiding (all)

-- | System task.
newtype Task m s a = Task (StateT (s, [Command m ()], World) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

runTask :: (Functor m) => Task m s a -> s -> World -> m (a, s, [Command m ()], World)
runTask (Task t) s w = fmap (\(a, (s', cmds, w')) -> (a, s', cmds, w')) (S.runStateT t (s, [], w))

-- | Queue a `Command` to run after this system is complete.
command :: (Monad m) => Command m () -> Task m a ()
command cmd = Task $ do
  (s, cmds, w) <- S.get
  S.put $ (s, cmds <> [cmd], w)
