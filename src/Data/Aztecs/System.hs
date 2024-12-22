{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aztecs.System (Access (..), query, System (..), runSystem) where

import Control.Monad.State (StateT (runStateT))
import Data.Aztecs.Command
import Data.Aztecs.Query
  ( Query (..),
    ReadWrites (..),
  )
import Data.Aztecs.Task
import Data.Aztecs.World
  ( World,
  )
import Data.Functor ((<&>))
import Data.Typeable
import Prelude hiding (all, read)

newtype Access m a = Access {unAccess :: ([ReadWrites], World -> m a)}
  deriving (Functor)

instance (Applicative m) => Applicative (Access m) where
  pure a = Access $ (mempty, \_ -> pure a)
  Access (fRws, f) <*> Access (aRws, a) =
    Access $ (fRws <> aRws, \w -> f w <*> a w)

query :: (Applicative m) => Query a -> Access m (Query a)
query (Query a f g) = Access $ ([a], \_ -> pure $ Query a f g)

class (Typeable a) => System m a where
  access :: Access m a
  run :: a -> Task m a ()

runSystem :: (Monad m, System m a) => World -> m (a, [Command m ()], World)
runSystem w = do
  let (_, f) = unAccess access
  i <- f w
  let (Task t) = run i
  runStateT t (i, [], w) <&> snd
