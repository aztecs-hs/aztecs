{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aztecs.System (Access (..), query, System (..), runSystem) where

import Control.Monad.State (StateT (runStateT))
import Data.Aztecs.Command
import Data.Aztecs.Query
  ( Query (..),
    QueryBuilder (QueryBuilder),
    ReadWrites (..),
  )
import Data.Aztecs.Task
import Data.Aztecs.World
  ( World (..),
  )
import qualified Data.Aztecs.World.Archetypes as A
import Data.Functor ((<&>))
import Data.Typeable
import Prelude hiding (all, read)

newtype Access m a = Access {unAccess :: ([ReadWrites], World -> m (a, World, Access m a))}
  deriving (Functor)

instance (Monad m) => Applicative (Access m) where
  pure a = Access $ (mempty, \w -> pure (a, w, pure a))
  Access (fRws, f) <*> Access (aRws, a) =
    Access $
      ( fRws <> aRws,
        \w -> do
          (f', w', fA) <- f w
          (a', w'', aA) <- a w'
          pure (f' a', w'', fA <*> aA)
      )

query :: (Monad m) => QueryBuilder a -> Access m (Query a)
query (QueryBuilder rws a f) =
  Access
    ( [rws],
      \(World cs as) ->
        let (aId, as') = A.insertArchetype a cs as
            q = f aId (World cs as)
         in pure (q, World cs as', pure q)
    )

class (Typeable a) => System m a where
  access :: Access m a
  run :: a -> Task m a ()

runSystem :: (Monad m, System m a) => World -> m (a, Access m a, [Command m ()], World)
runSystem w = do
  let (_, f) = unAccess access
  (i, w', acc) <- f w
  let (Task t) = run i
  (a, cmds, w'') <- runStateT t (i, [], w') <&> snd
  return (a, acc, cmds, w'')
