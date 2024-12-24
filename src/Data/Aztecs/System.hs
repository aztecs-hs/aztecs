{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.System
  ( Access (..),
    query,
    System (..),
    runSystem,
    all,
    alter,
    task,
    Cache (..),
  )
where

import Control.Monad.State (MonadState (..), MonadTrans (lift), StateT (runStateT))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Data.Aztecs.Command
import Data.Aztecs.Query
  ( Query (..),
    QueryBuilder (..),
    ReadWrites (..),
  )
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.Task (Task (..), runTask)
import Data.Aztecs.World
  ( Component,
    EntityComponent,
    World (..),
  )
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable
import Prelude hiding (all, read)

newtype Cache = Cache (Map TypeRep Dynamic)
  deriving (Semigroup, Monoid)

data Access m a = Access [ReadWrites] (StateT (World, Cache) (WriterT [Task m () ()] m) a)
  deriving (Functor)

instance (Monad m) => Applicative (Access m) where
  pure a = Access mempty (pure a)
  Access fRws f <*> Access aRws a =
    Access
      (fRws <> aRws)
      ( do
          f' <- f
          a' <- a
          return (f' a')
      )

query :: (Monad m) => QueryBuilder a -> Access m (Query a)
query qb =
  Access
    [] -- TODO
    ( do
        (World cs as, cache) <- get
        let (q, w) = Q.buildQuery qb (World cs as)
        put (w, cache)
        return q
    )

-- | Query all matches.
all :: forall m a. (Typeable a, Monad m) => QueryBuilder a -> Access m [a]
all qb =
  Access
    [] -- TODO
    ( do
        (World cs as, Cache cache) <- get
        let (q, w) = case Map.lookup (typeOf (Proxy :: Proxy a)) cache of
              Just q' -> (fromMaybe (error "TODO") (fromDynamic q'), World cs as)
              Nothing -> Q.buildQuery qb (World cs as)
        put (w, Cache cache)
        return $ Q.all q w
    )

alter :: forall m c. (Component c, Monad m) => (EntityComponent c -> c) -> QueryBuilder (EntityComponent c) -> Access m ()
alter f qb =
  Access
    [] -- TODO
    ( do
        (World cs as, (Cache cache)) <- get
        let (q, w) = case Map.lookup (typeOf (Proxy :: Proxy c)) cache of
              Just q' -> (fromMaybe (error "TODO") (fromDynamic q'), World cs as)
              Nothing -> Q.buildQuery qb (World cs as)
            es = Q.all q w
        let w' = Q.alter es f w
        put (w', (Cache (Map.insert (typeOf (Proxy :: Proxy c)) (toDyn q) cache)))
    )

task :: (Monad m) => Access m a -> (a -> Task m a ()) -> Access m ()
task (Access rw f) g =
  Access
    rw
    ( do
        a <- f
        tell
          [ Task $ do
              ((), cmds, w) <- get
              let t = g a
              ((), _, cmds', w') <- lift $ runTask t a w
              put ((), cmds <> cmds', w')
              return ()
          ]
        return ()
    )

class (Typeable a) => System m a where
  access :: Access m ()

runSystem :: forall m a. (Monad m, System m a) => Cache -> World -> m (Cache, [Command m ()], World)
runSystem cache w = do
  let (Access _ f) = access @m @a
  ((_, (w', cache')), tasks) <- runWriterT $ runStateT f (w, cache)

  let go (Task t) (acc, wAcc) = do
        (_, cmds, wAcc') <- runStateT t ((), [], wAcc) <&> snd
        return (acc <> cmds, wAcc')

  (cmds, w'') <- foldrM go ([], w') tasks
  return (cache', cmds, w'')
