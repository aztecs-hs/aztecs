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
    Cache (..),
  )
where

import Control.Monad.State (MonadState (..), StateT (runStateT))
import Data.Aztecs.Command
import Data.Aztecs.Query
  ( Query (..),
    QueryBuilder (QueryBuilder),
    ReadWrites (..),
  )
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.Task (Task (..))
import Data.Aztecs.World
  ( Component,
    EntityComponent,
    World (..),
  )
import qualified Data.Aztecs.World.Archetypes as A
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable
import Prelude hiding (all, read)

newtype Cache = Cache (Map TypeRep Dynamic)
  deriving (Semigroup, Monoid)

data Access m a = Access [ReadWrites] (StateT (World, Cache) m a)
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
query (QueryBuilder rws a f) =
  Access
    [rws]
    ( do
        (World cs as, cache) <- get
        let (aId, as') = A.insertArchetype a cs as
            q = f aId (World cs as)
        put (World cs as', cache)
        return q
    )

-- | Query all matches.
all :: forall m a. (Typeable a, Monad m) => QueryBuilder a -> Access m [a]
all (QueryBuilder rws a f) =
  Access
    [rws]
    ( do
        (World cs as, Cache cache) <- get
        let (q, w) = case Map.lookup (typeOf (Proxy :: Proxy a)) cache of
              Just q' -> (fromMaybe (error "TODO") (fromDynamic q'), World cs as)
              Nothing ->
                let (aId, as') = A.insertArchetype a cs as
                 in (f aId (World cs as'), (World cs as'))
        put (w, Cache cache)
        return $ Q.all q w
    )

alter :: forall m c. (Component c, Monad m) => (EntityComponent c -> c) -> QueryBuilder (EntityComponent c) -> Access m ()
alter f (QueryBuilder rws a g) =
  Access
    [rws]
    ( do
        (World cs as, (Cache cache)) <- get
        let (q, w) = case Map.lookup (typeOf (Proxy :: Proxy c)) cache of
              Just q' -> (fromMaybe (error "TODO") (fromDynamic q'), World cs as)
              Nothing ->
                let (aId, as') = A.insertArchetype a cs as
                 in (g aId (World cs as'), (World cs as'))
            es = Q.all q w
        let w' = Q.alter es f w
        put (w', (Cache (Map.insert (typeOf (Proxy :: Proxy c)) (toDyn q) cache)))
    )

class (Typeable a) => System m a where
  access :: Access m a
  run :: a -> Task m a ()

runSystem :: forall m a. (Monad m, System m a) => Cache -> World -> m (Cache, [Command m ()], World)
runSystem cache w = do
  let (Access _ f) = access @m @a
  (i, (w', acc)) <- runStateT f (w, cache)
  let (Task t) = run i
  (_, cmds, w'') <- runStateT t (i, [], w') <&> snd
  return (acc, cmds, w'')
