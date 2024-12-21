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
    Query,
    Write (..),
    QueryResult (..),
    Access (..),
    query,
    System (..),
    runSystem,
  )
where

import Control.Monad.State (StateT (runStateT))
import Data.Aztecs.Query
  ( Query (..),
    QueryResult (..),
    ReadWrites (..),
    Write (..),
  )
import Data.Aztecs.Task
import Data.Aztecs.World
  ( Component (..),
    Entity,
    EntityComponent (..),
    Storage (..),
    World,
    newWorld,
    table,
  )
import Data.Functor ((<&>))
import Prelude hiding (all, read)

newtype Access m a = Access {unAccess :: World -> m (ReadWrites, a)}
  deriving (Functor)

instance (Applicative m) => Applicative (Access m) where
  pure a = Access $ (\_ -> pure (mempty, a))
  Access f <*> Access a =
    Access $
      (\w -> (\(rs, f') (rs', a') -> (rs <> rs', f' a')) <$> f w <*> a w)

query :: (Applicative m) => Query a -> Access m (Query a)
query (Query a f g) = Access $ (\_ -> pure (a, (Query a f g)))

class System m a where
  access :: Access m a
  run :: a -> Task m a ()

runSystem :: (Monad m, System m a) => World -> m (a, World)
runSystem w = do
  (_, i :: a) <- unAccess access w
  let (Task t) = run i
  runStateT t (i, w) <&> snd
