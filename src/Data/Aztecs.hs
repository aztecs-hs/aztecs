{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
    Constraint (..),
    before,
    after,
    Schedule (..),
    schedule,
    build,
    runSchedule,
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
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Prelude hiding (all, read)

newtype Access m a = Access {unAccess :: (ReadWrites, World -> m a)}
  deriving (Functor)

instance (Applicative m) => Applicative (Access m) where
  pure a = Access $ (mempty, \_ -> pure a)
  Access (fRws, f) <*> Access (aRws, a) =
    Access $ (fRws <> aRws, \w -> f w <*> a w)

query :: (Applicative m) => Query a -> Access m (Query a)
query (Query a f g) = Access $ (a, \_ -> pure $ Query a f g)

class System m a where
  access :: Access m a
  run :: a -> Task m a ()

runSystem :: (Monad m, System m a) => World -> m (a, World)
runSystem w = do
  let (_, f) = unAccess access
  i <- f w
  let (Task t) = run i
  runStateT t (i, w) <&> snd

runSystemProxy :: (Monad m, System m a) => (Proxy a) -> World -> m (a, World)
runSystemProxy _ w = runSystem w

data Constraint = Before TypeRep | After TypeRep

before :: forall a. (Typeable a) => Constraint
before = Before $ typeOf (Proxy :: Proxy a)

after :: forall a. (Typeable a) => Constraint
after = After $ typeOf (Proxy :: Proxy a)

data Node m where
  Node :: (System m a) => (Proxy a) -> Node m

data ScheduleNode m = ScheduleNode (Node m) [Constraint]

data Schedule m = Schedule (Map TypeRep (ScheduleNode m))

instance Semigroup (Schedule m) where
  Schedule a <> Schedule b = Schedule $ a <> b

instance Monoid (Schedule m) where
  mempty = Schedule mempty

schedule :: forall m a. (System m a, Typeable a) => [Constraint] -> Schedule m
schedule cs = f (Proxy :: Proxy a)
  where
    f :: Proxy a -> Schedule m
    f p = Schedule $ Map.singleton (typeOf p) (ScheduleNode (Node p) cs)

data GraphNode m = GraphNode (Node m) (Set TypeRep) (Set TypeRep)

build :: Schedule m -> [Node m]
build (Schedule s) =
  let graph =
        fmap
          ( \(ScheduleNode node constraints) ->
              let (deps, befores) =
                    foldr
                      ( \c (depAcc, afterAcc) -> case c of
                          Before i -> (depAcc, [i])
                          After i -> (depAcc ++ [i], afterAcc)
                      )
                      ([], [])
                      constraints
               in GraphNode node (Set.fromList deps) (Set.fromList befores)
          )
          s
      graph' =
        foldr
          ( \(GraphNode _ _ befores) acc ->
              foldr
                ( \i acc' ->
                    Map.adjust (\(GraphNode n deps bs) -> GraphNode n (Set.singleton i <> deps) bs) i acc'
                )
                acc
                befores
          )
          graph
          graph
   in map
        (\(_, GraphNode n _ _) -> n)
        $ sortBy
          ( \(_, GraphNode _ deps _) (_, GraphNode _ deps' _) ->
              compare (length deps') (length deps)
          )
          (Map.toList graph')

runNode :: (Monad m) => Node m -> World -> m World
runNode (Node p) w = runSystemProxy p w <&> snd

runSchedule :: (Monad m) => Schedule m -> m ()
runSchedule s = do
  let nodes = build s
  _ <- foldrM (\n w -> runNode n w) newWorld nodes
  return ()
