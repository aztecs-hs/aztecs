{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs
  ( Entity,
    EntityComponent (..),
    Component (..),
    World,
    Query,
    Write,
    QueryResult (..),
    Access (..),
    query,
    System (..),
    runSystem,
    Constraint (..),
    before,
    after,
    Schedule (..),
    Startup,
    Update,
    runSchedule,
    Scheduler (..),
    schedule,
    build,
    runScheduler,
    OnSpawn,
    OnInsert,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Aztecs.Command
import Data.Aztecs.Query
  ( Query (..),
    QueryResult (..),
    ReadWrites (..),
    Write,
  )
import Data.Aztecs.Task
import Data.Aztecs.World
  ( Component (..),
    Entity,
    EntityComponent (..),
    World,
    newWorld,
    union,
  )
import qualified Data.Aztecs.World as W
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.List (find, groupBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
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

class System m a where
  access :: Access m a
  run :: a -> Task m a ()

runSystem :: (Monad m, System m a) => World -> m (a, [Command m ()], World)
runSystem w = do
  let (_, f) = unAccess access
  i <- f w
  let (Task t) = run i
  runStateT t (i, [], w) <&> snd

runSystemProxy :: (Monad m, System m a) => (Proxy a) -> World -> m (a, [Command m ()], World)
runSystemProxy _ w = runSystem w

accessSystemProxy :: (Monad m, System m a) => (Proxy a) -> Access m a
accessSystemProxy _ = access

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

data GraphNode m = GraphNode (Node m) (Set TypeRep) (Set TypeRep)

nodeReadWrites :: forall m. (Monad m) => Node m -> [ReadWrites]
nodeReadWrites (Node p) = fst $ unAccess $ accessSystemProxy @m p

hasConflict :: (Monad m) => GraphNode m -> GraphNode m -> Bool
hasConflict (GraphNode a _ _) (GraphNode b _ _) =
  let f n = map (\(ReadWrites rs ws) -> (Set.toList rs, Set.toList ws)) (nodeReadWrites n)
   in any (uncurry rwHasConflict) [(x, y) | x <- f a, y <- f b]
  where
    rwHasConflict (rs, (w : ws)) (rs', ws') =
      (isJust $ find (== w) ws) || rwHasReadConflict (rs, ws) (rs', ws')
    rwHasConflict rws rws' = rwHasReadConflict rws rws'

    rwHasReadConflict ((r : rs), ws) (rs', ws') =
      (isJust $ find (== r) ws) || rwHasReadConflict (rs, ws) (rs', ws')
    rwHasReadConflict _ _ = False

build :: (Monad m) => Schedule m -> [[GraphNode m]]
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
                    Map.adjust
                      ( \(GraphNode n deps bs) ->
                          GraphNode n (Set.singleton i <> deps) bs
                      )
                      i
                      acc'
                )
                acc
                befores
          )
          graph
          graph
      nodes =
        sortBy
          ( \(GraphNode _ deps _) (GraphNode _ deps' _) ->
              compare (length deps') (length deps)
          )
          (Map.elems graph')
   in groupBy
        ( \(GraphNode a deps aBefores) (GraphNode b deps' bBefores) ->
            (length deps == length deps')
              || hasConflict (GraphNode a deps aBefores) (GraphNode b deps' bBefores)
        )
        nodes

runNode :: (Monad m) => Node m -> World -> m ([Command m ()], World)
runNode (Node p) w = runSystemProxy p w <&> (\(_, cmds, w') -> (cmds, w'))

data OnSpawn a = OnSpawn (Proxy a)

instance (Component a) => Component (OnSpawn a)

data OnInsert a = OnInsert (Proxy a)

instance (Component a) => Component (OnInsert a)

data TemporaryComponent where
  TemporaryComponent :: (Component a) => Entity -> Proxy a -> TemporaryComponent

-- | Run a `Command`, returning any temporary `Entity`s and the updated `World`.
runCommand :: (Monad m) => Command m () -> World -> m ([TemporaryComponent], World)
runCommand (Command cmd) w = do
  (((), w'), edits) <- runWriterT $ runStateT cmd w
  return $
    foldr
      ( \edit (cs, w'') -> case edit of
          Spawn e p -> (TemporaryComponent e p : cs, W.insert e (OnSpawn p) w'')
          Insert e p -> (TemporaryComponent e p : cs, W.insert e (OnInsert p) w'')
      )
      ([], w')
      edits

runSchedule' :: [[GraphNode IO]] -> World -> IO ([TemporaryComponent], World)
runSchedule' nodes w =
  foldrM
    ( \nodeGroup (csAcc, w') -> do
        results <- mapConcurrently (\(GraphNode n _ _) -> runNode n w) nodeGroup
        let (cmdLists, worlds) = unzip results
            finalWorld = foldr union w' worlds
            (cmds, w'') = (concat cmdLists, finalWorld)
        foldrM
          ( \cmd (csAcc', wAcc) -> do
              (cs, wAcc') <- runCommand cmd wAcc
              return (cs ++ csAcc', wAcc')
          )
          (csAcc, w'')
          cmds
    )
    ([], w)
    nodes

removeProxy :: forall c. (Component c) => Entity -> Proxy c -> World -> World
removeProxy e _ = W.remove @c e

runSchedule :: [[GraphNode IO]] -> World -> IO World
runSchedule nodes w = do
  (cs, w') <- runSchedule' nodes w
  foldrM (\(TemporaryComponent e p) wAcc -> return $ removeProxy e p wAcc) w' cs

newtype Scheduler m = Scheduler (Map TypeRep (Schedule m))
  deriving (Monoid)

instance Semigroup (Scheduler m) where
  Scheduler a <> Scheduler b = Scheduler $ Map.unionWith (<>) a b

data Startup

data Update

schedule :: forall l m a. (Typeable l, System m a, Typeable a) => [Constraint] -> Scheduler m
schedule cs = f (Proxy :: Proxy l) (Proxy :: Proxy a)
  where
    f :: Proxy l -> Proxy a -> Scheduler m
    f lp p =
      Scheduler $
        Map.singleton
          (typeOf lp)
          (Schedule $ Map.singleton (typeOf p) (ScheduleNode (Node p) cs))

newtype SchedulerGraph m = SchedulerGraph (Map TypeRep [[GraphNode m]])

buildScheduler :: (Monad m) => Scheduler m -> SchedulerGraph m
buildScheduler (Scheduler s) = SchedulerGraph $ fmap build s

runScheduler' :: forall l. (Typeable l) => SchedulerGraph IO -> World -> IO World
runScheduler' (SchedulerGraph g) w = case Map.lookup (typeOf (Proxy :: Proxy l)) g of
  Just s -> runSchedule s w
  Nothing -> return w

runScheduler :: Scheduler IO -> IO ()
runScheduler s = do
  let g = buildScheduler s
  w <- runScheduler' @Startup g newWorld
  let go wAcc = do
        wAcc' <- runScheduler' @Update g wAcc
        go wAcc'
  _ <- go w
  return ()
