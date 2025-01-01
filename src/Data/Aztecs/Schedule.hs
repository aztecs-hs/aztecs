{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Schedule
  ( Node (..),
    Schedule (..),
    ScheduleNode (..),
    runSchedule,
    Startup,
    Update,
    Constraint (..),
    before,
    after,
    Scheduler (..),
    schedule,
    SchedulerGraph (..),
    buildScheduler,
    runSchedulerGraph,
    runScheduler,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.State (StateT (runStateT))
import Data.Aztecs.Command
import Data.Aztecs.System
import Data.Aztecs.World
  ( World,
    newWorld,
    union,
  )
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.List (groupBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Prelude hiding (all, read)

data Constraint = Before TypeRep | After TypeRep

before :: forall m a. (System m a) => Constraint
before = Before $ typeOf (Proxy :: Proxy a)

after :: forall m a. (System m a) => Constraint
after = After $ typeOf (Proxy :: Proxy a)

data Node m where
  Node :: (System m a) => Proxy a -> Cache -> Node m

data ScheduleNode m = ScheduleNode (Node m) [Constraint]

data Schedule m = Schedule (Map TypeRep (ScheduleNode m))

instance Semigroup (Schedule m) where
  Schedule a <> Schedule b = Schedule $ a <> b

instance Monoid (Schedule m) where
  mempty = Schedule mempty

data GraphNode m = GraphNode (Node m) (Set TypeRep) (Set TypeRep)

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
            -- TODO || hasConflict (GraphNode a deps aBefores) (GraphNode b deps' bBefores)
        )
        nodes

runNode :: Node IO -> World -> IO (Node IO, Maybe (Access IO ()), [Command IO ()], World)
runNode (Node p cache) w =
  runSystemProxy p cache w <&> (\(next, a', cmds, w') -> (Node p a', next, cmds, w'))

runSystemProxy :: forall a. (System IO a) => Proxy a -> Cache -> World -> IO (Maybe (Access IO ()), Cache, [Command IO ()], World)
runSystemProxy _ = runSystem' @a

-- | Run a `Command`, returning any temporary `Entity`s and the updated `World`.
runCommand :: Command IO () -> World -> IO (World)
runCommand (Command cmd) w = snd <$> runStateT cmd w

runSchedule :: [[GraphNode IO]] -> World -> IO ([[GraphNode IO]], World)
runSchedule nodes w =
  foldrM
    ( \nodeGroup (nodeAcc, w') -> do
        results <-
          mapConcurrently
            ( \(GraphNode n as bs) -> do
                (n', next, cmds, w'') <- runNode n w
                return ((next, (GraphNode n' as bs)), cmds, w'')
            )
            nodeGroup
        let (nexts, cmdLists, worlds) =
              foldr
                ( \(n, b, c) (ns, bs, cs) ->
                    (n : ns, b : bs, c : cs)
                )
                ([], [], [])
                results
            finalWorld = foldr union w' worlds
            (cmds, w'') = (concat cmdLists, finalWorld)

        (w''', nodes', cmds') <-
          foldrM
            ( \(a, (GraphNode (Node p cache) as bs)) (wAcc, nodeAcc', cmdAcc) -> case a of
                Just a' -> do
                  ((), wAcc', cache', cmdAcc') <- runAccess' a' wAcc cache
                  return (wAcc', (GraphNode (Node p cache') as bs) : nodeAcc', cmdAcc' ++ cmdAcc)
                Nothing -> return (w, (GraphNode (Node p cache) as bs) : nodeAcc', cmdAcc)
            )
            (w'', [], [])
            nexts

        w'''' <- foldrM (\cmd wAcc -> runCommand cmd wAcc) w''' (cmds ++ cmds')
        return (nodes' : nodeAcc, w'''')
    )
    ([], w)
    nodes

newtype Scheduler m = Scheduler (Map TypeRep (Schedule m))
  deriving (Monoid)

instance Semigroup (Scheduler m) where
  Scheduler a <> Scheduler b = Scheduler $ Map.unionWith (<>) a b

data Startup

data Update

schedule :: forall l m s. (Typeable l, System m s) => [Constraint] -> Scheduler m
schedule cs =
  Scheduler $
    Map.singleton
      (typeOf (Proxy :: Proxy l))
      ( Schedule $
          Map.singleton
            (typeOf (Proxy :: Proxy s))
            (ScheduleNode (Node (Proxy :: Proxy s) mempty) cs)
      )

newtype SchedulerGraph m = SchedulerGraph (Map TypeRep [[GraphNode m]])

buildScheduler :: (Monad m) => Scheduler m -> SchedulerGraph m
buildScheduler (Scheduler s) = SchedulerGraph $ fmap build s

runSchedulerGraph :: forall l. (Typeable l) => SchedulerGraph IO -> World -> IO (SchedulerGraph IO, World)
runSchedulerGraph (SchedulerGraph g) w = case Map.lookup (typeOf (Proxy :: Proxy l)) g of
  Just s -> do
    (nodes, w') <- runSchedule s w
    let g' = Map.insert (typeOf (Proxy :: Proxy l)) nodes g
    return (SchedulerGraph g', w')
  Nothing -> return (SchedulerGraph g, w)

runScheduler :: Scheduler IO -> IO ()
runScheduler s = do
  let g = buildScheduler s
  (g', w) <- runSchedulerGraph @Startup g newWorld
  let go gAcc wAcc = do
        (gAcc', wAcc') <- runSchedulerGraph @Update gAcc wAcc
        go gAcc' wAcc'
  go g' w
