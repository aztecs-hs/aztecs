{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Schedule
  ( SystemId (..),
    Node (..),
    Schedule (..),
    ScheduleNode (..),
    runSchedule,
    Stage (..),
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
import qualified Control.Monad.State as S
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
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, read)

newtype SystemId = SystemId Int
  deriving (Eq, Ord)

data Constraint = Before SystemId | After SystemId

before :: SystemId -> Constraint
before = Before

after :: SystemId -> Constraint
after = After

data Node m where
  Node :: System m () -> Cache -> Node m

data ScheduleNode m = ScheduleNode (Node m) [Constraint]

data Schedule m = Schedule (Map SystemId (ScheduleNode m))

instance Semigroup (Schedule m) where
  Schedule a <> Schedule b = Schedule $ a <> b

instance Monoid (Schedule m) where
  mempty = Schedule mempty

data GraphNode m = GraphNode (Node m) (Set SystemId) (Set SystemId)

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

runNode :: Node IO -> World -> IO (Node IO, Maybe (System IO ()), [Command IO ()], World)
runNode (Node s cache) w =
  runSystemProxy s cache w <&> (\(next, a', cmds, w') -> (Node s a', next, cmds, w'))

runSystemProxy :: System IO () -> Cache -> World -> IO (Maybe (System IO ()), Cache, [Command IO ()], World)
runSystemProxy s cache w = do
  (result, w', cache', cmds) <- runSystem s w cache
  case result of
    Left a -> return (Just a, cache', cmds, w')
    Right _ -> return (Nothing, cache', cmds, w')

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
                  ((), wAcc', cache', cmdAcc') <- runSystem' a' wAcc cache
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

newtype Scheduler m a = Scheduler (StateT (Map Stage (Schedule m), SystemId) m a)
  deriving (Functor, Applicative, Monad)

data Stage = Startup | Update
  deriving (Eq, Ord)

schedule :: (Monad m) => Stage -> [Constraint] -> System m () -> Scheduler m SystemId
schedule stage cs s = Scheduler $ do
  (m, SystemId i) <- S.get
  let m' = Map.insert stage (Schedule (Map.singleton (SystemId i) (ScheduleNode (Node s mempty) cs))) m
  S.put (m', SystemId (i + 1))
  return (SystemId i)

newtype SchedulerGraph m = SchedulerGraph (Map Stage [[GraphNode m]])

buildScheduler :: (Monad m) => Scheduler m () -> m (SchedulerGraph m)
buildScheduler (Scheduler s) = do
  (_, (m, _)) <- runStateT s (mempty, SystemId 0)
  return $ SchedulerGraph $ fmap build m

runSchedulerGraph :: Stage -> SchedulerGraph IO -> World -> IO (SchedulerGraph IO, World)
runSchedulerGraph stage (SchedulerGraph g) w = case Map.lookup stage g of
  Just s -> do
    (nodes, w') <- runSchedule s w
    let g' = Map.insert stage nodes g
    return (SchedulerGraph g', w')
  Nothing -> return (SchedulerGraph g, w)

runScheduler :: Scheduler IO () -> IO ()
runScheduler s = do
  g <- buildScheduler s
  (g', w) <- runSchedulerGraph Startup g newWorld
  let go gAcc wAcc = do
        (gAcc', wAcc') <- runSchedulerGraph Update gAcc wAcc
        go gAcc' wAcc'
  go g' w
