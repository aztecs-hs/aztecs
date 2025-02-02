{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Scheduler where

import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.System (System (..), Task (runTask))
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Components (ComponentID)
import Data.Data (Proxy (..), TypeRep, Typeable, typeOf)
import Data.Foldable (foldrM)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Node m = Node
  { nodeTask :: Task m () (),
    nodeBefore :: Set TypeRep,
    nodeAfter :: Set TypeRep
  }

instance Show (Node m) where
  show n =
    "Node "
      ++ "{ nodeBefore = "
      ++ show (nodeBefore n)
      ++ ", nodeAfter = "
      ++ show (nodeAfter n)
      ++ " }"

data Constraint = Constraint {constraintKind :: ConstraintKind, constraintId :: TypeRep}
  deriving (Eq, Show)

data ConstraintKind = Before | After deriving (Eq, Show)

before :: forall m a. (System m a) => Constraint
before = Constraint Before (typeOf (Proxy @a))

after :: forall m a. (System m a) => Constraint
after = Constraint After (typeOf (Proxy @a))

newtype Scheduler m = Scheduler {unScheduler :: Map TypeRep (Stage m)}
  deriving (Show, Monoid)

instance Semigroup (Scheduler m) where
  s1 <> s2 = Scheduler (Map.unionWith (<>) (unScheduler s1) (unScheduler s2))

schedule :: forall m l a. (Typeable l, System m a) => [Constraint] -> Scheduler m
schedule cs =
  let (bs, as) =
        foldr
          ( \c (bAcc, aAcc) -> case constraintKind c of
              Before -> (constraintId c : bAcc, aAcc)
              After -> (bAcc, constraintId c : aAcc)
          )
          ([], [])
          cs
   in Scheduler
        ( Map.singleton
            (typeOf (Proxy @l))
            ( Stage
                ( Map.singleton
                    (typeOf (Proxy @a))
                    ( Node
                        { nodeTask = task @m @a,
                          nodeBefore = Set.fromList bs,
                          nodeAfter = Set.fromList as
                        }
                    )
                )
            )
        )

newtype Stage m = Stage {unStage :: Map TypeRep (Node m)}
  deriving (Show, Semigroup, Monoid)

data ScheduleBuilderNode m = ScheduleBuilderNode
  { scheduleBuilderNodeTask :: Task m () (),
    scheduleBuilderNodeBefore :: Set TypeRep
  }

instance Show (ScheduleBuilderNode m) where
  show n = "Node " ++ "{ scheduleBuilderNodeBefore = " ++ show (scheduleBuilderNodeBefore n) ++ " }"

newtype ScheduleBuilderStage m = ScheduleBuilderStage {unScheduleBuilderStage :: Map TypeRep (ScheduleBuilderNode m)}
  deriving (Show)

data Error = MissingError TypeRep | AmbiguousError TypeRep
  deriving (Show)

stageBuilder :: Stage m -> (ScheduleBuilderStage m, [Error])
stageBuilder s =
  let go' nodeId (acc, errorAcc) = case Map.lookup nodeId acc of
        Just n ->
          let acc' = Map.insert nodeId n {nodeBefore = Set.insert nodeId (nodeBefore n)} acc
              errorAcc' =
                if Set.member nodeId (nodeBefore n)
                  then AmbiguousError nodeId : errorAcc
                  else errorAcc
           in (acc', errorAcc')
        Nothing -> (acc, MissingError nodeId : errorAcc)
      go node (acc, errorAcc) = foldr go' (acc, errorAcc) (nodeAfter node)
      (nodes, errors) = foldr go (unStage s, []) (unStage s)
   in ( ScheduleBuilderStage $
          fmap
            ( \n ->
                ScheduleBuilderNode
                  { scheduleBuilderNodeTask = nodeTask n,
                    scheduleBuilderNodeBefore = nodeBefore n
                  }
            )
            nodes,
        errors
      )

data ScheduleGraphNode m = ScheduleGraphNode
  { scheduleGraphNodeIds :: [Set ComponentID],
    scheduleGraphNodeBefore :: Set TypeRep,
    runScheduleGraphNode :: World -> m (World -> World, Access m ())
  }

instance Show (ScheduleGraphNode m) where
  show n =
    "Node "
      ++ "{ scheduleGraphNodeIds = "
      ++ show (scheduleGraphNodeIds n)
      ++ ", scheduleGraphNodeBefore = "
      ++ show (scheduleGraphNodeBefore n)
      ++ " }"

newtype ScheduleGraphStage m = ScheduleGraphStage {unScheduleGraphStage :: Map TypeRep (ScheduleGraphNode m)}
  deriving (Show)

buildGraphStage :: (Functor m) => ScheduleBuilderStage m -> World -> (ScheduleGraphStage m, World)
buildGraphStage s w =
  let go (nodeId, node) (acc, wAcc) =
        let (cs, cIds, f) = runTask (scheduleBuilderNodeTask node) (components wAcc)
         in ( Map.insert
                nodeId
                ScheduleGraphNode
                  { scheduleGraphNodeIds = cIds,
                    scheduleGraphNodeBefore = scheduleBuilderNodeBefore node,
                    runScheduleGraphNode = fmap (\(_, f', a) -> (f', a)) . f ()
                  }
                acc,
              wAcc {components = cs}
            )
      (nodes, w') = foldr go (Map.empty, w) (Map.toList $ unScheduleBuilderStage s)
   in (initGraphStage $ ScheduleGraphStage nodes, w')

initGraphStage :: ScheduleGraphStage m -> ScheduleGraphStage m
initGraphStage s =
  let go (nodeId, node) nodeAcc =
        let go' nodeId' (beforeAcc, nodeAcc') = case Map.lookup nodeId' nodeAcc of
              Just node' -> (scheduleGraphNodeBefore node' <> beforeAcc, nodeAcc')
              Nothing -> foldr go' (beforeAcc, nodeAcc') (Set.toList $ scheduleGraphNodeBefore (unScheduleGraphStage s Map.! nodeId))
            (befores, nodeAcc'') = foldr go' (Set.empty, nodeAcc) (Set.toList $ scheduleGraphNodeBefore node)
         in Map.insert nodeId node {scheduleGraphNodeBefore = befores} nodeAcc''
   in ScheduleGraphStage $ foldr go Map.empty (Map.toList $ unScheduleGraphStage s)

buildStage :: ScheduleGraphStage m -> [ScheduleNode m]
buildStage s =
  map (\n -> ScheduleNode {runScheduleNode = runScheduleGraphNode n})
    . sortBy (\a b -> compare (length $ scheduleGraphNodeIds a) (length $ scheduleGraphNodeIds b))
    $ Map.elems (unScheduleGraphStage s)

newtype ScheduleBuilder m = ScheduleBuilder {unScheduleBuilder :: Map TypeRep (ScheduleBuilderStage m)}
  deriving (Show)

builder :: Scheduler m -> (ScheduleBuilder m, [Error])
builder (Scheduler s) =
  let (stages, errors) = unzip $ fmap stageBuilder (Map.elems s)
   in (ScheduleBuilder $ Map.fromList $ zip (Map.keys s) stages, concat errors)

newtype ScheduleGraph m = ScheduleGraph {unScheduleGraph :: Map TypeRep (ScheduleGraphStage m)}
  deriving (Show)

buildGraph :: (Functor m) => Scheduler m -> World -> (ScheduleGraph m, World, [Error])
buildGraph s w =
  let (sb, errors) = builder s
      (s', w') = buildGraph' sb w
   in (s', w', errors)

buildGraph' :: (Functor m) => ScheduleBuilder m -> World -> (ScheduleGraph m, World)
buildGraph' (ScheduleBuilder s) w =
  let go (stageId, stage) (acc, wAcc) =
        let (stage', wAcc') = buildGraphStage stage wAcc
         in (Map.insert stageId stage' acc, wAcc')
      (stages, w') = foldr go (Map.empty, w) (Map.toList s)
   in (ScheduleGraph stages, w')

newtype ScheduleNode m = ScheduleNode {runScheduleNode :: World -> m (World -> World, Access m ())}

newtype Schedule m = Schedule {unSchedule :: Map TypeRep [ScheduleNode m]}

build :: (Functor m) => Scheduler m -> World -> (Schedule m, World, [Error])
build s w = let (s', w', errors) = buildGraph s w in (build' s', w', errors)

build' :: (Functor m) => ScheduleGraph m -> Schedule m
build' g = Schedule $ fmap buildStage (unScheduleGraph g)

runStage :: forall l m. (Typeable l, Monad m) => Schedule m -> World -> m World
runStage s w = case Map.lookup (typeOf (Proxy @l)) (unSchedule s) of
  Just stage ->
    let go node wAcc = do
          (f, access) <- runScheduleNode node wAcc
          (_, wAcc') <- runAccess access (f wAcc)
          return wAcc'
     in foldrM go w stage
  Nothing -> return w

data PreStartup

data Startup

data Update

runWorld :: (Monad m) => Scheduler m -> World -> m ()
runWorld s w = do
  let (s', w', _) = build s w
  w'' <- runStage @PreStartup s' w'
  w''' <- runStage @Startup s' w''
  let go wAcc = do
        wAcc' <- runStage @Update s' wAcc
        go wAcc'
  go w'''

run :: (Monad m) => Scheduler m -> m ()
run s = runWorld s W.empty
