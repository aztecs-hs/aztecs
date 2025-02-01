{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Scheduler where

import Data.Aztecs.Access (Access)
import Data.Aztecs.System (System (..), Task (runTask))
import Data.Aztecs.World (World)
import Data.Aztecs.World.Components (ComponentID, Components)
import Data.Data (Proxy (..), TypeRep, Typeable, typeOf)
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

data Constraint = Constraint {constraintKind :: ConstraintKind, constraintType :: TypeRep}
  deriving (Eq, Show)

data ConstraintKind = Before | After deriving (Eq, Show)

before :: forall m a. (System m a) => Constraint
before = Constraint Before (typeOf (Proxy @a))

after :: forall m a. (System m a) => Constraint
after = Constraint After (typeOf (Proxy @a))

data Startup

data Update

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

data ScheduleNode m = ScheduleNode
  { scheduleNodeIds :: [Set ComponentID],
    scheduleNodeBefore :: Set TypeRep,
    runScheduleNode :: Components -> m (World -> World, Access m ())
  }

instance Show (ScheduleNode m) where
  show n =
    "Node "
      ++ "{ scheduleNodeIds = "
      ++ show (scheduleNodeIds n)
      ++ ", scheduleNodeBefore = "
      ++ show (scheduleNodeBefore n)
      ++ " }"

newtype ScheduleStage m = ScheduleStage {unScheduleStage :: Map TypeRep (ScheduleNode m)}
  deriving (Show)

buildStage :: (Functor m) => ScheduleBuilderStage m -> World -> (ScheduleStage m, World)
buildStage s w =
  let go (nodeId, node) (acc, wAcc) =
        let (wAcc', cIds, f) = runTask (scheduleBuilderNodeTask node) wAcc
         in ( Map.insert
                nodeId
                ScheduleNode
                  { scheduleNodeIds = cIds,
                    scheduleNodeBefore = scheduleBuilderNodeBefore node,
                    runScheduleNode = fmap (\(_, f', a) -> (f', a)) . f ()
                  }
                acc,
              wAcc'
            )
      (nodes, w') = foldr go (Map.empty, w) (Map.toList $ unScheduleBuilderStage s)
   in (ScheduleStage nodes, w')

newtype Scheduler m = Scheduler {unScheduler :: Map TypeRep (Stage m)}
  deriving (Show, Monoid)

instance Semigroup (Scheduler m) where
  s1 <> s2 = Scheduler (Map.unionWith (<>) (unScheduler s1) (unScheduler s2))

schedule :: forall m l a. (Typeable l, System m a) => [Constraint] -> Scheduler m
schedule cs =
  let (bs, as) =
        foldr
          ( \c (bAcc, aAcc) -> case constraintKind c of
              Before -> (constraintType c : bAcc, aAcc)
              After -> (bAcc, constraintType c : aAcc)
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

newtype ScheduleBuilder m = ScheduleBuilder {unScheduleBuilder :: Map TypeRep (ScheduleBuilderStage m)}
  deriving (Show)

builder :: Scheduler m -> (ScheduleBuilder m, [Error])
builder (Scheduler s) =
  let (stages, errors) = unzip $ fmap stageBuilder (Map.elems s)
   in (ScheduleBuilder $ Map.fromList $ zip (Map.keys s) stages, concat errors)

newtype Schedule m = Schedule {unSchedule :: Map TypeRep (ScheduleStage m)}
  deriving (Show)

build :: (Functor m) => Scheduler m -> World -> (Schedule m, World, [Error])
build s w =
  let (sb, errors) = builder s
      (s', w') = build' sb w
   in (s', w', errors)

build' :: (Functor m) => ScheduleBuilder m -> World -> (Schedule m, World)
build' (ScheduleBuilder s) w =
  let go (stageId, stage) (acc, wAcc) =
        let (stage', wAcc') = buildStage stage wAcc
         in (Map.insert stageId stage' acc, wAcc')
      (stages, w') = foldr go (Map.empty, w) (Map.toList s)
   in (Schedule stages, w')
