{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Scheduler.Internal where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.Class
import Aztecs.ECS.Executor
import Aztecs.ECS.HSet
import Aztecs.ECS.Schedule.Internal
import Data.Kind

class Scheduler m s where
  type SchedulerInput m s :: [Type]
  type SchedulerOutput m s :: Type

  buildSchedule :: HSet (SchedulerInput m s) -> SchedulerOutput m s

instance (Applicative m, ECS m) => Access m s (HSet '[]) where
  type AccessType (HSet '[]) = '[]
  access = pure HEmpty
  {-# INLINE access #-}

instance
  ( AllSystems m systems,
    BuildSystemGraph systems ~ graph,
    TopologicalSort graph ~ levels,
    ScheduleLevels m levels ~ output,
    ScheduleLevelsBuilder m levels systems
  ) =>
  Scheduler m (HSet systems)
  where
  type SchedulerInput m (HSet systems) = systems
  type
    SchedulerOutput m (HSet systems) =
      HSet (LevelsToNestedHSet (ScheduleLevels m (TopologicalSort (BuildSystemGraph systems))))

  buildSchedule = scheduleSystemLevels @m @(TopologicalSort (BuildSystemGraph systems))
  {-# INLINE buildSchedule #-}

type family BuildSystemGraph (systems :: [Type]) :: DependencyGraph where
  BuildSystemGraph '[] = EmptyGraph
  BuildSystemGraph (runSys ': rest) =
    AddSystemToGraph
      (UnwrapSystem runSys)
      (GetConstraints runSys)
      (BuildSystemGraph rest)

data ConstrainedSystem = ConstrainedSystem Type [Type]

type family BuildDependencyGraph (constrainedSystems :: [ConstrainedSystem]) :: DependencyGraph where
  BuildDependencyGraph '[] = EmptyGraph
  BuildDependencyGraph ('ConstrainedSystem sys constraints ': rest) =
    AddSystemToGraph sys constraints (BuildDependencyGraph rest)

type family AddSystemToGraph (sys :: Type) (constraints :: [Type]) (graph :: DependencyGraph) :: DependencyGraph where
  AddSystemToGraph sys '[] graph = AddNode sys graph
  AddSystemToGraph sys (Before target ': rest) graph =
    AddSystemToGraph sys rest (AddEdge sys (UnwrapSystem target) graph)
  AddSystemToGraph sys (After source ': rest) graph =
    AddSystemToGraph sys rest (AddEdge (UnwrapSystem source) sys graph)
  AddSystemToGraph sys (other ': rest) graph =
    AddSystemToGraph sys rest graph

data DependencyGraph = EmptyGraph | Graph [Type] [(Type, Type)] [Type]

type family AddNode (sys :: Type) (graph :: DependencyGraph) :: DependencyGraph where
  AddNode sys EmptyGraph = Graph '[sys] '[] '[]
  AddNode sys (Graph nodes edges groups) = Graph (AddToList sys nodes) edges groups

type family AddEdge (from :: Type) (to :: Type) (graph :: DependencyGraph) :: DependencyGraph where
  AddEdge from to EmptyGraph = Graph '[from, to] '[ '(from, to)] '[]
  AddEdge from to (Graph nodes edges groups) =
    Graph (AddToList to (AddToList from nodes)) (AddToList '(from, to) edges) groups

type family AddGroupConstraint (sys :: Type) (graph :: DependencyGraph) :: DependencyGraph where
  AddGroupConstraint sys EmptyGraph = Graph '[sys] '[] '[sys]
  AddGroupConstraint sys (Graph nodes edges groups) =
    Graph (AddToList sys nodes) edges (AddToList sys groups)

type family AddToList (item :: k) (list :: [k]) :: [k] where
  AddToList item '[] = '[item]
  AddToList item (item ': rest) = item ': rest
  AddToList item (other ': rest) = other ': AddToList item rest

type family TopologicalSort (graph :: DependencyGraph) :: [[Type]] where
  TopologicalSort EmptyGraph = '[]
  TopologicalSort (Graph nodes edges groups) = TopSortHelper nodes edges '[]

type family TopSortHelper (nodes :: [Type]) (edges :: [(Type, Type)]) (result :: [[Type]]) :: [[Type]] where
  TopSortHelper '[] edges result = Reverse result
  TopSortHelper nodes edges result =
    TopSortHelper
      (RemoveNodes (NoIncomingEdges nodes edges) nodes)
      (RemoveEdgesFrom (NoIncomingEdges nodes edges) edges)
      (NoIncomingEdges nodes edges ': result)

type family NoIncomingEdges (nodes :: [Type]) (edges :: [(Type, Type)]) :: [Type] where
  NoIncomingEdges '[] edges = '[]
  NoIncomingEdges (node ': rest) edges =
    If
      (HasIncomingEdge node edges)
      (NoIncomingEdges rest edges)
      (node ': NoIncomingEdges rest edges)

type family HasIncomingEdge (node :: Type) (edges :: [(Type, Type)]) :: Bool where
  HasIncomingEdge node '[] = 'False
  HasIncomingEdge node ('(from, to) ': rest) =
    If (TypeEq node to) 'True (HasIncomingEdge node rest)

type family RemoveNodes (toRemove :: [Type]) (nodes :: [Type]) :: [Type] where
  RemoveNodes '[] nodes = nodes
  RemoveNodes (remove ': rest) nodes = RemoveNodes rest (FilterOut remove nodes)

type family FilterOut (item :: Type) (list :: [Type]) :: [Type] where
  FilterOut item '[] = '[]
  FilterOut item (item ': rest) = FilterOut item rest
  FilterOut item (other ': rest) = other ': FilterOut item rest

type family RemoveEdgesFrom (removed :: [Type]) (edges :: [(Type, Type)]) :: [(Type, Type)] where
  RemoveEdgesFrom '[] edges = edges
  RemoveEdgesFrom (node ': rest) edges = RemoveEdgesFrom rest (FilterOutEdgesFrom node edges)

type family FilterOutEdgesFrom (node :: Type) (edges :: [(Type, Type)]) :: [(Type, Type)] where
  FilterOutEdgesFrom node '[] = '[]
  FilterOutEdgesFrom node ('(from, to) ': rest) =
    If
      (TypeEq node from)
      (FilterOutEdgesFrom node rest)
      ('(from, to) ': FilterOutEdgesFrom node rest)

type family TypeEq (a :: Type) (b :: Type) :: Bool where
  TypeEq a a = 'True
  TypeEq a b = 'False

type family Reverse (list :: [k]) :: [k] where
  Reverse list = ReverseHelper list '[]

type family ReverseHelper (list :: [k]) (acc :: [k]) :: [k] where
  ReverseHelper '[] acc = acc
  ReverseHelper (x ': xs) acc = ReverseHelper xs (x ': acc)

type family ScheduleLevels (m :: Type -> Type) (levels :: [[Type]]) :: [[Type]] where
  ScheduleLevels m '[] = '[]
  ScheduleLevels m (level ': rest) =
    GroupByConflicts m level ': ScheduleLevels m rest

type family GroupByConflicts (m :: Type -> Type) (systems :: [Type]) :: [Type] where
  GroupByConflicts m '[] = '[]
  GroupByConflicts m '[sys] = '[sys]
  GroupByConflicts m systems = systems

scheduleSystemLevels ::
  forall m levels systems.
  ( AllSystems m systems,
    ScheduleLevelsBuilder m levels systems
  ) =>
  HSet systems ->
  HSet (LevelsToNestedHSet (ScheduleLevels m levels))
scheduleSystemLevels = buildScheduleLevels @m @levels @systems
{-# INLINE scheduleSystemLevels #-}

type family LevelsToNestedHSet (levels :: [[Type]]) :: [Type] where
  LevelsToNestedHSet '[] = '[]
  LevelsToNestedHSet (level ': rest) = HSet level ': LevelsToNestedHSet rest

class ScheduleLevelsBuilder (m :: Type -> Type) (levels :: [[Type]]) (systems :: [Type]) where
  buildScheduleLevels ::
    HSet systems ->
    HSet (LevelsToNestedHSet (ScheduleLevels m levels))

instance ScheduleLevelsBuilder m '[] systems where
  buildScheduleLevels _ = HEmpty
  {-# INLINE buildScheduleLevels #-}

instance
  ( GroupByConflicts m systems ~ systems
  ) =>
  ScheduleLevelsBuilder m '[systems] systems
  where
  buildScheduleLevels systems = HCons systems HEmpty
  {-# INLINE buildScheduleLevels #-}

instance
  ( SystemReorderer originalSystems levelSystems,
    GroupByConflicts m levelSystems ~ levelSystems
  ) =>
  ScheduleLevelsBuilder m '[levelSystems] originalSystems
  where
  buildScheduleLevels originalSystems =
    HCons (reorderSystems @originalSystems @levelSystems originalSystems) HEmpty
  {-# INLINE buildScheduleLevels #-}

instance
  ( SystemReorderer originalSystems levelSystems1,
    SystemReorderer originalSystems levelSystems2,
    GroupByConflicts m levelSystems1 ~ levelSystems1,
    GroupByConflicts m levelSystems2 ~ levelSystems2
  ) =>
  ScheduleLevelsBuilder m '[levelSystems1, levelSystems2] originalSystems
  where
  buildScheduleLevels originalSystems =
    HCons (reorderSystems @originalSystems @levelSystems1 originalSystems) $
      HCons
        (reorderSystems @originalSystems @levelSystems2 originalSystems)
        HEmpty
  {-# INLINE buildScheduleLevels #-}

instance
  {-# OVERLAPPABLE #-}
  ( SystemReorderer originalSystems levelSystems,
    GroupByConflicts m levelSystems ~ levelSystems,
    ScheduleLevelsBuilder m restLevels originalSystems
  ) =>
  ScheduleLevelsBuilder m (levelSystems ': restLevels) originalSystems
  where
  buildScheduleLevels originalSystems =
    HCons (reorderSystems @originalSystems @levelSystems originalSystems) $
      buildScheduleLevels @m @restLevels @originalSystems originalSystems
  {-# INLINE buildScheduleLevels #-}

class SystemReorderer (originalSystems :: [Type]) (targetSystems :: [Type]) where
  reorderSystems ::
    HSet originalSystems ->
    HSet targetSystems

instance SystemReorderer originalSystems '[] where
  reorderSystems _ = HEmpty
  {-# INLINE reorderSystems #-}

instance
  ( ExtractFromHSet targetSys originalSystems,
    SystemReorderer (RemainingAfterExtract targetSys originalSystems) restTargets
  ) =>
  SystemReorderer originalSystems (targetSys ': restTargets)
  where
  reorderSystems originalSystems =
    let (targetSys, remaining) = extractFromHSet @targetSys @originalSystems originalSystems
        rest = reorderSystems @(RemainingAfterExtract targetSys originalSystems) @restTargets remaining
     in HCons targetSys rest
  {-# INLINE reorderSystems #-}

type family RemainingAfterExtract (targetSys :: Type) (systems :: [Type]) :: [Type] where
  RemainingAfterExtract sys (sys ': rest) = rest
  RemainingAfterExtract sys (Run constraints sys ': rest) = rest
  RemainingAfterExtract targetSys (other ': rest) = other ': RemainingAfterExtract targetSys rest

class ExtractFromHSet (targetSys :: Type) (systems :: [Type]) where
  extractFromHSet ::
    HSet systems ->
    (targetSys, HSet (RemainingAfterExtract targetSys systems))

instance {-# OVERLAPPING #-} ExtractFromHSet sys (sys ': rest) where
  extractFromHSet (HCons sys rest) = (sys, rest)
  {-# INLINE extractFromHSet #-}

instance
  {-# OVERLAPPING #-}
  (RemainingAfterExtract sys (Run constraints sys ': rest) ~ rest) =>
  ExtractFromHSet sys (Run constraints sys ': rest)
  where
  extractFromHSet (HCons (Run sys) rest) = (sys, rest)
  {-# INLINE extractFromHSet #-}

instance
  ( ExtractFromHSet targetSys rest,
    RemainingAfterExtract targetSys (other ': rest) ~ (other ': RemainingAfterExtract targetSys rest),
    TypeEq targetSys other ~ 'False
  ) =>
  ExtractFromHSet targetSys (other ': rest)
  where
  extractFromHSet (HCons other rest) =
    let (target, remaining) = extractFromHSet @targetSys @rest rest
     in (target, HCons other remaining)
  {-# INLINE extractFromHSet #-}

instance
  {-# OVERLAPPING #-}
  ( Monad m,
    Execute' m (HSet level),
    Execute m (HSet restLevels)
  ) =>
  Execute m (HSet (HSet level ': restLevels))
  where
  execute (HCons level restLevels) = do
    ExecutorT $ \run -> run $ execute' level
    execute restLevels
  {-# INLINE execute #-}
