{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Schedule.Internal where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.HSet (HSet (..), Run)
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.System
import Data.Kind

class Schedule m s where
  type Scheduled m s :: Type

  schedule :: s -> Scheduled m s

type family SystemInOf m sys :: [Type] where
  SystemInOf m sys = AccessType (SystemIn m sys)

type family HasInputOverlap (inputs1 :: [Type]) (inputs2 :: [Type]) :: Bool where
  HasInputOverlap inputs1 inputs2 =
    Or
      (HasComponentOverlap (WriteComponents inputs1) (AccessToComponents inputs2))
      (HasComponentOverlap (WriteComponents inputs2) (AccessToComponents inputs1))

type family HasComponentOverlap (components1 :: [Type]) (components2 :: [Type]) :: Bool where
  HasComponentOverlap '[] components2 = 'False
  HasComponentOverlap (c ': rest) components2 =
    Or (Contains c components2) (HasComponentOverlap rest components2)

type family GroupSystems m (systems :: [Type]) :: [[Type]] where
  GroupSystems m '[] = '[]
  GroupSystems m '[sys] = '[ '[sys]]
  GroupSystems m (sys1 ': sys2 ': rest) =
    If
      (HasInputOverlap (SystemInOf m sys1) (SystemInOf m sys2))
      (GroupSystemsConflict m (sys1 ': sys2 ': rest))
      (GroupSystemsNoConflict m (sys1 ': sys2 ': rest))

type family GroupSystemsConflict m (systems :: [Type]) :: [[Type]] where
  GroupSystemsConflict m (sys ': rest) = '[sys] ': GroupSystems m rest

type family GroupSystemsNoConflict m (systems :: [Type]) :: [[Type]] where
  GroupSystemsNoConflict m (sys1 ': sys2 ': rest) =
    MergeCompatibleSystems m '[sys1, sys2] rest

type family MergeCompatibleSystems m (group :: [Type]) (remaining :: [Type]) :: [[Type]] where
  MergeCompatibleSystems m group '[] = '[group]
  MergeCompatibleSystems m group (sys ': rest) =
    If
      (CanAddSystemToGroup m sys group)
      (MergeCompatibleSystems m (AppendToGroup sys group) rest)
      (group ': GroupSystems m (sys ': rest))

type family CanAddSystemToGroup m (sys :: Type) (group :: [Type]) :: Bool where
  CanAddSystemToGroup m sys '[] = 'True
  CanAddSystemToGroup m sys (groupSys ': rest) =
    And
      (Not (HasInputOverlap (SystemInOf m sys) (SystemInOf m groupSys)))
      (CanAddSystemToGroup m sys rest)

type family AppendToGroup (sys :: Type) (group :: [Type]) :: [Type] where
  AppendToGroup sys group = sys ': group

type family If (condition :: Bool) (then_ :: k) (else_ :: k) :: k where
  If 'True then_ else_ = then_
  If 'False then_ else_ = else_

type family GroupsToNestedHSet m (groups :: [[Type]]) :: [Type] where
  GroupsToNestedHSet m '[] = '[]
  GroupsToNestedHSet m (group ': rest) = HSet (MapToIdentityT m group) ': GroupsToNestedHSet m rest

instance Schedule m (HSet '[]) where
  type Scheduled m (HSet '[]) = HSet '[]
  schedule HEmpty = HEmpty

instance (System m sys) => Schedule m (HSet '[sys]) where
  type Scheduled m (HSet '[sys]) = HSet (GroupsToNestedHSet m (GroupSystems m '[sys]))
  schedule (HCons sys HEmpty) = HCons (HCons sys HEmpty) HEmpty

instance
  ( System m sys,
    AllSystems m rest,
    rest ~ (sys2 ': rest'),
    CompileGroups m (GroupSystems m (sys ': rest)) (sys ': rest)
  ) =>
  Schedule m (HSet (sys ': rest))
  where
  type Scheduled m (HSet (sys ': rest)) = HSet (GroupsToNestedHSet m (GroupSystems m (sys ': rest)))
  schedule = compileGroups @m @(GroupSystems m (sys ': rest)) @(sys ': rest)

class AllSystems m systems

instance AllSystems m '[]

instance (System m (Run constraints sys), AllSystems m rest) => AllSystems m (Run constraints sys ': rest)

instance {-# OVERLAPPABLE #-} (System m sys, AllSystems m rest) => AllSystems m (sys ': rest)

class CompileGroups m (groups :: [[Type]]) (systems :: [Type]) where
  compileGroups :: HSet systems -> HSet (GroupsToNestedHSet m groups)

instance CompileGroups m '[] systems where
  compileGroups _ = HEmpty

instance (CompileGroup m group systems) => CompileGroups m '[group] systems where
  compileGroups systems = HCons (compileGroup @m @group @systems systems) HEmpty

instance
  (CompileGroup m group systems, CompileGroups m rest systems) =>
  CompileGroups m (group ': rest) systems
  where
  compileGroups systems =
    HCons (compileGroup @m @group @systems systems) (compileGroups @m @rest @systems systems)

class CompileGroup m (group :: [Type]) (systems :: [Type]) where
  compileGroup :: HSet systems -> HSet (MapToIdentityT m group)

type family MapToIdentityT m (systems :: [Type]) :: [Type] where
  MapToIdentityT m '[] = '[]
  MapToIdentityT m (sys ': rest) = sys ': MapToIdentityT m rest

instance CompileGroup m '[] systems where
  compileGroup _ = HEmpty

instance (ExtractSystem m sys systems) => CompileGroup m '[sys] systems where
  compileGroup systems = HCons ((extractSystem @m @sys @systems systems)) HEmpty

instance
  (ExtractSystem m sys systems, CompileGroup m rest systems) =>
  CompileGroup m (sys ': rest) systems
  where
  compileGroup systems =
    HCons ((extractSystem @m @sys @systems systems)) (compileGroup @m @rest @systems systems)

class ExtractSystem m (sys :: Type) (systems :: [Type]) where
  extractSystem :: HSet systems -> sys

instance ExtractSystem m sys (sys ': rest) where
  extractSystem (HCons sys _) = sys

instance (ExtractSystem m sys rest) => ExtractSystem m sys (other ': rest) where
  extractSystem (HCons _ rest) = extractSystem @m @sys @rest rest
