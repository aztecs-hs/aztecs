{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Aztecs.ECS.Scheduler (Scheduler (..), runSchedule) where

import Aztecs.ECS.Executor
import Aztecs.ECS.HSet
import Aztecs.ECS.Schedule.Internal
import Aztecs.ECS.Scheduler.Internal
import Data.Foldable

executeSchedule ::
  forall m cs s.
  ( Scheduler m s,
    Execute m (SchedulerOutput m s),
    s ~ HSet (SchedulerInput m s)
  ) =>
  s ->
  ExecutorT m ()
executeSchedule s = execute (buildSchedule @m @s s)

runSchedule ::
  forall m cs s.
  ( Applicative m,
    Execute m (SchedulerOutput m s),
    s ~ HSet (SchedulerInput m s),
    AllSystems m (SchedulerInput m s),
    ScheduleLevelsBuilder
      m
      (TopologicalSort (BuildSystemGraph (SchedulerInput m s)))
      (SchedulerInput m s)
  ) =>
  s ->
  m ()
runSchedule s = runSystems (executeSchedule @m @cs @s s) $
  \actions -> sequenceA_ actions
