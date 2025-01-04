module Data.Aztecs
  ( Entity,
    EntityComponent (..),
    Component (..),
    World,
    Query,
    Access (..),
    System (..),
    SystemId (..),
    Constraint (..),
    before,
    after,
    Schedule (..),
    Stage (..),
    Scheduler (..),
    schedule,
    runScheduler,
  )
where

import Data.Aztecs.Query (Query (..))
import Data.Aztecs.Schedule
import Data.Aztecs.System
import Data.Aztecs.World
  ( Component (..),
    Entity,
    EntityComponent (..),
    World,
  )
