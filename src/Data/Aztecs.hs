module Data.Aztecs
  ( Entity,
    EntityComponent (..),
    Component (..),
    World,
    QueryBuilder,
    Query,
    Access (..),
    Task,
    System (..),
    Constraint (..),
    before,
    after,
    Schedule (..),
    Startup,
    Update,
    Scheduler (..),
    schedule,
    runScheduler,
    OnSpawn,
    OnInsert,
  )
where

import Data.Aztecs.Query
  ( Query (..),
    QueryBuilder,
  )
import Data.Aztecs.Schedule
import Data.Aztecs.System
import Data.Aztecs.Task (Task (..))
import Data.Aztecs.World
  ( Component (..),
    Entity,
    EntityComponent (..),
    World,
  )
