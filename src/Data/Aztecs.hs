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
    QueryResult (..),
    Write,
  )
import Data.Aztecs.Schedule
import Data.Aztecs.System
import Data.Aztecs.World
  ( Component (..),
    Entity,
    EntityComponent (..),
    World,
  )
import Prelude hiding (all, read)
