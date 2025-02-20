-- | Aztecs is a type-safe and friendly ECS for games and more.
--
-- An ECS is a modern approach to organizing your application state as a database,
-- providing patterns for data-oriented design and parallel processing.
--
-- The ECS architecture is composed of three main concepts:
--
-- === Entities
-- An entity is an object comprised of zero or more components.
-- In Aztecs, entities are represented by their `EntityID`, a unique identifier.
--
-- === Components
-- A `Component` holds the data for a particular aspect of an entity.
-- For example, a zombie entity might have a @Health@ and a @Transform@ component.
--
-- > newtype Position = Position Int deriving (Show)
-- > instance Component Position
-- >
-- > newtype Velocity = Velocity Int deriving (Show)
-- > instance Component Velocity
--
-- === Systems
-- A `System` is a pipeline that processes entities and their components.
-- Systems in Aztecs either run in sequence or in parallel automatically based on the components they access.
--
-- Systems can access game state in two ways:
--
-- ==== Access
-- An `Access` can be queued for full access to the `World`, after a system is complete.
-- `Access` allows for spawning, inserting, and removing components.
--
-- > setup :: System  () ()
-- > setup = S.queue . const . A.spawn_ $ bundle (Position 0) <> bundle (Velocity 1)
--
-- ==== Queries
-- A `Query` can read and write matching components.
--
-- > move :: System  () ()
-- > move =
-- >  S.map
-- >    ( proc () -> do
-- >        Velocity v <- Q.fetch -< ()
-- >        Position p <- Q.fetch -< ()
-- >        Q.set -< Position $ p + v
-- >    )
-- >    >>> S.run print
--
-- Finally, systems can be run on a `World` to produce a result.
--
-- > main :: IO ()
-- > main = runSystem_ $ setup >>> S.forever move
module Aztecs.ECS
  ( Access,
    runAccessT,
    Bundle,
    bundle,
    Component (..),
    EntityID,
    Query,
    ArrowQueryReader,
    ArrowQuery,
    QueryFilter,
    with,
    without,
    System,
    ArrowReaderSystem,
    ArrowSystem,
    ArrowQueueSystem,
    Schedule,
    ArrowReaderSchedule,
    ArrowSchedule,
    ArrowAccessSchedule,
    reader,
    system,
    forever,
    forever_,
    access,
    runSchedule,
    runSchedule_,
    World,
  )
where

import Aztecs.ECS.Access (Access, runAccessT)
import Aztecs.ECS.Component (Component (..))
import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.Query (ArrowQuery, ArrowQueryReader, Query, QueryFilter, with, without)
import Aztecs.ECS.Schedule
  ( ArrowAccessSchedule,
    ArrowReaderSchedule,
    ArrowSchedule,
    Schedule,
    access,
    forever,
    forever_,
    reader,
    runSchedule,
    runSchedule_,
    system,
  )
import Aztecs.ECS.System (ArrowQueueSystem, ArrowReaderSystem, ArrowSystem, System)
import Aztecs.ECS.World (World)
import Aztecs.ECS.World.Archetype (Bundle, bundle)
