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
module Aztecs.ECS
  ( module Aztecs.ECS.System,
    module Aztecs.ECS.Query,
    Access,
    AccessT,
    runAccessT,
    runAccessT_,
    Bundle,
    bundle,
    fromDynBundle,
    DynamicBundle,
    dynBundle,
    Component (..),
    EntityID,
    spawn,
    system,
    World,
  )
where

import Aztecs.ECS.Access
import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Query hiding
  ( query,
    querySingle,
    querySingleMaybe,
    readQuery,
    readQueryEntities,
  )
import Aztecs.ECS.System
import Aztecs.ECS.World (World)
import Aztecs.ECS.World.Bundle
import Aztecs.ECS.World.Bundle.Dynamic
