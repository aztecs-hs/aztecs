-- | Aztecs is a type-safe and friendly ECS for games and more.
--
-- An ECS is a modern approach to organizing your application as a database,
-- providing patterns for data-oriented design and parallel processing.
--
-- The ECS architecture is composed of three main concepts:
--
-- === Entities
-- An entity is an object comprised of zero or more components.
-- In Aztecs, entities are represented by their `EntityID`, a unique identifier.
--
-- === Components
-- A component holds the data for a particular aspect of an entity.
-- For example, a zombie entity might have a @Health@ component and a @Transform@ component.
--
-- === Systems
-- A system is a pipeline that processes entities and their components.
-- Systems in Aztecs either run in sequence or in parallel automatically based on the components they access.
module Data.Aztecs
  ( Access,
    runAccess,
    Bundle,
    bundle,
    Component (..),
    EntityID,
    System (..),
    Query,
    QueryFilter,
    with,
    without,
    runSystem,
    runSystem_,
  )
where

import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (EntityID)
import Data.Aztecs.Query (Query, QueryFilter, with, without)
import Data.Aztecs.System (System (..), runSystem, runSystem_)
import Data.Aztecs.World.Archetype (Bundle, bundle)
