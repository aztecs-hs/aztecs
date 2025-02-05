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
