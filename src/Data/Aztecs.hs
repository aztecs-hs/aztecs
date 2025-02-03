module Data.Aztecs
  ( Access,
    runAccess,
    Component (..),
    EntityID,
    Entity,
    (:&) (..),
    System (..),
    QueryFilter,
    with,
    without,
    runSystem,
    runSystem_
  )
where

import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (Entity, EntityID, (:&) (..))
import Data.Aztecs.Query (QueryFilter, with, without)
import Data.Aztecs.System (System (..), runSystem, runSystem_)
