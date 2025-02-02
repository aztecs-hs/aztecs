module Data.Aztecs
  ( Access,
    runAccess,
    Component (..),
    EntityID,
    Entity,
    (:&) (..),
    System (..),
    Scheduler,
    PreStartup,
    Startup,
    Update,
    run,
    schedule,
    before,
    after,
  )
where

import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (Entity, EntityID, (:&) (..))
import Data.Aztecs.Scheduler
  ( Scheduler,
    PreStartup,
    Startup,
    Update,
    after,
    before,
    run,
    schedule,
  )
import Data.Aztecs.System (System (..))
