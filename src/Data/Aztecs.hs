module Data.Aztecs
  ( Access,
    runAccess,
    Component (..),
    EntityID,
    Entity,
    (:&) (..),
    System (..),
    Scheduler,
    Startup,
    Update,
    run,
    schedule,
  )
where

import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (Entity, EntityID, (:&) (..))
import Data.Aztecs.Scheduler (Scheduler, Startup, Update, run, schedule)
import Data.Aztecs.System (System (..))
