module Data.Aztecs
  ( Access,
    runAccess,
    Component (..),
    EntityID,
    Entity,
    (:&) (..),
  )
where

import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Core (Component (..), EntityID)
import Data.Aztecs.Entity (Entity, (:&) (..))
