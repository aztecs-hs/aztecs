module Data.Aztecs
  ( Access,
    runAccess,
    Component (..),
    EntityID,
    Entity,
    (:&) (..),
    System (..),
    (<&>),
  )
where

import Data.Aztecs.Access (Access, runAccess)
import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Entity (Entity, EntityID, (:&) (..))
import Data.Aztecs.System (System (..), (<&>))
