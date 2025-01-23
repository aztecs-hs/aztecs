module Data.Aztecs where

import Data.Aztecs.Archetype (Archetype)
import Data.Aztecs.Entity (EntityID (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable (TypeRep)

newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

data World = World
  { archetypes :: Map ArchetypeID Archetype,
    archetypeIds :: Map (Set TypeRep) ArchetypeID,
    nextArchetypeId :: ArchetypeID,
    nextEntityId :: EntityID
  }

empty :: World
empty =
  World
    { archetypes = mempty,
      archetypeIds = mempty,
      nextArchetypeId = ArchetypeID 0,
      nextEntityId = EntityID 0
    }
