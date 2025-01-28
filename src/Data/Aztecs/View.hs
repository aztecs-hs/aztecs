{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.View where

import Data.Aztecs.Entity (ComponentIds, componentIds)
import Data.Aztecs.Query (Query, Queryable (..))
import Data.Aztecs.World (ArchetypeID, World)
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Archetype (Archetype)
import Data.Aztecs.World.Archetypes (Archetypes)
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (Components)
import Data.Map (Map)

data View a = View
  { viewArchetypes :: Map ArchetypeID Archetype,
    viewQuery :: Query a
  }

view :: forall a. (ComponentIds a, Queryable a) => World -> (View a, World)
view w =
  let (v, cs') = view' @a (W.components w) (W.archetypes w)
   in (v, w {W.components = cs'})

view' :: forall a. (ComponentIds a, Queryable a) => Components -> Archetypes -> (View a, Components)
view' cs as =
  let (cIds, cs') = componentIds @a cs
   in ( View
          { viewArchetypes = AS.lookup cIds as,
            viewQuery = query @a
          },
        cs'
      )
