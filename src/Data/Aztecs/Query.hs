{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( Query (..),
    fetch,
    lookup,
  )
where

import Data.Aztecs.Component (Component)
import Data.Aztecs.Core (Entity)
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetypes (Archetype, ArchetypeComponents (ArchetypeComponents), ArchetypeState (..), archetype)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Aztecs.World.Components (ComponentRef (..), Components)
import qualified Data.Aztecs.World.Components as CS
import Data.Dynamic (fromDynamic)
import qualified Data.Map as Map
import Prelude hiding (lookup)

data Query a
  = forall s. Query
      (Components -> (s, Archetype, Components))
      (Entity -> s -> ArchetypeState -> Components -> Maybe a)

fetch :: forall c. (Component c) => Query c
fetch =
  Query
    ( \cs ->
        let (cId, cs') = CS.insertComponentId @c cs
         in (cId, archetype @c cId, cs')
    )
    ( \e cId (ArchetypeState _ archCs _) cs ->
        case Map.lookup e archCs of
          Just (ArchetypeComponents archCs') -> do
            d <- Map.lookup cId archCs'
            (ComponentRef f) <- fromDynamic d
            return $ f cs
          Nothing -> Nothing
    )

lookup :: Entity -> Query a -> World -> (Maybe a, World)
lookup e (Query f g) (World cs as i) =
  let (s, arch, cs') = f cs
      (archId, as') = A.insertArchetype arch cs as
      res = do
        archState <- A.getArchetype archId as'
        g e s archState cs'
   in (res, World cs' as' i)
