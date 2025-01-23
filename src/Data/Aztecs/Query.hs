{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query where

import Data.Aztecs
import Data.Aztecs.Archetype (Archetype)
import qualified Data.Aztecs.Archetype as A
import Data.Aztecs.World (World (..))
import Data.Data (Typeable)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Query into the `World`.
newtype Query a
  = Query {runQuery' :: Components -> (Set ComponentID, Archetype -> ([a], [a] -> Archetype -> Archetype))}

(<&>) :: Query a -> Query b -> Query (a, b)
Query a <&> Query b = Query $ \cs ->
  let (aIds, a') = a cs
      (bIds, b') = b cs
   in ( aIds <> bIds,
        \arch ->
          let (a'', aF) = a' arch
              (b'', bF) = b' arch
           in (zip a'' b'', \new newArch -> let (as, bs) = unzip new in bF bs $ aF as newArch)
      )

fetch :: forall a. (Component a, Typeable (StorageT a)) => Query a
fetch = Query $ \cs ->
  let cId = fromMaybe (error "TODO") (lookupComponentId @a cs)
   in ( Set.singleton cId,
        \arch ->
          let as = A.all cId arch
           in (fmap snd as, A.insertAscList cId . fmap (\((e, _), a) -> (e, a)) . zip as)
      )

all :: Query a -> World -> [a]
all q w =
  let (cIds, g) = runQuery' q (components w)
      res = do
        aId <- Map.lookup cIds (archetypeIds w)
        Map.lookup aId (archetypes w)
   in case res of
        Just arch -> fst $ g arch
        Nothing -> []

map :: Query a -> (a -> a) -> World -> ([a], World)
map q f w =
  let (cIds, g) = runQuery' q (components w)
      res = do
        aId <- Map.lookup cIds (archetypeIds w)
        arch <- Map.lookup aId (archetypes w)
        return (aId, arch)
   in case res of
        Just (aId, arch) ->
          let (as, h) = g arch
              as' = fmap f as
              arch' = h as' arch
           in (as', w {archetypes = Map.insert aId arch' (archetypes w)})
        Nothing -> ([], w)
