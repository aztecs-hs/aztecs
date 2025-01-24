{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.Query where

import Data.Aztecs
import Data.Aztecs.Archetype (Archetype)
import qualified Data.Aztecs.Archetype as A
import Data.Aztecs.Entity (ConcatT, Entity (..), EntityT, FromEntity (..), ToEntity (..))
import qualified Data.Aztecs.Entity as E
import Data.Aztecs.World (World (..))
import Data.Data (Typeable)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Query into the `World`.
newtype Query a
  = Query {runQuery' :: Components -> (Set ComponentID, Archetype -> ([Entity a], [Entity a] -> Archetype -> Archetype))}

(<?>) ::
  (E.Split a (ConcatT a b), E.SplitT a (ConcatT a b) ~ b) =>
  Query a ->
  Query b ->
  Query (ConcatT a b)
(Query a) <?> (Query b) = Query $ \cs ->
  let (aIds, a') = a cs
      (bIds, b') = b cs
   in ( aIds <> bIds,
        \arch ->
          let (a'', aF) = a' arch
              (b'', bF) = b' arch
           in ( uncurry E.concat <$> zip a'' b'',
                \new newArch -> let (as, bs) = unzip $ fmap E.split new in bF bs $ aF as newArch
              )
      )

fetch :: forall a. (Component a, Typeable (StorageT a)) => Query '[a]
fetch = Query $ \cs ->
  let cId = fromMaybe (error "TODO") (lookupComponentId @a cs)
   in ( Set.singleton cId,
        \arch ->
          let as = A.all cId arch
           in ( fmap (\x -> ECons (snd x) ENil) as,
                A.insertAscList cId . fmap (\((e, _), ECons a ENil) -> (e, a)) . zip as
              )
      )

all :: Query a -> World -> [Entity a]
all q w =
  let (cIds, g) = runQuery' q (components w)
      res = do
        aId <- Map.lookup cIds (archetypeIds w)
        Map.lookup aId (archetypes w)
   in case res of
        Just arch -> fst $ g arch
        Nothing -> []

map ::
  (FromEntity i, ToEntity o, EntityT i ~ a, EntityT o ~ a) =>
  Query a ->
  (i -> o) ->
  World ->
  ([o], World)
map q f w =
  let (cIds, g) = runQuery' q (components w)
      res = do
        aId <- Map.lookup cIds (archetypeIds w)
        arch <- Map.lookup aId (archetypes w)
        return (aId, arch)
   in case res of
        Just (aId, arch) ->
          let (as, h) = g arch
              as' = fmap (f . fromEntity) as
              arch' = h (fmap toEntity as') arch
           in (as', w {archetypes = Map.insert aId arch' (archetypes w)})
        Nothing -> ([], w)

mapWith ::
  (FromEntity i, ToEntity o, EntityT i ~ ConcatT a b, EntityT o ~ b) =>
  Query a ->
  Query b ->
  (i -> o) ->
  World ->
  ([o], World)
mapWith a b f w =
  let (aCIds, aG) = runQuery' a (components w)
      (bCIds, bG) = runQuery' b (components w)
      res = do
        aId <- Map.lookup (aCIds <> bCIds) (archetypeIds w)
        arch <- Map.lookup aId (archetypes w)
        return (aId, arch)
   in case res of
        Just (aId, arch) ->
          let (as, _) = aG arch
              (bs, bH) = bG arch
              es = fmap (\(aE, bE) -> f $ fromEntity (E.concat aE bE)) (zip as bs)
              arch' = bH (fmap (toEntity . toEntity) es) arch
           in (es, w {archetypes = Map.insert aId arch' (archetypes w)})
        Nothing -> ([], w)
