{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query.Dynamic
  ( -- * Dynamic queries
    DynamicQuery (..),
    ArrowDynamicQueryReader (..),
    ArrowDynamicQuery (..),

    -- ** Conversion
    fromDynReader,
    toDynReader,

    -- ** Running
    mapDyn,
    mapSingleDyn,
    mapSingleMaybeDyn,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Query.Dynamic.Class
import Aztecs.ECS.Query.Dynamic.Reader
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Data.Either (partitionEithers)
import Data.Foldable
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Prelude hiding ((.))

-- | Dynamic query for components by ID.
newtype DynamicQuery i o
  = DynamicQuery {runDynQuery :: [i] -> [EntityID] -> Archetype -> ([o], Archetype)}
  deriving (Functor)

instance Applicative (DynamicQuery i) where
  {-# INLINE pure #-}
  pure a = DynamicQuery $ \_ es arch -> (replicate (length es) a, arch)
  {-# INLINE (<*>) #-}
  f <*> g = DynamicQuery $ \i es arch ->
    let !(as, arch') = runDynQuery g i es arch
        !(fs, arch'') = runDynQuery f i es arch'
     in (zipWith ($) fs as, arch'')

instance Category DynamicQuery where
  {-# INLINE id #-}
  id = DynamicQuery $ \as _ arch -> (as, arch)
  {-# INLINE (.) #-}
  f . g = DynamicQuery $ \i es arch ->
    let !(as, arch') = runDynQuery g i es arch in runDynQuery f as es arch'

instance Arrow DynamicQuery where
  {-# INLINE arr #-}
  arr f = DynamicQuery $ \bs _ arch -> (fmap f bs, arch)
  {-# INLINE first #-}
  first f = DynamicQuery $ \bds es arch ->
    let !(bs, ds) = unzip bds
        !(cs, arch') = runDynQuery f bs es arch
     in (zip cs ds, arch')

instance ArrowChoice DynamicQuery where
  {-# INLINE left #-}
  left f = DynamicQuery $ \eds es arch ->
    let !(es', ds) = partitionEithers eds
        !(cs, arch') = runDynQuery f es' es arch
     in (fmap Left cs ++ fmap Right ds, arch')

instance ArrowDynamicQueryReader DynamicQuery where
  {-# INLINE entity #-}
  entity = fromDynReader entity
  {-# INLINE fetchDyn #-}
  fetchDyn = fromDynReader . fetchDyn
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn = fromDynReader . fetchMaybeDyn

instance ArrowDynamicQuery DynamicQuery where
  {-# INLINE adjustDyn #-}
  adjustDyn f cId = DynamicQuery $ \is _ arch ->
    let !(as, arch') = A.zipWith is f cId arch in (as, arch')

  {-# INLINE adjustDyn_ #-}
  adjustDyn_ f cId = DynamicQuery $ \is _ arch -> (repeat (), A.zipWith_ is f cId arch)

  {-# INLINE setDyn #-}
  setDyn cId = DynamicQuery $ \is _ arch -> (is, A.insertAscList cId is arch)

{-# INLINE fromDynReader #-}
fromDynReader :: DynamicQueryReader i o -> DynamicQuery i o
fromDynReader q = DynamicQuery $ \is es arch ->
  let !os = runDynQueryReader' q is es arch in (os, arch)

{-# INLINE toDynReader #-}
toDynReader :: DynamicQuery i o -> DynamicQueryReader i o
toDynReader q = DynamicQueryReader $ \is es arch -> fst $ runDynQuery q is es arch

-- | Map all matched entities.
{-# INLINE mapDyn #-}
mapDyn :: Set ComponentID -> i -> DynamicQuery i a -> Entities -> ([a], Entities)
mapDyn cIds i q es =
  let go = runDynQuery q (repeat i)
      (as, es') =
        if Set.null cIds
          then (fst $ go (Map.keys $ entities es) A.empty, es)
          else
            let go' (acc, esAcc) (aId, n) =
                  let !(as', arch') = go (Set.toList . A.entities $ nodeArchetype n) $ nodeArchetype n
                      !nodes = Map.insert aId n {nodeArchetype = arch'} . AS.nodes $ archetypes esAcc
                   in (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
             in foldl' go' ([], es) $ Map.toList . AS.find cIds $ archetypes es
   in (as, es')

mapSingleDyn :: (HasCallStack) => Set ComponentID -> i -> DynamicQuery i a -> Entities -> (a, Entities)
mapSingleDyn cIds i q es = case mapDyn cIds i q es of
  ([a], es') -> (a, es')
  _ -> error "mapSingleDyn: expected single matching entity"

-- | Map a single matched entity.
{-# INLINE mapSingleMaybeDyn #-}
mapSingleMaybeDyn :: Set ComponentID -> i -> DynamicQuery i a -> Entities -> (Maybe a, Entities)
mapSingleMaybeDyn cIds i q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> case runDynQuery q [i] [eId] A.empty of
        ([a], _) -> (Just a, es)
        _ -> (Nothing, es)
      _ -> (Nothing, es)
    else case Map.toList $ AS.find cIds $ archetypes es of
      [(aId, n)] ->
        let !eIds = Set.toList $ A.entities $ AS.nodeArchetype n
         in case runDynQuery q [i] eIds (AS.nodeArchetype n) of
              ([a], arch') ->
                let nodes = Map.insert aId n {nodeArchetype = arch'} . AS.nodes $ archetypes es
                 in (Just a, es {archetypes = (archetypes es) {AS.nodes = nodes}})
              _ -> (Nothing, es)
      _ -> (Nothing, es)
