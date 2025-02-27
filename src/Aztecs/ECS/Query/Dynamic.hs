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
    let !(as, arch') = A.zipMap is f cId arch in (as, arch')

  {-# INLINE setDyn #-}
  setDyn cId = DynamicQuery $ \is _ arch ->
    let !arch' = A.insertAscList cId is arch in (is, arch')

{-# INLINE fromDynReader #-}
fromDynReader :: DynamicQueryReader i o -> DynamicQuery i o
fromDynReader q = DynamicQuery $ \is es arch ->
  let !os = runDynQueryReader' q is es arch in (os, arch)

{-# INLINE toDynReader #-}
toDynReader :: DynamicQuery i o -> DynamicQueryReader i o
toDynReader q = DynamicQueryReader $ \is es arch -> fst $ runDynQuery q is es arch

-- | Map all matched entities.
mapDyn :: Set ComponentID -> i -> DynamicQuery i a -> Entities -> ([a], Entities)
mapDyn cIds i q es =
  let go = runDynQuery q (repeat i)
      (as, es') =
        if Set.null cIds
          then (fst $ go (Map.keys $ entities es) A.empty, es)
          else
            foldl'
              ( \(acc, esAcc) (aId, n) ->
                  let !(as', arch') = go (Set.toList . A.entities $ nodeArchetype n) (nodeArchetype n)
                      !nodes = Map.insert aId n {nodeArchetype = arch'} (AS.nodes $ archetypes esAcc)
                   in (as' ++ acc, esAcc {archetypes = (archetypes esAcc) {AS.nodes = nodes}})
              )
              ([], es)
              (Map.toList . AS.find cIds $ archetypes es)
   in (as, es')
