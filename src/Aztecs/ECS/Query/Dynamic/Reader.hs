{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query.Dynamic.Reader
  ( -- * Dynamic queries
    DynamicQueryReader (..),
    ArrowDynamicQueryReader (..),

    -- ** Running
    allDyn,
    runDynQueryReader,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Aztecs.ECS.World.Storage as S
import Control.Arrow
import Control.Category
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Dynamic query for components by ID.
newtype DynamicQueryReader i o
  = DynamicQueryReader {runDynQueryReader' :: [i] -> [EntityID] -> Archetype -> [o]}
  deriving (Functor)

instance Applicative (DynamicQueryReader i) where
  pure a = DynamicQueryReader $ \_ es _ -> replicate (length es) a

  f <*> g =
    DynamicQueryReader $ \i es arch ->
      let as = runDynQueryReader' g i es arch
          fs = runDynQueryReader' f i es arch
       in zipWith ($) fs as

instance Category DynamicQueryReader where
  id = DynamicQueryReader $ \as _ _ -> as
  f . g = DynamicQueryReader $ \i es arch ->
    let as = runDynQueryReader' g i es arch in runDynQueryReader' f as es arch

instance Arrow DynamicQueryReader where
  arr f = DynamicQueryReader $ \bs _ _ -> fmap f bs
  first f = DynamicQueryReader $ \bds es arch ->
    let (bs, ds) = unzip bds
        cs = runDynQueryReader' f bs es arch
     in zip cs ds

instance ArrowChoice DynamicQueryReader where
  left f = DynamicQueryReader $ \eds es arch ->
    let (es', ds) = partitionEithers eds
        cs = runDynQueryReader' f es' es arch
     in fmap Left cs ++ fmap Right ds

instance ArrowDynamicQueryReader DynamicQueryReader where
  entity = DynamicQueryReader $ \_ es _ -> es
  fetchDyn :: forall a. (Component a) => ComponentID -> DynamicQueryReader () a
  fetchDyn cId = DynamicQueryReader $ \_ _ arch ->
    maybe [] (S.toAscList @a @(StorageT a)) (A.lookupStorage @a cId arch)
  fetchMaybeDyn :: forall a. (Component a) => ComponentID -> DynamicQueryReader () (Maybe a)
  fetchMaybeDyn cId = DynamicQueryReader $ \_ es arch -> case A.lookupStorage @a cId arch of
    Just s -> let !as = S.toAscList @a @(StorageT a) s in fmap Just as
    Nothing -> map (const Nothing) es

data DynamicQueryFilter = DynamicQueryFilter
  { filterWith :: !(Set ComponentID),
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty

runDynQueryReader :: i -> DynamicQueryReader i o -> [EntityID] -> Archetype -> [o]
runDynQueryReader i q = runDynQueryReader' q (repeat i)

-- | Match all entities.
allDyn :: Set ComponentID -> i -> DynamicQueryReader i a -> Entities -> [a]
allDyn cIds i q es =
  if Set.null cIds
    then runDynQueryReader i q (Map.keys $ entities es) A.empty
    else
      let go n =
            let eIds = Set.toList $ A.entities $ AS.nodeArchetype n
             in runDynQueryReader i q eIds (AS.nodeArchetype n)
       in concatMap go (AS.find cIds $ archetypes es)
