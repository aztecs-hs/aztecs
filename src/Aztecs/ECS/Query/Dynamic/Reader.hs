{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Aztecs.ECS.Query.Dynamic.Reader
  ( -- * Dynamic queries
    DynamicQueryReader (..),
    ArrowDynamicQueryReader (..),

    -- ** Running
    allDyn,
    singleDyn,
    singleMaybeDyn,
    runDynQueryReader,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.Query.Dynamic.Reader.Class
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

-- | Dynamic query for components by ID.
newtype DynamicQueryReader i o
  = DynamicQueryReader {runDynQueryReader' :: [i] -> [EntityID] -> Archetype -> [o]}
  deriving (Functor)

instance Applicative (DynamicQueryReader i) where
  {-# INLINE pure #-}
  pure a = DynamicQueryReader $ \_ es _ -> replicate (length es) a
  {-# INLINE (<*>) #-}
  f <*> g =
    DynamicQueryReader $ \i es arch ->
      let !as = runDynQueryReader' g i es arch
          !fs = runDynQueryReader' f i es arch
       in zipWith ($) fs as

instance Category DynamicQueryReader where
  {-# INLINE id #-}
  id = DynamicQueryReader $ \as _ _ -> as
  {-# INLINE (.) #-}
  f . g = DynamicQueryReader $ \i es arch ->
    let !as = runDynQueryReader' g i es arch in runDynQueryReader' f as es arch

instance Arrow DynamicQueryReader where
  {-# INLINE arr #-}
  arr f = DynamicQueryReader $ \bs _ _ -> fmap f bs
  {-# INLINE first #-}
  first f = DynamicQueryReader $ \bds es arch ->
    let !(bs, ds) = unzip bds
        !cs = runDynQueryReader' f bs es arch
     in zip cs ds

instance ArrowChoice DynamicQueryReader where
  {-# INLINE left #-}
  left f = DynamicQueryReader $ \eds es arch ->
    let !(es', ds) = partitionEithers eds
        !cs = runDynQueryReader' f es' es arch
     in fmap Left cs ++ fmap Right ds

instance ArrowDynamicQueryReader DynamicQueryReader where
  {-# INLINE entity #-}
  entity = DynamicQueryReader $ \_ es _ -> es
  {-# INLINE fetchDyn #-}
  fetchDyn cId = DynamicQueryReader $ \_ _ arch -> A.lookupComponentsAsc cId arch
  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn cId = DynamicQueryReader $ \_ es arch -> case A.lookupComponentsAscMaybe cId arch of
    Just as -> fmap Just as
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

{-# INLINE runDynQueryReader #-}
runDynQueryReader :: i -> DynamicQueryReader i o -> [EntityID] -> Archetype -> [o]
runDynQueryReader i q = runDynQueryReader' q (repeat i)

-- | Match all entities.
allDyn :: Set ComponentID -> i -> DynamicQueryReader i a -> Entities -> [a]
allDyn cIds i q es =
  if Set.null cIds
    then runDynQueryReader i q (Map.keys $ entities es) A.empty
    else
      let go n =
            let !eIds = Set.toList $ A.entities $ AS.nodeArchetype n
             in runDynQueryReader i q eIds (AS.nodeArchetype n)
       in concatMap go (AS.find cIds $ archetypes es)

singleDyn :: (HasCallStack) => Set ComponentID -> i -> DynamicQueryReader i a -> Entities -> a
singleDyn cIds i q es = case singleMaybeDyn cIds i q es of
  Just a -> a
  _ -> error "singleDyn: expected a single entity"

singleMaybeDyn :: Set ComponentID -> i -> DynamicQueryReader i a -> Entities -> Maybe a
singleMaybeDyn cIds i q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> case runDynQueryReader i q [eId] A.empty of
        [a] -> Just a
        _ -> Nothing
      _ -> Nothing
    else case Map.elems $ AS.find cIds $ archetypes es of
      [n] ->
        let !eIds = Set.toList $ A.entities $ AS.nodeArchetype n
         in case runDynQueryReader i q eIds (AS.nodeArchetype n) of
              [a] -> Just a
              _ -> Nothing
      _ -> Nothing
