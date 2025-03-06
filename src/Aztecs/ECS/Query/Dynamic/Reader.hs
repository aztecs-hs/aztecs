{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Aztecs.ECS.Query.Dynamic.Reader
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Query.Dynamic.Reader
  ( -- * Dynamic queries
    DynamicQueryReader (..),
    DynamicQueryReaderF (..),

    -- ** Running
    allDyn,
    filterDyn,
    singleDyn,
    singleMaybeDyn,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Dynamic.Reader.Class
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node)
import qualified Aztecs.ECS.World.Archetypes as AS
import Aztecs.ECS.World.Entities (Entities (..))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack

-- | Dynamic query reader.
--
-- @since 0.10
newtype DynamicQueryReader a = DynamicQueryReader
  { -- | Run a dynamic query reader.
    --
    -- @since 0.10
    unDynQueryReader :: (Set ComponentID, Archetype -> [a])
  }
  deriving (Functor)

-- | @since 0.10
instance Applicative DynamicQueryReader where
  {-# INLINE pure #-}
  pure a = DynamicQueryReader (mempty, \arch -> replicate (length $ A.entities arch) a)

  {-# INLINE (<*>) #-}
  f <*> g =
    let (cs, f') = unDynQueryReader f
        (cs', g') = unDynQueryReader g
     in DynamicQueryReader (cs <> cs', \arch -> zipWith ($) (f' arch) (g' arch))

-- | @since 0.10
instance DynamicQueryReaderF DynamicQueryReader where
  {-# INLINE entity #-}
  entity = DynamicQueryReader (mempty, Set.toList . A.entities)

  {-# INLINE fetchDyn #-}
  fetchDyn cId = DynamicQueryReader (Set.singleton cId, A.lookupComponentsAsc cId)

  {-# INLINE fetchMaybeDyn #-}
  fetchMaybeDyn cId =
    DynamicQueryReader
      ( Set.singleton cId,
        \arch -> case A.lookupComponentsAscMaybe cId arch of
          Just as -> fmap Just as
          Nothing -> replicate (length $ A.entities arch) Nothing
      )

-- | Dynamic query for components by ID.
--
-- @since 0.9

-- | Dynamic query filter.
--
-- @since 0.9
data DynamicQueryFilter = DynamicQueryFilter
  { -- | `ComponentID`s to include.
    --
    -- @since 0.9
    filterWith :: !(Set ComponentID),
    -- | `ComponentID`s to exclude.
    --
    -- @since 0.9
    filterWithout :: !(Set ComponentID)
  }

-- | @since 0.9
instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

-- | @since 0.9
instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty

-- | Match all entities.
--
-- @since 0.10
allDyn :: DynamicQueryReader a -> Entities -> [a]
allDyn q es =
  let (cIds, q') = unDynQueryReader q
   in if Set.null cIds
        then q' A.empty {A.entities = Map.keysSet $ entities es}
        else
          let go n = q' $ AS.nodeArchetype n
           in concatMap go (AS.find cIds $ archetypes es)

-- | Match all entities with a filter.
--
-- @since 0.10
filterDyn :: (Node -> Bool) -> DynamicQueryReader a -> Entities -> [a]
filterDyn f q es =
  let (cIds, q') = unDynQueryReader q
   in if Set.null cIds
        then q' A.empty {A.entities = Map.keysSet $ entities es}
        else
          let go n = q' $ AS.nodeArchetype n
           in concatMap go (Map.filter f $ AS.find cIds $ archetypes es)

-- | Match a single entity.
--
-- @since 0.10
singleDyn :: (HasCallStack) => DynamicQueryReader a -> Entities -> a
singleDyn q es = case singleMaybeDyn q es of
  Just a -> a
  _ -> error "singleDyn: expected a single entity"

-- | Match a single entity, or `Nothing`.
--
-- @since 0.10
singleMaybeDyn :: DynamicQueryReader a -> Entities -> Maybe a
singleMaybeDyn q es =
  let (cIds, q') = unDynQueryReader q
   in if Set.null cIds
        then case Map.keys $ entities es of
          [eId] -> case q' $ A.singleton eId of
            [a] -> Just a
            _ -> Nothing
          _ -> Nothing
        else case Map.elems $ AS.find cIds $ archetypes es of
          [n] -> case q' $ AS.nodeArchetype n of
            [a] -> Just a
            _ -> Nothing
          _ -> Nothing
