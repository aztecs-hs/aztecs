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
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack

-- | Dynamic query reader.
--
-- @since 0.10
newtype DynamicQueryReader a = DynamicQueryReader
  { -- | Run a dynamic query reader.
    --
    -- @since 0.10
    runDynQueryReader :: Archetype -> Vector a
  }
  deriving (Functor)

-- | @since 0.10
instance Applicative DynamicQueryReader where
  pure a = DynamicQueryReader $ \arch -> V.replicate (length $ A.entities arch) a
  {-# INLINE pure #-}

  f <*> g = DynamicQueryReader $ \arch ->
    V.zipWith ($) (runDynQueryReader f arch) $ runDynQueryReader g arch
  {-# INLINE (<*>) #-}

-- | @since 0.10
instance DynamicQueryReaderF DynamicQueryReader where
  entity = DynamicQueryReader $ \arch -> V.fromList . Set.toList $ A.entities arch
  {-# INLINE entity #-}

  fetchDyn cId = DynamicQueryReader $ \arch -> A.lookupComponentsAsc cId arch
  {-# INLINE fetchDyn #-}

  fetchMaybeDyn cId = DynamicQueryReader $ \arch -> case A.lookupComponentsAscMaybe cId arch of
    Just as -> V.map Just as
    Nothing -> V.replicate (length $ A.entities arch) Nothing
  {-# INLINE fetchMaybeDyn #-}

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
allDyn :: Set ComponentID -> DynamicQueryReader a -> Entities -> Vector a
allDyn cIds q es =
  if Set.null cIds
    then runDynQueryReader q A.empty {A.entities = Map.keysSet $ entities es}
    else
      let go n = runDynQueryReader q $ AS.nodeArchetype n
       in V.concat . fmap go . Map.elems $ AS.find cIds $ archetypes es

-- | Match all entities with a filter.
--
-- @since 0.10
filterDyn :: Set ComponentID -> (Node -> Bool) -> DynamicQueryReader a -> Entities -> Vector a
filterDyn cIds f q es =
  if Set.null cIds
    then runDynQueryReader q A.empty {A.entities = Map.keysSet $ entities es}
    else
      let go n = runDynQueryReader q $ AS.nodeArchetype n
       in V.concat . fmap go . Map.elems . Map.filter f $ AS.find cIds $ archetypes es

-- | Match a single entity.
--
-- @since 0.10
singleDyn :: (HasCallStack) => Set ComponentID -> DynamicQueryReader a -> Entities -> a
singleDyn cIds q es = case singleMaybeDyn cIds q es of
  Just a -> a
  _ -> error "singleDyn: expected a single entity"

-- | Match a single entity, or `Nothing`.
--
-- @since 0.10
singleMaybeDyn :: Set ComponentID -> DynamicQueryReader a -> Entities -> Maybe a
singleMaybeDyn cIds q es =
  if Set.null cIds
    then case Map.keys $ entities es of
      [eId] -> case runDynQueryReader q $ A.singleton eId of
        v | V.length v == 1 -> Just (V.head v)
        _ -> Nothing
      _ -> Nothing
    else case Map.elems $ AS.find cIds $ archetypes es of
      [n] -> case runDynQueryReader q $ AS.nodeArchetype n of
        v | V.length v == 1 -> Just (V.head v)
        _ -> Nothing
      _ -> Nothing
