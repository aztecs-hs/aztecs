{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World.Archetypes
  ( Archetype (..),
    ArchetypeComponent (..),
    ArchetypeId (..),
    Archetypes (..),
    newArchetypes,
    archetype,
    insertArchetype,
    getArchetype,
    insert,
  )
where

import Data.Aztecs.Core
import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World.Components (Component, Components, getRow)
import qualified Data.Aztecs.World.Components as C
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Prelude hiding (read)

data ArchetypeComponent where
  ArchetypeComponent :: (Component c) => Proxy c -> ArchetypeComponent

instance Eq ArchetypeComponent where
  ArchetypeComponent a == ArchetypeComponent b = typeOf a == typeOf b

instance Ord ArchetypeComponent where
  ArchetypeComponent a `compare` ArchetypeComponent b = typeOf a `compare` typeOf b

instance Show ArchetypeComponent where
  show (ArchetypeComponent p) = show (typeOf p)

newtype Archetype = Archetype (Set ArchetypeComponent)
  deriving (Eq, Ord, Show, Monoid, Semigroup)

archetype :: forall c. (Component c) => Archetype
archetype = Archetype . Set.singleton $ ArchetypeComponent (Proxy @c)

newtype ArchetypeId = ArchetypeId Int deriving (Show)

data Archetypes
  = Archetypes
      (IntMap (Archetype, [Entity]))
      (Map TypeRep [ArchetypeId])
      (Map Archetype ArchetypeId)
      Int
  deriving (Show)

newArchetypes :: Archetypes
newArchetypes = Archetypes IntMap.empty Map.empty Map.empty 0

insertArchetype :: Archetype -> Components -> Archetypes -> (ArchetypeId, Archetypes)
insertArchetype (Archetype a) w (Archetypes es ids as i) = case Map.lookup (Archetype a) as of
  Just (ArchetypeId i') -> (ArchetypeId i, Archetypes es ids as i')
  Nothing ->
    let (es', ids') =
          foldr
            ( \(ArchetypeComponent p) (eAcc, acc) ->
                let cs = fromMaybe [] $ fmap (\s -> S.toList s) (getRow p w)
                    eAcc' = map (\(EntityComponent e _) -> e) cs
                 in (eAcc' ++ eAcc, Map.unionWith (<>) (Map.singleton (typeOf p) [ArchetypeId i]) acc)
            )
            ([], ids)
            (Set.toList a)
     in (ArchetypeId i, Archetypes (IntMap.insert i (Archetype a, es') es) ids' as (i + 1))

getArchetype :: ArchetypeId -> Archetypes -> [Entity]
getArchetype (ArchetypeId i) (Archetypes es _ _ _) = fromMaybe [] . fmap snd $ IntMap.lookup i es

insert :: forall c. (Component c) => Entity -> Components -> Archetypes -> Archetypes
insert e cs (Archetypes es ids as j) = case Map.lookup (typeOf (Proxy @c)) ids of
  Just (ids') ->
    let f a = case a of
          Just ((Archetype acs), esAcc) ->
            let isMatch =
                  all
                    (\(ArchetypeComponent p) -> isJust $ C.getRow p cs)
                    (Set.toList acs)
             in if isMatch
                  then Just (Archetype acs, e : esAcc)
                  else Just (Archetype acs, esAcc)
          Nothing -> Nothing
        es' = foldr (\(ArchetypeId i) acc -> IntMap.alter f i acc) es ids'
     in Archetypes es' ids as j
  Nothing -> Archetypes es ids as j
