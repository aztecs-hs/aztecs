{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World.Archetypes
  ( Archetype (..),
    ArchetypeComponent (..),
    ArchetypeComponents (..),
    getArchetypeComponent,
    ArchetypeState (..),
    ArchetypeId (..),
    Archetypes (..),
    newArchetypes,
    archetype,
    archetype',
    insertArchetype,
    getArchetype,
    insert,
  )
where

import Data.Aztecs.Core
import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World.Components (Component, ComponentID, Components, getRow)
import qualified Data.Aztecs.World.Components as C
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Foldable (foldrM)
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
  ArchetypeComponent :: (Component c) => ComponentID -> Proxy c -> ArchetypeComponent

instance Eq ArchetypeComponent where
  ArchetypeComponent a _ == ArchetypeComponent b _ = typeOf a == typeOf b

instance Ord ArchetypeComponent where
  ArchetypeComponent a _ `compare` ArchetypeComponent b _ = typeOf a `compare` typeOf b

instance Show ArchetypeComponent where
  show (ArchetypeComponent p _) = show (typeOf p)

newtype Archetype = Archetype (Set ArchetypeComponent)
  deriving (Eq, Ord, Show, Monoid, Semigroup)

archetype :: forall c. (Component c) => ComponentID -> Archetype
archetype cId = archetype' (Proxy @c) cId

archetype' :: (Component c) => Proxy c -> ComponentID -> Archetype
archetype' p cId = Archetype . Set.singleton $ ArchetypeComponent cId p

newtype ArchetypeId = ArchetypeId Int deriving (Eq, Ord, Show)

newtype ArchetypeComponents = ArchetypeComponents (Map ComponentID Dynamic)
  deriving (Show)

getArchetypeComponent :: (Component c) => ComponentID -> ArchetypeComponents -> Maybe (IO c, c -> IO ())
getArchetypeComponent i (ArchetypeComponents m) = do
  d <- Map.lookup i m
  fromDynamic d

insertArchetypeComponent :: (Component c) => ComponentID -> IO c -> (c -> IO ()) -> ArchetypeComponents -> ArchetypeComponents
insertArchetypeComponent i c f (ArchetypeComponents m) = ArchetypeComponents $ Map.insert i (toDyn (c, f)) m

data ArchetypeState = ArchetypeState Archetype (Map Entity ArchetypeComponents) [ArchetypeId]
  deriving (Show)

data Archetypes
  = Archetypes
      (IntMap ArchetypeState)
      (Map ComponentID [ArchetypeId])
      (Map Archetype ArchetypeId)
      Int
  deriving (Show)

newArchetypes :: Archetypes
newArchetypes = Archetypes IntMap.empty Map.empty Map.empty 0

insertArchetype :: Archetype -> Components -> Archetypes -> IO (ArchetypeId, Archetypes)
insertArchetype (Archetype a) w (Archetypes es ids as i) = case Map.lookup (Archetype a) as of
  Just (ArchetypeId i') -> return (ArchetypeId i, Archetypes es ids as i')
  Nothing -> do
    (es', ids') <-
      foldrM
        ( \(ArchetypeComponent cId p) (eAcc, acc) -> do
            cs <- fromMaybe (pure []) $ fmap (\s -> S.toList' s) (getRow p w)
            let eAcc' = map (\(EntityComponent e c, f) -> (e, insertArchetypeComponent cId c f (ArchetypeComponents mempty))) cs
            return (eAcc' ++ eAcc, Map.unionWith (<>) (Map.singleton cId [ArchetypeId i]) acc)
        )
        ([], ids)
        (Set.toList a)
    return (ArchetypeId i, Archetypes (IntMap.insert i (ArchetypeState (Archetype a) (Map.fromList es') []) es) ids' as (i + 1))

getArchetype :: ArchetypeId -> Archetypes -> Maybe ArchetypeState
getArchetype (ArchetypeId i) (Archetypes es _ _ _) = IntMap.lookup i es

insert :: (Component c) => ComponentID -> Entity -> Components -> Archetypes -> Archetypes
insert i e cs (Archetypes es ids as j) = case Map.lookup i ids of
  Just (ids') ->
    let insertInArchetype :: Int -> IntMap ArchetypeState -> IntMap ArchetypeState
        insertInArchetype archetypeId acc =
          IntMap.alter (updateArchetypeState archetypeId) archetypeId acc

        updateArchetypeState :: Int -> Maybe ArchetypeState -> Maybe ArchetypeState
        updateArchetypeState _ state = case state of
          Just (ArchetypeState arch esAcc deps) ->
            let isMatch =
                  all
                    (\(ArchetypeComponent _ p) -> isJust $ C.getRow p cs)
                    (Set.toList $ unwrapArchetype arch)
             in if isMatch
                  then
                    Just $ ArchetypeState arch (Map.singleton e (ArchetypeComponents mempty) <> esAcc) deps
                  else state
          Nothing -> state

        updateDependencies :: Int -> IntMap ArchetypeState -> IntMap ArchetypeState
        updateDependencies archetypeId acc = case IntMap.lookup archetypeId acc of
          Just (ArchetypeState _ _ deps) -> foldr updateDependencies (insertInArchetype archetypeId acc) (map getArchetypeId deps)
          Nothing -> acc
        es' = foldr updateDependencies es (map getArchetypeId ids')
     in merge $ Archetypes es' ids as j
  Nothing -> Archetypes es ids as j

merge :: Archetypes -> Archetypes
merge archetypes@(Archetypes es _ _ _) =
  foldl processArchetype archetypes (IntMap.toList es)
  where
    processArchetype :: Archetypes -> (Int, ArchetypeState) -> Archetypes
    processArchetype acc (parentId, ArchetypeState parentArch _ _) =
      foldl (updateDependency parentId parentArch) acc (IntMap.toList es)

    updateDependency :: Int -> Archetype -> Archetypes -> (Int, ArchetypeState) -> Archetypes
    updateDependency parentId parentArch acc (childId, ArchetypeState childArch _ _) =
      let parentComponents = unwrapArchetype parentArch
          childComponents = unwrapArchetype childArch
       in if childId /= parentId && Set.isSubsetOf childComponents parentComponents
            then mergeWithDeps (ArchetypeId parentId) (ArchetypeId childId) acc
            else acc

mergeWithDeps :: ArchetypeId -> ArchetypeId -> Archetypes -> Archetypes
mergeWithDeps parentId childId (Archetypes es ids as nextId) =
  case (IntMap.lookup (getArchetypeId parentId) es, IntMap.lookup (getArchetypeId childId) es) of
    (Just (ArchetypeState parentArch parentEntities parentDeps), Just (ArchetypeState childArch childEntities childDeps)) ->
      let parentComponents = unwrapArchetype parentArch
          childComponents = unwrapArchetype childArch

          adjustedChildComponents = Set.intersection parentComponents childComponents
          adjustedChildArch = Archetype adjustedChildComponents

          (childId', updatedEs, updatedAs, newNextId) =
            if adjustedChildComponents == childComponents
              then (childId, es, as, nextId)
              else case Map.lookup adjustedChildArch as of
                Just existingId -> (existingId, es, as, nextId)
                Nothing ->
                  let newChildId = ArchetypeId nextId
                      newState = ArchetypeState adjustedChildArch childEntities childDeps
                   in ( newChildId,
                        IntMap.insert nextId newState es,
                        Map.insert adjustedChildArch newChildId as,
                        nextId + 1
                      )

          updatedParentDeps = childId' : parentDeps
          updatedParentState = ArchetypeState parentArch parentEntities updatedParentDeps
          finalEs = IntMap.insert (getArchetypeId parentId) updatedParentState updatedEs
       in Archetypes finalEs ids updatedAs newNextId
    _ -> Archetypes es ids as nextId

getArchetypeId :: ArchetypeId -> Int
getArchetypeId (ArchetypeId x) = x

unwrapArchetype :: Archetype -> Set ArchetypeComponent
unwrapArchetype (Archetype set) = set
