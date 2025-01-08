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
    getArchetypeComponents,
    ArchetypeState (..),
    ArchetypeId (..),
    Archetypes (..),
    empty,
    archetype,
    archetype',
    insertArchetype,
    getArchetype,
    insert,
  )
where

import Data.Aztecs.Component (Component (..))
import Data.Aztecs.Core
import Data.Aztecs.Storage (ComponentStorage (..))
import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World.Components (ComponentID, ComponentRef, Components, insertComponent, lookupComponent)
import qualified Data.Aztecs.World.Components as C
import qualified Data.Aztecs.World.Components as CS
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
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
  ArchetypeComponent a _ == ArchetypeComponent b _ = a == b

instance Ord ArchetypeComponent where
  ArchetypeComponent a _ `compare` ArchetypeComponent b _ = a `compare` b

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
  deriving (Show, Semigroup, Monoid)

getArchetypeComponent :: forall c. (Component c) => Entity -> ComponentID -> ArchetypeComponents -> Maybe (ComponentRef c)
getArchetypeComponent e i (ArchetypeComponents m) = do
  d <- Map.lookup i m
  s <- fromDynamic @(ComponentStorage c (ComponentRef c)) d
  lookupComponent e s

getArchetypeComponents :: forall c. (Component c) => ComponentID -> ArchetypeComponents -> [(Entity, ComponentRef c)]
getArchetypeComponents i (ArchetypeComponents m) =
  let res = do
        d <- Map.lookup i m
        s <- fromDynamic @(ComponentStorage c (ComponentRef c)) d
        return $ toList' s
   in fromMaybe [] res

insertArchetypeComponent :: forall c. (Component c) => Entity -> ComponentID -> ComponentRef c -> ArchetypeComponents -> ArchetypeComponents
insertArchetypeComponent e i r (ArchetypeComponents cs) =
  let s = fromMaybe storage (lookupStorage @c @(ComponentRef c) i (ArchetypeComponents cs))
      (_, s') = insertComponent e r s
   in ArchetypeComponents $ Map.insert i (toDyn s') cs

lookupStorage :: forall a c. (Typeable a, Typeable c) => ComponentID -> ArchetypeComponents -> Maybe (ComponentStorage a c)
lookupStorage cId (ArchetypeComponents cs) = case Map.lookup cId cs of
  Just d -> fromDynamic d
  Nothing -> Nothing

insertArchetypeComponentProxy :: (Component c) => Proxy c -> Entity -> ComponentID -> ComponentRef c -> ArchetypeComponents -> ArchetypeComponents
insertArchetypeComponentProxy _ = insertArchetypeComponent

data ArchetypeState = ArchetypeState Archetype ArchetypeComponents [ArchetypeId]
  deriving (Show)

data Archetypes
  = Archetypes
      (IntMap ArchetypeState)
      (Map ComponentID [ArchetypeId])
      (Map Archetype ArchetypeId)
      Int
  deriving (Show)

empty :: Archetypes
empty = Archetypes IntMap.empty Map.empty Map.empty 0

insertArchetype :: Archetype -> Components -> Archetypes -> (ArchetypeId, Archetypes)
insertArchetype (Archetype a) w (Archetypes es ids as i) = case Map.lookup (Archetype a) as of
  Just (ArchetypeId i') -> (ArchetypeId i, Archetypes es ids as i')
  Nothing ->
    let (es', ids') =
          foldr
            ( \(ArchetypeComponent cId p) (eAcc, acc) ->
                let cs = fromMaybe [] (fmap (\s -> toList s) (lookupStorageProxy p w))
                    eAcc' =
                      map
                        ( \(e, f) ->
                            let r = (C.componentRef f cId)
                             in insertArchetypeComponentProxy p e cId r (ArchetypeComponents mempty)
                        )
                        cs
                 in (eAcc' ++ eAcc, Map.unionWith (<>) (Map.singleton cId [ArchetypeId i]) acc)
            )
            ([], ids)
            (Set.toList a)
     in (ArchetypeId i, Archetypes (IntMap.insert i (ArchetypeState (Archetype a) (mconcat es') []) es) ids' as (i + 1))

toList :: forall a c. (Typeable a, Component c) => ComponentStorage a c -> [(Entity, (Dynamic -> c))]
toList (ComponentStorage s) =
  let f (a, g) =
        ( a,
          ( \d ->
              case fromMaybe (error "TODO") (fromDynamic @(ComponentStorage a c) d) of
                (ComponentStorage s') -> g (fromMaybe (error "TODO") $ fromDynamic $ toDyn s')
          )
        )
   in map f (S.toList s)

toList' :: forall a c. (Typeable a, Typeable c) => ComponentStorage a c -> [(Entity, c)]
toList' (ComponentStorage s) = S.toList' s

getArchetype :: ArchetypeId -> Archetypes -> Maybe ArchetypeState
getArchetype (ArchetypeId i) (Archetypes es _ _ _) = IntMap.lookup i es

lookupStorageProxy :: (Component c) => Proxy c -> Components -> Maybe (ComponentStorage c c)
lookupStorageProxy _ = CS.lookupStorage

insert :: (Component c) => Entity -> ComponentID -> ComponentRef c -> Components -> Archetypes -> Archetypes
insert e i c cs (Archetypes es ids as j) = case Map.lookup i ids of
  Just (ids') ->
    let insertInArchetype :: Int -> IntMap ArchetypeState -> IntMap ArchetypeState
        insertInArchetype archetypeId acc =
          IntMap.alter (updateArchetypeState archetypeId) archetypeId acc

        updateArchetypeState :: Int -> Maybe ArchetypeState -> Maybe ArchetypeState
        updateArchetypeState _ state = case state of
          Just (ArchetypeState arch esAcc deps) ->
            let isMatch =
                  all
                    (\(ArchetypeComponent _ p) -> isJust $ lookupStorageProxy p cs)
                    (Set.toList $ unwrapArchetype arch)
             in if isMatch
                  then
                    Just $ ArchetypeState arch (insertArchetypeComponent e i c esAcc) deps
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