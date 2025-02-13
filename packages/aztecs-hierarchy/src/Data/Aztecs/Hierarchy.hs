{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Hierarchy where

import Control.Arrow (returnA)
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

newtype Parent = Parent {unParent :: EntityID}
  deriving (Eq, Ord, Show)

instance Component Parent

newtype ParentState = ParentState {unParentState :: EntityID}
  deriving (Show)

instance Component ParentState

newtype Children = Children {unChildren :: Set EntityID}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance Component Children

newtype ChildState = ChildState {unChildState :: Set EntityID}
  deriving (Show)

instance Component ChildState

update :: System () ()
update = proc () -> do
  parents <-
    S.all
      ( proc () -> do
          entity <- Q.entity -< ()
          Parent parent <- Q.fetch -< ()
          maybeParentState <- Q.fetchMaybe @_ @ParentState -< ()
          returnA -< (entity, parent, maybeParentState)
      )
      -<
        ()
  children <-
    S.all
      ( proc () -> do
          entity <- Q.entity -< ()
          Children cs <- Q.fetch -< ()
          maybeChildState <- Q.fetchMaybe @_ @ChildState -< ()
          returnA -< (entity, cs, maybeChildState)
      )
      -<
        ()
  S.queue
    ( \(parents, childRes) -> do
        mapM_
          ( \(entity, parent, maybeParentState) -> case maybeParentState of
              Just (ParentState parentState) -> do
                if parent /= parentState
                  then do
                    A.insert parent $ ParentState parent

                    -- Remove this entity from the previous parent's children.
                    maybeLastChildren <- A.lookup parentState
                    let lastChildren = fromMaybe mempty $ unChildren <$> maybeLastChildren
                    let lastChildren' = Set.filter (/= entity) lastChildren
                    A.insert parentState . Children $ lastChildren'

                    -- Add this entity to the new parent's children.
                    maybeChildren <- A.lookup parent
                    let parentChildren = fromMaybe mempty $ unChildren <$> maybeChildren
                    A.insert parent . Children $ Set.insert entity parentChildren
                  else return ()
              Nothing -> do
                A.spawn_ . bundle $ ParentState parent
                maybeChildren <- A.lookup parent
                let parentChildren = fromMaybe mempty $ unChildren <$> maybeChildren
                A.insert parent . Children $ Set.insert entity parentChildren
          )
          parents
        mapM_
          ( \(entity, children, maybeChildState) -> case maybeChildState of
              Just (ChildState childState) -> do
                if children /= childState
                  then do
                    A.insert entity $ ChildState children
                    let added = Set.difference children childState
                        removed = Set.difference childState children
                    mapM_ (\e -> A.insert e . Parent $ entity) added
                  else return ()
              Nothing -> do
                A.insert entity $ ChildState children
                mapM_ (\e -> A.insert e . Parent $ entity) children
          )
          childRes
    )
    -<
      (parents, children)
