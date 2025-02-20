{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.Hierarchy
  ( Parent (..),
    Children (..),
    update,
    Hierarchy (..),
    hierarchy,
    hierarchies,
    ParentState (..),
    ChildState (..),
  )
where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Arrow (returnA)
import Control.DeepSeq
import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

newtype Parent = Parent {unParent :: EntityID}
  deriving (Eq, Ord, Show, Generic, NFData)

instance Component Parent

newtype ParentState = ParentState {unParentState :: EntityID}
  deriving (Show, Generic, NFData)

instance Component ParentState

newtype Children = Children {unChildren :: Set EntityID}
  deriving (Eq, Ord, Show, Semigroup, Monoid, Generic, NFData)

instance Component Children

newtype ChildState = ChildState {unChildState :: Set EntityID}
  deriving (Show, Generic, NFData)

instance Component ChildState

update ::
  (ArrowQueryReader qr, ArrowReaderSystem qr arr, ArrowQueueSystem b m arr) =>
  arr () ()
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
                when (parent /= parentState) $ do
                  A.insert parent $ ParentState parent

                  -- Remove this entity from the previous parent's children.
                  maybeLastChildren <- A.lookup parentState
                  let lastChildren = maybe mempty unChildren maybeLastChildren
                  let lastChildren' = Set.filter (/= entity) lastChildren
                  A.insert parentState . Children $ lastChildren'

                  -- Add this entity to the new parent's children.
                  maybeChildren <- A.lookup parent
                  let parentChildren = maybe mempty unChildren maybeChildren
                  A.insert parent . Children $ Set.insert entity parentChildren
              Nothing -> do
                A.spawn_ . bundle $ ParentState parent
                maybeChildren <- A.lookup parent
                let parentChildren = maybe mempty unChildren maybeChildren
                A.insert parent . Children $ Set.insert entity parentChildren
          )
          parents
        mapM_
          ( \(entity, children, maybeChildState) -> case maybeChildState of
              Just (ChildState childState) -> do
                when (children /= childState) $ do
                  A.insert entity $ ChildState children
                  let added = Set.difference children childState
                      removed = Set.difference childState children
                  mapM_ (\e -> A.insert e . Parent $ entity) added
                  mapM_ (A.remove @_ @_ @Parent) removed
              Nothing -> do
                A.insert entity $ ChildState children
                mapM_ (\e -> A.insert e . Parent $ entity) children
          )
          childRes
    )
    -<
      (parents, children)

data Hierarchy a = Node
  { nodeEntityId :: EntityID,
    nodeEntity :: a,
    nodeChildren :: [Hierarchy a]
  }

hierarchy ::
  (ArrowQueryReader q, ArrowReaderSystem q arr) =>
  EntityID ->
  q i a ->
  arr i [Hierarchy a]
hierarchy e q = proc i -> do
  children <-
    S.all
      ( proc i -> do
          entity <- Q.entity -< ()
          Children cs <- Q.fetch -< ()
          a <- q -< i
          returnA -< (entity, (cs, a))
      )
      -<
        i
  let childMap = Map.fromList children
  returnA -< hierarchy' e childMap

-- | Build all hierarchies of parents to children with the given query.
hierarchies ::
  (ArrowQueryReader q, ArrowReaderSystem q arr) =>
  q i a ->
  arr i [[Hierarchy a]]
hierarchies q = proc i -> do
  children <-
    S.all
      ( proc i -> do
          entity <- Q.entity -< ()
          Children cs <- Q.fetch -< ()
          a <- q -< i
          returnA -< (entity, (cs, a))
      )
      -<
        i
  let childMap = Map.fromList children
  roots <- S.filter Q.entity $ with @Children <> without @Parent -< ()
  returnA -< map (`hierarchy'` childMap) roots

hierarchy' :: EntityID -> Map EntityID (Set EntityID, a) -> [Hierarchy a]
hierarchy' e childMap = case Map.lookup e childMap of
  Just (cs, a) ->
    let bs = concatMap (`hierarchy'` childMap) (Set.toList cs)
     in [ Node
            { nodeEntityId = e,
              nodeEntity = a,
              nodeChildren = bs
            }
        ]
  Nothing -> []
