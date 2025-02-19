{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Hierarchy
  ( Parent (..),
    Children (..),
    update,
    Hierarchy (..),
    hierarchies,
    ParentState (..),
    ChildState (..),
  )
where

import Control.Arrow (returnA)
import Control.Monad (when)
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.Query.Reader (QueryReader)
import Data.Aztecs.System (ArrowReaderSystem, ArrowSystem)
import qualified Data.Aztecs.System as S
import qualified Data.Map as Map
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

update :: (ArrowSystem arr) => arr () ()
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
                  -- TODO removed = Set.difference childState children
                  mapM_ (\e -> A.insert e . Parent $ entity) added
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

-- | Build all hierarchies of parents to children with the given query.
hierarchies ::
  (ArrowReaderSystem arr) =>
  QueryReader i a ->
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
  let go e = case Map.lookup e childMap of
        Just (cs, a) ->
          let bs = concatMap (go $) (Set.toList cs)
           in [ Node
                  { nodeEntityId = e,
                    nodeEntity = a,
                    nodeChildren = bs
                  }
              ]
        Nothing -> []
  returnA -< map go roots
